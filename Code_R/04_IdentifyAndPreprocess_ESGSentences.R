library(tidyverse)

exp_dict <- read_delim("../CreateDict_SplitSentence/outputs/dict/expanded_dict.csv", delim = ";")
exp_dict <- list(E = exp_dict %>% select(environmental) %>% drop_na %>% pull, S = exp_dict %>% select(social) %>%  drop_na %>% pull,G = exp_dict %>% select(governance) %>%  drop_na %>% pull)
tokens <-fst::read_fst("../Data/tokens.fst")



# Identify ESG Sentences --------------------------------------------------


results = tibble()
for(ii in 1:3){
  idx = tokens$term %in% exp_dict[[ii]]
  res <- tokens[idx,] %>% mutate(type = names(exp_dict[ii]))
  
  results <- bind_rows(results, res)
}


# Compute tf, df, idf, tf-idf -----------------------------------------------------

# number of sentences in documents
ids <- readRDS("../Data/ids.RDS")
ids <- ids %>% 
  group_by(doc_id) %>%
  mutate(n_Sentc = n()) %>%
  ungroup()

# number of documents
nDocs = length(unique(tokens$doc_id))
# tokens in documents
nToken_doc <- tokens %>% group_by(doc_id) %>% count() %>% rename(n_tokInDoc = n)
# tokens in sentences
nToken_sentence <- fst::read.fst( "../Data/nToken_sentence_trigram.fst")

#tf, idf, tf-idf
results <- results %>%
  group_by(id,term) %>% 
  mutate(tf = n() ) %>%
  ungroup() %>%
  group_by(term) %>%
  mutate(df = length(unique(doc_id))) %>%
  ungroup() %>% 
  mutate(idf = log(nDocs/df)
         , tf_idf = tf*idf) %>% 
  left_join(nToken_doc) %>% 
  left_join(nToken_sentence) %>% 
  left_join(ids)

# Add count of sentiment words from LM2011 dictionary -----------------------------


sent_dict <- read_csv("../Data/Loughran-McDonald_MasterDictionary_1993-2021.csv")

sent_lm2011 <- list(pos = sent_dict %>% filter(Positive>0) %>% pull(Word) %>% str_to_lower()
                    , neg = sent_dict %>% filter(Negative>0) %>% pull(Word) %>% str_to_lower())

results_sent = tibble()
for(ii in 1:2){
  idx = tokens$term %in% sent_lm2011[[ii]]
  res <- tokens[idx,] %>% mutate(type = names(sent_lm2011[ii]))
  
  results_sent <- bind_rows(results_sent, res)
}

results_sent <- results_sent %>% 
  group_by(id, type) %>% 
  count() %>%
  ungroup()%>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0)

results <- results %>% left_join(results_sent) %>% mutate(pos = if_else(is.na(pos), 0L, pos)
                                                          ,neg= if_else(is.na(neg), 0L, neg)
                                                          ,sent_scr =(pos-neg)/n_tokInSentc ) %>% 
  select(-pos, -neg)
# save
saveRDS(results,"../Data/locations_ESG_words.RDS")

rm(list = ls())
gc()
