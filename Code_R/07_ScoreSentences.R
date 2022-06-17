library(tidyverse)

# Load Data ---------------------------------------------------------------

doc_ids <- read_table("../CreateDict_SplitSentence/data/input/document_ids.txt", col_names = F) %>% rename(doc_id = 1)
results <- readRDS("../Data/locations_ESG_words_final.RDS")

# Filter duplicates (multiple instances of the same word in a sentence)
results <- results %>%
  group_by(id, term) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Sentiment with FinBERT --------------------------------------------------


sentence_score <- results %>% 
  group_by(id, type) %>% 
  summarise(sentc_score = sum(tf)) %>%
  left_join(results %>% select(id, doc_id, doc_sentence, starts_with("n"), starts_with("sent"))) %>%
  group_by(id, type) %>% filter(row_number()==1) %>% 
  mutate(sentc_score_sentim = sentc_score*sent_finbert)

doc_score <-  sentence_score %>% 
  group_by(type, doc_id) %>% 
  summarise(sentc_score_sentim = sum(sentc_score_sentim)
            ,  n_Sentc) %>% 
  filter(row_number() == 1) %>%
  ungroup()%>%
  mutate(sentc_score_sentim = sentc_score_sentim/n_Sentc*100) %>% select(-n_Sentc)

# TF Score ----------------------------------------------------------------


tf_score <-  results %>% group_by(doc_id, type) %>% 
  summarise(tf = sum(tf), n_tokInDoc) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  mutate(tf_score = tf/n_tokInDoc*100) %>% select(-tf,-n_tokInDoc)


# TF-IDF ------------------------------------------------------------------


tf_idf_score <- results %>%
  group_by(doc_id, type) %>%
  summarise(tf_idf = sum(tf_idf), n_tokInDoc)%>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(tf_idf_score = tf_idf/n_tokInDoc*100) %>% select(-n_tokInDoc, -tf_idf)

# Join-------------------------------------------------------------------------

scores_join <- doc_score %>%
  inner_join(tf_score) %>%
  inner_join(tf_idf_score) %>%
  mutate(doc_id = as.numeric(doc_id)) %>% 
  rename(sentim_TF_score = sentc_score_sentim)

# Join with docs not containing ESG words

scores_full <- doc_ids %>% mutate(E = NA, S = NA, G = NA) %>%
  pivot_longer(cols = c(E,S,G), names_to = "type") %>%
  select(-value) %>% 
  left_join(scores_join) %>% 
  mutate(across(ends_with("score"), ~ifelse(is.na(.), 0,.)    ))

# Sentiment LM 2011 -------------------------------------------------------
sentence_score2 <- results %>% 
  group_by(id, type) %>% 
  summarise(sentc_score = sum(tf)) %>%
  left_join(results %>% select(id, doc_id, doc_sentence, starts_with("n"), starts_with("sent"))) %>%
  group_by(id, type) %>% filter(row_number()==1) %>% 
  mutate(sentc_score_sentim_LM = sentc_score*sent_scr)

doc_score2<-  sentence_score2 %>% 
  group_by(type, doc_id) %>% 
  summarise(score_sentim_LM = sum(sentc_score_sentim_LM)
            ,  n_Sentc) %>% 
  filter(row_number() == 1) %>%
  ungroup()%>%
  mutate(score_sentim_LM =  score_sentim_LM/n_Sentc*100) %>% select(-n_Sentc)


# Join rest with LM 2011 score --------------------------------------------
scores <- scores_full %>%
  left_join(doc_score2 %>%
  mutate(doc_id = as.numeric(doc_id))) %>% 
  mutate(score_sentim_LM = if_else(is.na(score_sentim_LM),0,score_sentim_LM)) %>% 
  rename(sentim_TF_LM_score = score_sentim_LM)




# Same procedure with tf-idf as base for sentiment scores -----------------


# Sentiment with FinBERT --------------------------------------------------


sentence_score <- results %>% 
  group_by(id, type) %>% 
  summarise(sentc_score = sum(tf_idf)) %>%
  left_join(results %>% select(id, doc_id, doc_sentence, starts_with("n"), starts_with("sent"))) %>%
  group_by(id, type) %>% filter(row_number()==1) %>% 
  mutate(sentc_score_sentim = sentc_score*sent_finbert)

doc_score <-  sentence_score %>% 
  group_by(type, doc_id) %>% 
  summarise(sentc_score_sentim = sum(sentc_score_sentim)
            ,  n_Sentc) %>% 
  filter(row_number() == 1) %>%
  ungroup()%>%
  mutate(sentc_score_sentim = sentc_score_sentim/n_Sentc*100) %>% select(-n_Sentc)



# TF Score ----------------------------------------------------------------


tf_score <-  results %>% group_by(doc_id, type) %>% 
  summarise(tf = sum(tf), n_tokInDoc) %>% 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  mutate(tf_score = tf/n_tokInDoc*100) %>% select(-tf,-n_tokInDoc)


# TF-IDF ------------------------------------------------------------------


tf_idf_score <- results %>%
  group_by(doc_id, type) %>%
  summarise(tf_idf = sum(tf_idf), n_tokInDoc)%>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(tf_idf_score = tf_idf/n_tokInDoc*100) %>% select(-n_tokInDoc, -tf_idf)


# Join-------------------------------------------------------------------------



scores_join <- doc_score %>% inner_join(tf_score) %>% inner_join(tf_idf_score) %>% mutate(doc_id = as.numeric(doc_id)) %>% 
  rename(sentim_score = sentc_score_sentim)

# Join with docs not containing ESG words

scores_full <- doc_ids %>% mutate(E = NA, S = NA, G = NA) %>%
  pivot_longer(cols = c(E,S,G), names_to = "type") %>%
  select(-value) %>% 
  left_join(scores_join) %>% 
  mutate(across(ends_with("score"), ~ifelse(is.na(.), 0,.)    ))


# Sentiment LM 2011 -------------------------------------------------------
sentence_score2 <- results %>% 
  group_by(id, type) %>% 
  summarise(sentc_score = sum(tf_idf)) %>%
  left_join(results %>% select(id, doc_id, doc_sentence, starts_with("n"), starts_with("sent"))) %>%
  group_by(id, type) %>% filter(row_number()==1) %>% 
  mutate(sentc_score_sentim_LM = sentc_score*sent_scr)

doc_score2<-  sentence_score2 %>% 
  group_by(type, doc_id) %>% 
  summarise(score_sentim_LM = sum(sentc_score_sentim_LM)
            ,  n_Sentc) %>% 
  filter(row_number() == 1) %>%
  ungroup()%>%
  mutate(score_sentim_LM =  score_sentim_LM/n_Sentc*100) %>% select(-n_Sentc)


# Join rest with LM 2011 score --------------------------------------------
scores2 <- scores_full %>% 
  left_join(doc_score2 %>%
              mutate(doc_id = as.numeric(doc_id))) %>% 
  mutate(score_sentim_LM = if_else(is.na(score_sentim_LM),0,score_sentim_LM)) %>% 
  rename(sentim_LM_score = score_sentim_LM)

scores_final <- scores %>% inner_join(scores2)

saveRDS(scores_final, "../Data/scores.RDS")
rm(list = ls())
gc()
