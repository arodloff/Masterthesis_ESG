library(tidyverse)

# Load input sentences of FinBERT and merge with their IDs ----------------------------------------------

ids <- read_table("../Data/ESG_sentences_ids.txt")
raw_sentences <- read_table("../FinBERT/ESG_sentences_raw.txt", col_names = F) 
ids <- bind_cols(ids, raw_sentences) %>% rename(id = 1, sentence = 2)

# find unique sentences conaining ESG words
raw_sentences <- raw_sentences%>% rename(sentence = 1) %>% distinct(sentence)

# Load FinBERT predictions, identified by sentence ------------------------

sent_pred <- read_csv("../FinBERT/output/predictions.csv")

# Remove duplicate sentences (sentences containing more than one ESG word were evaluated multiple times )
sent_pred <- sent_pred %>% distinct(sentence, .keep_all = T)


# Match FinBERT predicitons with id ---------------------------------------

# FinBERT sometimes classified two sentences as one
# Since we match the FinBERT predcition and id on the sentence,
# these sentences need to be identified and split

# Split sentences which FinBERT evaluated as one
mismatch <- anti_join(sent_pred, raw_sentences)%>% #identify mismatches
  separate_rows(sentence, sep = "\\.") %>%
  filter(sentence != "") %>%
  mutate(sentence = str_remove_all(sentence, "\r\n |\r|\n")
         #, sentence = str_remove_all(sentence,"\n")
         , sentence = paste0(sentence,".")
         , sentence = str_replace_all(sentence, pattern = "\\?\\.|\\?\\s\\.", "\\?") ) %>% 
  distinct(sentence, .keep_all = T)

# Get unique sentence predicitons which can be matched with IDs on sentences
sent_pred <- sent_pred %>%
  inner_join(raw_sentences) %>% # select sentences that match
  bind_rows(mismatch) %>% # add split sentences which now also match
  distinct(sentence, .keep_all = T)

# Join with ids by sentence
df_join <- ids %>%
  left_join(sent_pred)


# Add FinBERT predictions to table ----------------------------------------

results <- readRDS("../Data/locations_ESG_words.RDS")

results <- results %>% left_join(df_join %>% select(id, sent_finbert = sentiment_score))

# sanity check

results %>% filter(is.na(sent_finbert))

# save to create scores
saveRDS(results,"../Data/locations_ESG_words_final.RDS")
rm(list = ls())
gc()

