library(tidyverse)


# Prepare for finBERT -----------------------------------------------------

results <- readRDS("../Data/locations_ESG_words.RDS")
# Get rows of ESG words
rows_ESG <- unique(results$i)

# These are the Earnings Calls which were split at the sentence level by the CoreNLP package without any further processing
ids <- read_table("../CreateDict_SplitSentence/data/processed/parsed/document_sent_ids_split.txt", col_names = F, skip_empty_rows = F)
df <- read_table("../CreateDict_SplitSentence/data/processed/parsed/documents_split.txt", col_names = F, skip_empty_rows = F)

# Subset only ESG sentences, s.t. FinBERT does not need to classify every sentence
df <- df[rows_ESG,]
ids <- ids[rows_ESG,]

# Clean for FinBERT (messy but works)
df <- df %>% mutate(X1 = str_replace_all(X1, "\\s([?.,!'](?:\\s|$))","\\1" ) 
                    , X1 = str_replace_all(X1,"\\s*(['])\\s*", "\\1")
                    , X1 = str_replace_all(X1," n't", "n't")
                    , X1 = str_remove_all(X1, "-LSB-|-RRB-|-LRB-|-RSB-|\\s*\\'\\s*")
                    , X1 = str_remove_all(X1, "\\.")
                    , X1 = str_remove_all(X1, "\\!")
                    , X1 = str_remove_all(X1, "\\``\\s+")
                    , X1 = paste0(X1,".")
                    , X1 = str_replace_all(X1, pattern = "\\?\\.|\\?\\s\\.", "\\?") )

# Save the files, which are inputs for FinBERT
write.table(df,"../FinBERT/ESG_sentences_raw.txt", col.names = F, row.names = F, quote = FALSE, fileEncoding = "UTF-8")
write.table(ids,"../Data/ESG_sentences_ids.txt", col.names = T, row.names = F, quote = FALSE, fileEncoding = "UTF-8")

rm(list = ls())
gc()
