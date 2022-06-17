#####
# This script summarizes tasks that take a lot of time/RAM but makes later things easier later on
####

library(tidyverse)
library(tidytext)



# Create df of tokens and their position ------------------------------

ids <- read_table("../CreateDict_SplitSentence/data/processed/parsed/document_sent_ids.txt", col_names = F) %>% rename(id = 1)
doc <- read_table("../CreateDict_SplitSentence/data/processed/trigram/documents.txt", col_names = F,skip_empty_rows = F) %>% rename(doc_id = 1)
df <- bind_cols(doc,ids)

#need this later
ids <- ids %>% separate(col = id, into = c("doc_id", "doc_sentence"), sep = "_", remove = F)
saveRDS(ids, "../Data/ids.RDS")
# move up space
rm(doc,ids)
gc()
tokens <- df %>% group_by(id) %>% unnest_tokens(term, doc_id, token = "regex",  pattern = "\\s") %>% ungroup()
tokens <- tokens %>% mutate(doc_id = gsub("\\_.*","",id, fixed = F))

ids <- read_table("../CreateDict_SplitSentence/data/processed/parsed/document_sent_ids.txt", col_names = F) %>% rename(id = 1)
ids <- ids %>% mutate(i = row_number())
tokens <- tokens %>% left_join(ids)

fst::write_fst(tokens, "../Data/tokens.fst")
#tokens <- fst::read_fst("token_raw.fst")
rm(ids)
gc()



# Count words in sentences ------------------------------------------------

nToken_sentence <- tokens %>% group_by(id) %>% count() %>% rename(n_tokInSentc = n)
fst::write.fst(nToken_sentence, path = "../Data/nToken_sentence_trigram.fst")
rm(df, tokens, nToken_sentence)
gc()

# better restart R after this as gc() is flawed
