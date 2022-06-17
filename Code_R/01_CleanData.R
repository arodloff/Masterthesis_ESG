library(RSQLite)
library(tidyverse)
library(lubridate)


# Save Q&A as RDS and Prepare for Li et al. code ------------------------


db = dbConnect(RSQLite::SQLite(), '../Data/refinitiv.db')


df <- dbGetQuery(db, "SELECT Doc_id, Speech from Turns
                                  WHERE Section_id = 5") %>% 
  group_by(Doc_id) %>% 
  summarise(Speech  = paste0(Speech,collapse =  " ")) %>% 
  ungroup() %>% 
  mutate(Speech  = str_remove_all(Speech, "\\n"))


saveRDS(df, "../Data/earningscall_qa.RDS")

# Copy these into Data/Input folder of Li et al code 
write.table(df %>% select(Doc_id), "../CreateDict_SplitSentence/data/input/document_ids.txt", row.names = F, col.names = F)
write.table(df %>% select(Speech), "../CreateDict_SplitSentence/data/input/documents.txt", row.names = F, col.names = F)


# Clean Refitiv Asset4 Scores and save as RDS -----------------------------

df_ESG <- readxl::read_xlsx("../Data/ESG_refinitiv_full.xlsx", sheet = 1,skip = 3)%>% mutate(across(.cols = c(-Name,-Code),as.numeric) 
                                                                                             #, tickerType = as.numeric(str_detect(Code, "U:"))
                                                                                             , Code = str_remove(Code, "@")
                                                                                             , Code = str_remove(Code, "U:") 
                                                                                             , Code = str_remove(Code, "\\(.*") 
) %>% rename(ticker = Code) %>% 
  #separate(col = Code, into = c("ticker", "type"), sep = "\\(" ) %>% 
  separate(col = Name, into = c("name", "type"), sep = " - ") %>% 
  pivot_longer(cols = c(-name, -type, -ticker), names_to = "year") %>%
  mutate(year = as.numeric(year))%>%
  mutate(type = case_when(type == "Environment Pillar Score" ~ "E"
                          , type == "Governance Pillar Score" ~ "G"
                          , type == "Social Pillar Score" ~ "S"
                          , type == "ESG Score" ~ "Overall"
  )  ) %>% #filter(type %in% c("E","S","G")) %>% 
  rename(refinitiv_code = ticker) %>% 
  filter(!is.na(type))




df_ESG_ticker <- readxl::read_xlsx("../Data/codes.xlsx", sheet = 1,skip = 1) 
df_ESG_ticker <- as_tibble(cbind(ticker = colnames(df_ESG_ticker), t(df_ESG_ticker)))
colnames(df_ESG_ticker) <- df_ESG_ticker %>% slice(1) %>% c()
df_ESG_ticker <- df_ESG_ticker[-1,] %>% 
  select(refinitiv_code = 1
         , ticker = 2
         , ric = 7) %>% 
  separate(ric, into = c("tic", "exch"), sep = "\\.", remove = F) %>% 
  mutate(ric = if_else(str_detect(ticker,"-US"), tic, ric)) %>%    # remove exchange suffix for US companies
  select(refinitiv_code, ticker = ric)


df_ESG <- df_ESG_ticker %>% right_join(df_ESG) %>% filter(!is.na(ticker))
saveRDS(df_ESG,"../Data/refinitiv_esg_clean.RDS")

rm(list = ls())
gc()
