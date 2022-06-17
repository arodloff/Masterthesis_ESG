
# Small script to get seed words in format to paste to general_settings in Li et al code --------



library(tidyverse)
library(readxl)


df <- read_excel("../Data/berg_draft5.xlsx", sheet = 3)%>% rename(term1 = 1, term2 = 3, use = 4) %>% filter(use >0) %>% 
  pivot_longer(cols = starts_with("term")) %>% filter(!is.na(value), !(use == 2 & name == "term1")) %>% select(-c(name,use, Comment))

df %>% count(Category)

df <- df %>%  separate_rows(value, sep = ", " ) %>% 
  mutate(value= str_to_lower(value), value = str_replace_all(value,"  ", "_"),  value = str_replace_all(value," ", "_") )
env <- df %>% filter(Category == "E") %>% pull(value)
soc <- df %>% filter(Category == "S") %>% pull(value)
gov <- df %>% filter(Category == "G") %>% pull(value)

env <- paste0(' "environmental": [\n"',paste0(env, collapse = '"\n, "'),'"  ],')
soc <- paste0(' "social": [\n"',paste0(soc, collapse = '"\n, "'),'"  ],')
gov <- paste0(' "governance": [\n"',paste0(gov, collapse = '"\n, "'),'"  ],')
cat(c(env,"\n \n",soc,"\n \n",gov))

rm(list = ls())
gc()
