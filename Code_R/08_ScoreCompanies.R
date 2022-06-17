library(tidyverse)
library(RSQLite)
library(lubridate)
library(fuzzyjoin)

df_ESG <- readRDS("../Data/refinitiv_esg_clean.RDS") #%>% # %>% group_by(ticker, type, year) %>% filter(n() == 1) %>% ungroup()

# Load an merge scores

scores_full <- readRDS("../Data/scores.RDS")

db = dbConnect(RSQLite::SQLite(), '../Data/refinitiv.db')

df_callInfo = dbGetQuery(db, "SELECT Doc_id, XML_Event_startDate, PDF_Title_Ticker, XML_Event_companyTicker, XML_Event_companyName FROM Documents") %>% 
  transmute(doc_id =Doc_id,
            ticker = PDF_Title_Ticker,
            #  ticker2 = XML_Event_companyTicker,
            companyname = XML_Event_companyName,
            Date = lubridate::dmy_hm(XML_Event_startDate)
            , quarter = quarter(Date)
            , year = year(Date)
  )





# Merge and Aggregate -----------------------------------------------------
ec_ticker <- unique(df_callInfo$ticker)
asset4_ticker <- unique(df_ESG$ticker)
intersect(ec_ticker, asset4_ticker)

temp <- df_callInfo %>%
  group_by(companyname) %>%
  mutate(n_ticker = length(unique(ticker))) %>%
  filter(n_ticker >1) %>% 
  arrange(companyname) %>% 
  mutate(ticker2 = if_else(ticker %in% asset4_ticker, ticker, NA_character_) )%>% 
  drop_na(ticker2) %>% filter(length(unique(ticker2)) == 1) %>% 
  select(ticker2, companyname) %>% distinct_all()

df_callInfo <- df_callInfo %>% left_join(temp)  %>% as_tibble() %>% mutate(ticker_og = ticker,
                                                                           ticker = if_else(is.na(ticker2), ticker, ticker2)
)


## Quarterly to yearly; match CallID with ticker, year, name

df_aggregated_scores <- scores_full %>% left_join(df_callInfo) %>% 
  group_by(ticker, year, type, companyname) %>% 
  summarise(across(ends_with("score"), ~mean(.) )) %>%
  separate(ticker, into = c("tic", "exch"), sep = "\\.", remove = F) %>% # make US stocks comparable
  mutate(ticker = if_else(str_detect(ticker, ".OQ"), tic, ticker )) %>% 
  group_by(ticker, year, type) %>% 
  filter(n() == 1) %>% #Sometimes multiple tickers per year
  ungroup() %>% 
  filter(!is.na(ticker))

# Add overall score
temp <- df_aggregated_scores %>% group_by(ticker, year, companyname) %>% 
  summarise(across(ends_with("score"), ~mean(.) )) %>% mutate(type = "Overall")

df_aggregated_scores <- df_aggregated_scores %>% bind_rows(temp) %>% select(-tic, -exch)
# Merge with Asset4 Scores ------------------------------------------------

df_merged <- df_ESG %>% select(-name)%>% right_join(df_aggregated_scores)

# Merge with industry classifier ------------------------------------------

industry_class <- read_csv("../Data/industry_classifier.csv") %>% filter(!is.na(HTICK)) %>% transmute(ticker = HTICK,industry = factor(HSICCD) )

df_merged <- df_merged %>% left_join(industry_class) %>% rename(asset4 = value)


# Merge with FF 12 Industry Classifications -------------------------------

# The URL for the data.
ff.url <- paste("http://mba.tuck.dartmouth.edu", 
                "pages/faculty/ken.french/ftp",
                "Industry_Definitions.zip", sep="/")

# Download the data and unzip it
f <- tempfile() 
download.file(ff.url, f) 
file.list <- unzip(f,list=TRUE)

trim <- function(string) {
  # Remove leading and trailing spaces from a string
  ifelse(grepl("^\\s*$", string, perl=TRUE),"", 
         gsub("^\\s*(.*?)\\s*$","\\1",string,perl=TRUE))
}

# Function to do the heavy lifting
extract_ff_ind_data <- function (file) {
  
  # Read in the data in a plain form
  ff_ind <- as.vector(read.delim(unzip(f, files=file), header=FALSE, 
                                 stringsAsFactors=FALSE))
  
  # The first 10 characters of each line are the industry data, but only the first
  # row of the data for the SIC codes in an industry are filled in;
  # so fill in the rest.
  ind_num <- trim(substr(ff_ind[,1],1,10))
  for (i in 2:length(ind_num)) { 
    if (ind_num[i]=="") ind_num[i] <- ind_num[i-1]
  }
  
  # The rest of each line is either detail on an industry or details about the
  # range of SIC codes that fit in each industry with a label for each group
  # of SIC codes.
  sic_detail <- trim(substr(ff_ind[,1],11,100))
  
  # If the line doesn't start with a number, it's an industry description
  is.desc <- grepl("^\\D",sic_detail,perl=TRUE)
  
  # Pull out information from rows about industries
  regex.ind <- "^(\\d+)\\s+(\\w+).*$"
  ind_num <- gsub(regex.ind,"\\1",ind_num,perl=TRUE)
  ind_abbrev <- gsub(regex.ind,"\\2",ind_num[is.desc],perl=TRUE)
  ind_list <- data.frame(ind_num=ind_num[is.desc],ind_abbrev, 
                         ind_desc=sic_detail[is.desc])
  
  # Pull out information rows about ranges of SIC codes
  regex.sic <- "^(\\d+)-(\\d+)\\s*(.*)$"
  ind_num <- ind_num[!is.desc]
  sic_detail <- sic_detail[!is.desc]
  sic_low  <- as.integer(gsub(regex.sic,"\\1",sic_detail,perl=TRUE))
  sic_high <- as.integer(gsub(regex.sic,"\\2",sic_detail,perl=TRUE))
  sic_desc <- gsub(regex.sic,"\\3",sic_detail,perl=TRUE)
  sic_list <- data.frame(ind_num, sic_low, sic_high, sic_desc)
  
  return(merge(ind_list,sic_list,by="ind_num",all=TRUE))
}

# Extract the data of interest
#ind_48_table <- extract_ff_ind_data("Siccodes48.txt")
# ind_49_table <- extract_ff_ind_data("Siccodes49.txt")
ind_12_table <- extract_ff_ind_data("Siccodes12.txt") %>% as_tibble() %>%
  mutate(ind_desc = str_remove_all(ind_desc, "\\--.*$")
         , ind_desc = str_remove_all(ind_desc, "\\(.*$")
         , ind_desc = str_remove_all(ind_desc, "[\\s]+$"))

# ind_17_table <- extract_ff_ind_data("Siccodes17.txt")

df_merged <- df_merged %>% 
  mutate(industry = as.integer(paste0(industry))) %>%
  fuzzy_left_join(ind_12_table %>%  select(ind_desc, sic_low, sic_high),by = c("industry" = "sic_low", "industry" = "sic_high")
                  , match_fun = list(`>=`, `<=`)) %>%
  select(-industry,-sic_low, -sic_high) 

# Add Original ticker
tempCallInfo <- df_callInfo %>% group_by(ticker, companyname, year) %>% filter(row_number() == 1) %>% ungroup()

df_merged <- df_merged %>% left_join(tempCallInfo %>% select(ticker, companyname, year, ticker_og))


saveRDS(df_merged,"../Data/scores_yearly_matched_wide.RDS")
df_merged <- df_merged %>% pivot_longer(cols = ends_with("score"), names_to = "score")

saveRDS(df_merged,"../Data/scores_yearly_matched.RDS")
rm(list = ls())
gc()

