library(tidyverse)
library(RPostgres)
library(lubridate)
library(stargazer)
library(sandwich)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='arodloff')

# Load Data ---------------------------------------------------------------
ESG_dimensions <- c("Overall", "E", "S", "G")



# Fama-French 3 Factors
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

temp_file <- tempfile()
download.file(ff_url, temp_file)


ff3_factors <- read_csv(unzip(temp_file), skip = 3) %>%
  rename(date = 1)%>% 
  mutate(date = ym(paste0(date))) %>% 
  drop_na(date) %>% 
  #filter(date %in% seq(2003,2021)) %>% 
  mutate(across(-date, ~./100))


# Monthly CRSP

m_crsp = dbSendQuery(conn = wrds, statement = 
                       "select  a.permno, a.date, a.ret,
                        b.ticker
                        from crsp.msf as a
                        left join crsp.msenames as b
                        on a.permno=b.permno
                        and b.namedt<=a.date
                        and a.date<=b.nameendt
                        where date >= '2002-01-01'
                        and   date <= '2021-12-31'
                       "
) %>% 
  # Pull data
  dbFetch(n = -1) %>% as_tibble() %>% drop_na(ret) %>% mutate(year = year(date) ) 

dbDisconnect(wrds)


# sentim-tf-score ---------------------------------------------------------


# Matched scores
df_merged<- readRDS("../Data/scores_yearly_matched.RDS") %>% 
  mutate(ticker = ticker_og) %>% 
  select(-ticker_og) %>% 
  drop_na(asset4, ind_desc) %>% filter(score == "sentim_TF_score") %>%
  select(-score) 


results_HHLL <- list()
results_HLLH <- list()
res_n <- list()
n_tile = 5
results <- NULL

for(temp_dim in ESG_dimensions){
  #temp_dim = "Overall"
  print(temp_dim)
  #  results <- NULL
  for(temp_year  in seq(2003,2019)){
    
    #temp_year = 2015
    print(temp_year)
    temp  <- df_merged %>%
      filter(year == temp_year) %>% 
      filter(type == temp_dim) %>% 
      transmute(ticker, companyname, ind_desc
                , rank_asset4 = rank(asset4), rank_text = rank(value)
                , ntile_a4 = ntile(rank_asset4, 5)
                , ntile_txt= ntile(rank_text, 5)
      )
    
    
    a4Low_txtLow <- temp %>% filter(ntile_a4 == 1, ntile_txt == 1) %>% pull(ticker)
    a4High_txtHigh <- temp %>% filter(ntile_a4 == n_tile, ntile_txt == n_tile) %>% pull(ticker)
    
    a4High_txtLow <- temp %>% filter(ntile_a4 == n_tile , ntile_txt == 1) %>% pull(ticker)
    a4Low_txtHigh <- temp %>% filter(ntile_a4 == 1, ntile_txt == n_tile) %>% pull(ticker)
    
    # Create portfolios   
    
    port_dispH <- m_crsp %>% 
      filter( ticker %in% c(a4High_txtLow, a4Low_txtHigh ), year == temp_year +1) %>%
      arrange(ticker) %>% group_by(date) %>% summarise(ret_dispH = mean(ret, na.rm = T)) 
    
    port_dispL <- m_crsp %>% 
      filter( ticker %in% c(a4Low_txtLow, a4High_txtHigh ), year == temp_year +1) %>%
      arrange(ticker) %>% group_by(date) %>% summarise(ret_dispL = mean(ret, na.rm = T)) 
    
    temp_res <- bind_cols(
      port_dispH
      , port_dispL %>% select(-date)) %>% 
      mutate(type = temp_dim)
    results <- bind_rows(temp_res, results)
    
  } }
results <- results %>%
  pivot_longer(cols = -c(date, type), names_to = "port") %>%
  mutate(port = str_remove_all(port, "ret_"))
ports <- unique(results$port)

temp2_reg <- list()
se_nw <- list()
for(temp_dim in ESG_dimensions){
  temp_reg <- results %>%
    filter(type == temp_dim) %>% 
    filter(port == "dispH" | port == "dispL" ) %>% 
    pivot_wider(names_from = port) %>%
    rename(long =3, short = 4) %>% 
    transmute(date, type, retLS = long - short) %>% 
    mutate(date = floor_date(date, "month")) %>% left_join(ff3_factors) %>%
    select(-date, -RF,-type) %>%
    lm(retLS ~ ., .)
  temp_se <- sqrt(diag(NeweyWest(temp_reg)))
  temp2_reg[[temp_dim]] <- temp_reg
  se_nw[[temp_dim]] <- temp_se
  
}


stargazer(temp2_reg, se = se_nw, type = "text", title = "HHLL - HLLH",  column.labels = names(temp2_reg))
