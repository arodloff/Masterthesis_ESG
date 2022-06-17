library(tidyverse)
library(stargazer)
library(xtable)


# Load Data ---------------------------------------------------------------

scores_full <-  readRDS("../Data/scores_yearly_matched.RDS")
scores_wide <-  readRDS("../Data/scores_yearly_matched_wide.RDS")
df_aggregated_scores <- scores_full




# Functions ---------------------------------------------------------------

# stars for correlation p-values
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "*  ", "   ")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

# Also stars for correlation coef
startup <- function(x, out=NULL, ...){
  undo <- gsub("\\\\textasteriskcentered", "*", stargazer(x, ...))
  restar <- gsub("* * *", "${}^{***}$", undo, fixed = TRUE)
  restar <- gsub("* *", "${}^{**}$", restar, fixed = TRUE)
  restar <- gsub("* ", "${}^{*}$", restar, fixed = TRUE)
  if(!is.null(out)) cat(restar, file = out, sep="\n")
  restar
}

# Modified boxplot
calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

# For labeling scores -----------------------------------------------------
scores_names <- list(
  'tf_score'="tf_score",
  'tf_idf_score'="tfidf_score",
  'sentim_TF_score'= expression(paste("sentim_tf_score"^{"(FB)"}) ),
  'sentim_score' = expression(paste("sentim_tfidf_score"^{"(FB)"}) ),
  'sentim_TF_LM_score'= expression(paste("sentim_tf_score"^{"(LM)"}) ),
  'sentim_LM_score' =expression(paste("sentim_tfidf_score"^{"(LM)"}) )
)
scores_labeller <- function(variable,value){
  return(scores_names[value])
}


# Other stuff -------------------------------------------------------------

scores <- c("tf_score", "tf_idf_score", "sentim_score", "sentim_LM_score", "sentim_TF_score", "sentim_TF_LM_score")
tf_scores <- scores[c(1,5,6)]
esg_dimensions <- c("Overall","E", "S", "G")
idf_scores <- setdiff(scores, tf_scores)


# Descriptive ------------------------------------------------------------



# Top words --------------------------------------------------------
results <- readRDS("../Data/locations_ESG_words.RDS")

# Top words
results %>% group_by(term, type) %>% count() %>% 
  group_by(type) %>% slice_max(n,n =  20) %>% 
  select(-n) %>% mutate(temp = row_number()) %>% 
  ungroup() %>%
  pivot_wider(names_from = "type", values_from = "term") %>% select(-temp) %>% select(E,S,G) %>% 
  stargazer(.,type = "latex", summary = F, rownames = T)

## Summary Stats -----------------------------------------------------------

stargazer(data.frame(scores_wide %>% filter(type == "Overall")%>% select(ends_with("score"))), type = "latex", summary = T)
stargazer(data.frame(scores_wide %>% filter(type == "E")%>% select(ends_with("score"))), type = "latex", summary = T)
stargazer(data.frame(scores_wide %>% filter(type == "S")%>% select(ends_with("score"))), type = "latex", summary = T)
stargazer(data.frame(scores_wide %>% filter(type == "G")%>% select(ends_with("score"))), type = "latex", summary = T)

## Boxplot -----------------------------------------------------------------


scores_full %>%
  mutate(across(type, factor, levels = c("G","S","E","Overall")) ) %>% 
  mutate(across(score, factor
                , levels = c("tf_score", "tf_idf_score"
                             ,"sentim_TF_score","sentim_score",
                             "sentim_TF_LM_score", "sentim_LM_score"))) %>% 
  ggplot(aes( y = factor(type), x = value, fill = type)) +
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot") +
  scale_fill_manual(values = c("#F8766D","#619CFF","#00BA38","grey50"),guide = guide_legend(reverse = T)) +
  facet_wrap(~score, scales = "free", ncol = 1, labeller = scores_labeller) +
  theme_minimal(base_size = 18) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .3)+
  labs(x = " ", y = ""
  ) +
  theme(legend.title = element_blank())

ggsave("../Plots/boxplot_nooutliers.png", width = 7, height = 10, bg = "white")


## Intra-correlation -------------------------------------------------------

df_merged <- readRDS("../Data/scores_yearly_matched.RDS") %>%
  pivot_wider(id_cols = c(ticker, year, score),names_from = "type", values_from = "value") %>% 
  select(ticker, year, score, Overall, E, S, G) 

#tf scores
for(ii in tf_scores){
  print(ii)
  temp <- df_merged %>% filter(score == ii) %>% select(Overall, E, S, G)
  corstars(temp, "spearman", removeTriangle = "upper", result = "latex" )
}

#tf-idf scores
for(ii in idf_scores){
  print(ii)
  temp <- df_merged %>% filter(score == ii) %>% select(Overall, E, S, G)
  corstars(temp, "spearman", removeTriangle = "upper", result = "latex" )
  
}


## Time-Series -------------------------------------------------------------

df_aggregated_scores <-  df_aggregated_scores %>%
  group_by(year, type, score) %>% 
  summarise(value =  median(value)) 

df_aggregated_scores$type <- factor(df_aggregated_scores$type, levels=c("Overall","E", "S", "G"))  
df_aggregated_scores$score <- factor(df_aggregated_scores$score,
                                     levels=c("tf_score", "tf_idf_score", "sentim_TF_score", "sentim_score",  "sentim_TF_LM_score","sentim_LM_score"))  


df_aggregated_scores %>% ggplot(aes(x = year, y = value, color = type, group = type)) +
  geom_line() +
  geom_smooth(se = F, method = "lm")+
  facet_wrap(~score, scales = "free", ncol = 1,
             labeller = scores_labeller
  ) +
  theme_minimal(base_size = 18) +
  theme(legend.title = element_blank())+
  scale_color_manual(values = c("grey50", "#00BA38" , "#619CFF","#F8766D")) +
  labs(#title = "Mean Scores over Time"
    x = " ", y = " ")

ggsave(filename = "../Plots/median_scores_year.png", width = 7, height = 15, bg = "white")

# Asset4

scores_full %>% 
  drop_na(asset4) %>%
  group_by(year, type) %>% 
  summarize(asset4 = median(asset4)) %>% 
  mutate(across(type, factor, levels = c("Overall", "E", "S", "G")) ) %>% 
  ggplot(aes(x = year, y = asset4, color = type)) +
  geom_line() +
  geom_smooth(se = F, method = "lm") +
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank())+
  scale_color_manual(values = c("grey50", "#00BA38" , "#619CFF","#F8766D")) +
  labs(#title = "Mean Scores over Time"
    x = " ", y = " ")

ggsave(filename = "../Plots/median_asset4_year.png", width = 14, height = 7, bg = "white")
# Correlations ------------------------------------------------------------


## Full sample -------------------------------------------------------------


df_merged <- readRDS("../Data/scores_yearly_matched.RDS") %>% drop_na(asset4)

results <- NULL
for(jj in esg_dimensions){
  temp2 <- df_merged %>% filter(type == jj)
  for(mm in scores){
    temp3 <- temp2 %>% filter(score == mm)
    temp_cor <- cor.test(temp3 %>% pull(value), temp3 %>% pull(asset4), method = "spearman")
    res <- tibble( score = mm
                   , type = jj
                   , cor = temp_cor$estimate
                   #, CI_low = temp_cor$conf.int[1] 
                   #, CI_high =  temp_cor$conf.int[2]
                   , p = temp_cor$p.value)
    results <- bind_rows(res, results)
    
  }
  
}

results$type <- factor(results$type, levels=c("Overall", "E", "S", "G"))

results %>% mutate(star = case_when(p <= 0.01 ~ "***"
                                    ,p > 0.01 & p <= 0.05 ~ "**"
                                    ,p > 0.05 & p <= 0.1 ~ "*"
                                    ,TRUE ~ " "
)
, rho = paste0(round(cor,2)," ", star) 
) %>% 
  select(score, type, rho) %>% pivot_wider(names_from = type, values_from = rho) %>% 
  select(score, Overall, E, S, G) %>% mutate(id = c(4,3,6,5,2,1)) %>% arrange(id) %>% select(-id) %>% 
  startup(.,type = "latex", summary = F, rownames = F) %>% 
  cat(.)



## US Only -----------------------------------------------------------------

# Only US companies -------------------------------------------------------


df_merged <- readRDS("../Data/scores_yearly_matched.RDS") %>% drop_na(asset4, ind_desc)


results <- NULL
for(jj in esg_dimensions){
  temp2 <- df_merged %>% filter(type == jj)
  for(mm in scores){
    temp3 <- temp2 %>% filter(score == mm)
    temp_cor <- cor.test(temp3 %>% pull(value), temp3 %>% pull(asset4), method = "spearman")
    res <- tibble( score = mm
                   , type = jj
                   , cor = temp_cor$estimate
                   , CI_low = temp_cor$conf.int[1] 
                   , CI_high =  temp_cor$conf.int[2]
                   , p = temp_cor$p.value)
    results <- bind_rows(res, results)
    
  }
  
  
  
}

results$type <- factor(results$type, levels=c("Overall", "E", "S", "G"))

results %>% mutate(star = case_when(p <= 0.01 ~ "***"
                                    ,p > 0.01 & p <= 0.05 ~ "**"
                                    ,p > 0.05 & p <= 0.1 ~ "*"
                                    ,TRUE ~ " "
)
, rho = paste0(round(cor,2)," ", star) 
) %>% 
  select(score, type, rho) %>% pivot_wider(names_from = type, values_from = rho) %>% 
  select(score, Overall, E, S, G) %>% mutate(id = c(4,3,6,5,2,1)) %>% arrange(id) %>% select(-id) %>% 
  startup(.,type = "latex", summary = F, rownames = F) %>% 
  cat(.)


# By FF 12 industry --------------------------------------------------------------------
df_merged <- df_merged %>% filter(!is.na(ind_desc))
results <- NULL
for( ii in unique(df_merged$ind_desc)){
  temp <- df_merged %>% filter(ind_desc == ii)
  for(mm in c("tf_score", "tf_idf_score", "sentim_score", "sentim_LM_score", "sentim_TF_score", "sentim_TF_LM_score")){
    temp3 <- temp %>% filter(score == mm)
    n = nrow(temp3)
    temp_cor <- cor.test(temp3 %>% pull(value), temp3 %>% pull(asset4), method = "spearman")
    #ci <- spearman_CI(temp_cor$estimate,n)
    res <- tibble(ind_desc = ii
                  , score = mm
                  , type = "all"
                  , cor = temp_cor$estimate
                  #, CI_low = ci[1] 
                  #, CI_high =  ci[2]
                  #, p = temp_cor$p.value
    )
    results <- bind_rows(res, results)}
  
  for(jj in c("Overall","E", "S", "G")){
    temp2 <- temp %>% filter(type == jj)
    for(mm in c("tf_score", "tf_idf_score", "sentim_score", "sentim_LM_score", "sentim_TF_score", "sentim_TF_LM_score")){
      temp3 <- temp2 %>% filter(score == mm)
      n <- nrow(temp3)
      temp_cor <- cor.test(temp3 %>% pull(value), temp3 %>% pull(asset4), method = "spearman")
      #ci <- spearman_CI(cor = temp_cor$estimate, n = n)
      res <- tibble(ind_desc = ii
                    , score = mm
                    , type = jj
                    , cor = temp_cor$estimate
                    #, CI_low = ci[1] 
                    #, CI_high =  ci[2]
                    #, p = temp_cor$p.value
      )
      results <- bind_rows(res, results)
      
    }
    
    
    
  }
}

results$type <- factor(results$type, levels=c("all","Overall", "E", "S", "G"))

results <- results %>% filter(!(score %in% c("sentim_LM_score", "sentim_TF_LM_score")))
results$score <- factor(results$score, levels = c("tf_score", "tf_idf_score", "sentim_TF_score", "sentim_score"))

### No LM -all -------------------------------------------------------------

score_names <- c("tf_score", "tfidf_score"
                 , expression(paste("sentim_tf_score"^{"(FB)"}))
                 ,expression(paste("sentim_tfidf_score"^{"(FB)"})) )

results %>% filter(type == "all") %>% 
  mutate(type = "All") %>% 
  ggplot(aes(x = cor, y = reorder(ind_desc,cor), fill = score  )   )+
  geom_bar( stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin= CI_low, xmax= CI_high), position = position_dodge(.9), width = 0.5, size=.1, alpha = .5)+
  facet_wrap(~type, scales = "free") +
  labs(x = "Spearman Correlation", y = "" ) +
  geom_vline(xintercept = 0, linetype = "dashed" ) +
  scale_fill_hue(labels = score_names) +
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank(), legend.text.align = 0)

ggsave("../Plots/ff12_all.png", width = 14, height= 7)

### No LM - by category -------------------------------------------------------------


results %>% filter(type != "all") %>%  ggplot(aes(x = cor, y = reorder(ind_desc,cor), fill = score  )   )+
  geom_bar( stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin= CI_low, xmax= CI_high), position = position_dodge(.9), width = 0.5, size=.1, alpha = .3)+
  geom_vline(xintercept = 0, linetype = "dashed" ) +
  facet_wrap(~type, scales = "free", ncol = 1) +
  labs(x = "Spearman Correlation", y = "" ) +
  scale_fill_hue(labels = score_names) +
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank(), legend.text.align = 0)

ggsave("../Plots/sp_ff12_category.png", width = 15, height= 25)

# Table n per ind_desc

df_merged %>% 
  distinct(refinitiv_code, .keep_all = T) %>%
  group_by(ind_desc) %>%  
  count() %>% 
  stargazer(.,summary = F, rownames = F)
  


# Berg et al Tables -------------------------------------------------------

## Berg Original Table -----------------------------------------------------


readxl::read_excel("../Data/berg_draft5.xlsx", sheet = 1, skip = 4) %>%
  mutate(across(everything(), ~ ifelse(is.na(.),0,. )      )) %>% 
  rename(Category = 1, `Vigeo Eiris` = 3) %>% 
  mutate(Category = str_squish(Category)) %>% 
  slice(1:66) %>% 
  stargazer(., type = "latex", summary = F, rownames = F)


## Berg my Classification --------------------------------------------------

readxl::read_excel("../Data/berg_draft5.xlsx", sheet = 2) %>% 
  select(`Taxonomy Category` = 2, `Final ESG Category` = AutoCategory, Sustainalytics, Asset4, `Vigeo Eiris` = `Vigeo  Eiris`, KLD) %>% 
  filter( `Final ESG Category` != 0 ) %>% 
  stargazer(., type = "latex", summary = F, rownames = F
            # , notes = "This table shows the ESG classification of the Berg et al (2019) illustration for four providers and the resulting final classifcation. The values represent the different ESG dimensions of the providers, i.e. 'E' = environmental, 'S' = social, 'G' = governance, 'Ec' = economic[CHECK] (only Asset4), 'F' = financial (only KLD[CHECK]) "
  )

## Berg with seed words --------------------------------------------------------------

readxl::read_excel("../Data/berg_draft5.xlsx", sheet = 3)%>% rename(Taxonomy = 1, Extensions = 3, use = 4) %>% mutate(seed_words = case_when(use == 0 ~ NA_character_,
                                                                                                                                             use == 1 ~ Taxonomy, 
                                                                                                                                             use == 2 ~ Extensions, 
                                                                                                                                             use == 3 ~ paste(Taxonomy, Extensions, sep = ", ")
                                                                                                                                             
)
#,seed_words = str_replace(seed_words, "\\s\\s+, ' ')", " ")
, seed_words = str_to_lower(seed_words)
, seed_words = str_squish(seed_words)
, seed_words = str_replace_all(seed_words, "\\b\\s+\\b", "_")
, Taxonomy = str_squish(Taxonomy)
) %>% select(Taxonomy, Category, seed_words) %>% arrange(Category) %>% filter(!is.na(seed_words)) %>% 
  stargazer(., type = "latex", summary = F, rownames = F)

## Actual seedwords ---------------------------------------------------------------

# full seed words
df <- readxl::read_excel("../Data/berg_draft5.xlsx", sheet = 3)%>% rename(term1 = 1, term2 = 3, use = 4) %>% filter(use >0) %>% 
  pivot_longer(cols = starts_with("term")) %>% filter(!is.na(value), !(use == 2 & name == "term1")) %>% select(-c(name,use, Comment))

# Count seedwords per category
df %>% count(Category)

df <- df %>%  separate_rows(value, sep = ", " ) %>% 
  mutate(value= str_to_lower(value), value = str_replace_all(value,"  ", "_"),  value = str_replace_all(value," ", "_") )
# Load unique trigram tokens
unique_tokens <- fst::read_fst("../Data/tokens.fst", columns = "term") %>% pull(term) %>% unique(.)
seed_words <- df %>% pull(value)

idx <-  seed_words %in% unique_tokens

df_new <- df[idx,]

# Count actual seed words
df_new %>% group_by(Category) %>% count()

# Actual seedwords   
df_new %>% group_by(Category) %>%
  mutate(id = row_number() ) %>% 
  ungroup() %>%  pivot_wider(names_from = "Category", values_from = "value") %>% select(E, S, G) %>% 
  stargazer(., type = "latex", summary = F, rownames = F)



## Dropped Categories ------------------------------------------------------


readxl::read_excel("../Data/berg_draft5.xlsx", sheet = 3) %>% filter(Rating == 0) %>% 
  select(Taxonomy = Taxomony, Comment) %>% 
  stargazer(., type = "latex", summary = F, rownames = F)
