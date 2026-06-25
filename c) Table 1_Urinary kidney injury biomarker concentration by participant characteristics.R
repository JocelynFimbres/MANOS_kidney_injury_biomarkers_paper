# Table 1 ####
# Set-up ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(tidyr)
library(purrr)
library(glue)

source("Merged dfs/kid_kib_qaire.R")
rm(list = setdiff(names(Filter(is.data.frame, mget(ls()))),  "kid_kib_q"))

# Which participants experienced KDIGO AKI? ####
aki <- kid_kib_q %>%
  filter(round.coded %in% c("13A","13D")) %>%
  select(MANOS_ID, round.coded, sCr) %>%
  distinct() %>% pivot_wider(names_from = round.coded, values_from = sCr) %>%
  mutate(sCr_change = `13D` - `13A`,
         over0.3 = sCr_change >= 0.3) %>%
  select(MANOS_ID, over0.3)

# Participants at measured study visits ####
kid_kib_q_round_patterns <- kid_kib_q %>%
  filter(!is.na(eGFR_RF)) %>%
  distinct(MANOS_ID, round) %>%
  arrange(MANOS_ID, round) %>%
  group_by(MANOS_ID) %>%
  summarise(round_pattern = paste(round, collapse = ",")) %>%
  ungroup()
kid_kib_q_round_patterns_counts <- kid_kib_q_round_patterns %>%
  group_by(round_pattern) %>%
  summarise(n = n()) %>%
  arrange(desc(n))  
# Remove those that only stayed for visit 1 & visit 1+2
# Remove Day 1 & Day 3 post-shift
kid_kib_q_prop_filtered <- left_join(kid_kib_q, kid_kib_q_round_patterns, by = "MANOS_ID") %>% 
  filter(!round_pattern %in% c("1", "1,2")) %>% 
  filter(!round.coded %in% c("11A", "13D"))

# Find the difference in eGFR from the first to last visit ####
differences <- kid_kib_q_prop_filtered %>%
  group_by(MANOS_ID) %>%
  summarize(eGFR_R1 = eGFR_RF[round == 1][1],
    eGFR_last = eGFR_RF[which.max(round)],
    round_last = round[which.max(round)],
    eGFR_diff = eGFR_last - eGFR_R1,
    eGFR_percent_diff = ((eGFR_last - eGFR_R1)/eGFR_R1) * 100,
    .groups = "drop") %>%
  mutate(direction = ifelse(eGFR_diff >= 0, "Incr", "Decr"),
    pct30decline = eGFR_percent_diff <= -30) %>%
  select(MANOS_ID, direction, pct30decline)


# Read-in the eGFR trajectory clusters data ####
clusters <- read_excel("../original/new cluster assignments MANOS Aug2024.xlsx") %>%
  mutate(MANOS_ID = as.character(ID),
         cluster_new = as.character(cluster_new)) %>%
  select(MANOS_ID, cluster_new)



analysis_df <- kid_kib_q %>%
  filter(round.coded == "13A") %>%
  filter(!is.na(Biomarker)) %>%
  filter(!is.nan(KIBConc1.pg.mg_UCradj)) %>%
  left_join(clusters, by = "MANOS_ID") %>%
  left_join(differences, by = "MANOS_ID") %>%
  left_join(aki, by = "MANOS_ID") %>%
  distinct()


cat_vars <- c("Country", "agecat", "YearsResidence_cat", "Industry", "fam_CKD_Status_Group", "pct30decline", "cluster_new")
biomarkers <- c("EGF", "IL-18", "KIM-1","MCP-1","NGAL", "YKL-40" )
total_n <- analysis_df %>%
  distinct(MANOS_ID) %>%
  nrow()


summ_fcn <- function(var){
  
  df <- analysis_df %>%
    select(all_of(var), Biomarker, KIBConc1.pg.mg_UCradj, MANOS_ID) %>%
    drop_na()
  
  summary_tbl <- df %>%
    group_by(!!sym(var), Biomarker) %>%
    summarise(
      n = n_distinct(MANOS_ID),
      median = median(KIBConc1.pg.mg_UCradj),
      Q1 = quantile(KIBConc1.pg.mg_UCradj, 0.25),
      Q3 = quantile(KIBConc1.pg.mg_UCradj, 0.75),
      .groups = "drop"
    ) %>%
    mutate(
      summary = glue("{round(median,1)} ({round(Q1,1)}, {round(Q3,1)})"),
      variable = var,
      category = as.character(!!sym(var))
    )
  
  pvals <- df %>%
    group_by(Biomarker) %>%
    summarise(
      p = {
        g <- cur_data()[[var]]
        if(length(unique(g)) == 2){
          wilcox.test(KIBConc1.pg.mg_UCradj ~ g)$p.value
        } else {
          kruskal.test(KIBConc1.pg.mg_UCradj ~ g)$p.value
        }
      },
      .groups="drop"
    )
  
  summary_tbl %>%
    left_join(pvals, by="Biomarker")
}

summary_table <- purrr::map_df(cat_vars, summ_fcn)




# SAME AS ABOVE BUT NOW WITH A 20% EGFR DECLINE AFTER 1 YEAR (R1 R2 R3) ####
kid_kib_q_prop_filtered2 <- left_join(kid_kib_q, kid_kib_q_round_patterns, by = "MANOS_ID") %>% 
  filter(!round_pattern %in% c("1", "1,2")) %>% 
  filter(!round.coded %in% c("11A", "13D")) %>% 
  # Everything the same but now we are keeping the first year of the study:
  filter(round.coded %in% c("13A", "2", "3"))
differences2 <- kid_kib_q_prop_filtered2 %>%
  group_by(MANOS_ID) %>%
  summarize(eGFR_R1 = eGFR_RF[round == 1][1],
            eGFR_last = eGFR_RF[which.max(round)],
            round_last = round[which.max(round)],
            eGFR_diff = eGFR_last - eGFR_R1,
            eGFR_percent_diff = ((eGFR_last - eGFR_R1)/eGFR_R1) * 100,
            .groups = "drop") %>%
  mutate(pct20decline = eGFR_percent_diff <= -20) %>%
  select(MANOS_ID, pct20decline)

analysis_df2 <- kid_kib_q %>%
  filter(round.coded == "13A") %>%
  filter(!is.na(Biomarker)) %>%
  filter(!is.nan(KIBConc1.pg.mg_UCradj)) %>%
  left_join(clusters, by = "MANOS_ID") %>%
  left_join(differences2, by = "MANOS_ID") %>%
  distinct()

length(unique(analysis_df2$MANOS_ID)) #471

cat_vars2 <- c("pct20decline") 
biomarkers <- c("EGF", "IL-18", "KIM-1","MCP-1","NGAL", "YKL-40")
total_n2 <- analysis_df2 %>%
  distinct(MANOS_ID) %>%
  nrow()


summ_fcn2 <- function(var){
  
  df <- analysis_df2 %>%
    select(all_of(var), Biomarker, KIBConc1.pg.mg_UCradj, MANOS_ID) %>%
    drop_na()
  
  summary_tbl <- df %>%
    group_by(!!sym(var), Biomarker) %>%
    summarise(
      n = n_distinct(MANOS_ID),
      median = median(KIBConc1.pg.mg_UCradj),
      Q1 = quantile(KIBConc1.pg.mg_UCradj, 0.25),
      Q3 = quantile(KIBConc1.pg.mg_UCradj, 0.75),
      .groups = "drop"
    ) %>%
    mutate(
      summary = glue("{round(median,1)} ({round(Q1,1)}, {round(Q3,1)})"),
      variable = var,
      category = as.character(!!sym(var))
    )
  
  pvals <- df %>%
    group_by(Biomarker) %>%
    summarise(
      p = {
        g <- cur_data()[[var]]
        if(length(unique(g)) == 2){
          wilcox.test(KIBConc1.pg.mg_UCradj ~ g)$p.value
        } else {
          kruskal.test(KIBConc1.pg.mg_UCradj ~ g)$p.value
        }
      },
      .groups="drop"
    )
  
  summary_tbl %>%
    left_join(pvals, by="Biomarker")
}


summary_table2 <- purrr::map_df(cat_vars2, summ_fcn2)

