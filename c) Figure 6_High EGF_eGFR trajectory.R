# Setup ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(moments)
library(nnet)
library(broom) 
library(broom.mixed)
library(purrr)
library(lme4)
library(logistf)

source("Merged dfs/kid_kib_qaire.R")
smoke_alc <- read_excel("Merged dfs/smoke_alcohol_byRound_092325.xlsx")
indus <- read_excel("Merged dfs/20250730_Industry changes.xlsx") %>% 
  select(c(MANOS_ID, changes)) %>% 
  filter(changes %in% TRUE)
shift <- read_excel("Merged dfs/ind_site_smoke_alc_shift.xlsx") %>% 
  filter(round.coded %in% "13A") %>% 
  select(MANOS_ID, shift_dur_R1) %>% 
  distinct() %>% 
  group_by(MANOS_ID) %>%
  slice(1) %>%
  ungroup()

kid_kib_q <- kid_kib_q %>% 
  select(-Smokecurrent, -Alcoholcurrent) %>% 
  left_join(., smoke_alc, by = c("MANOS_ID", "round")) %>% 
  select(c(MANOS_ID, round, round.coded, Biomarker, KIBConc1.pg.mg_UCradj, 
           Country, Site, Industry, age, eGFR_RF, 
           R1R2_stage, sCr, YearsResidence, 
           YearsDept, YearsIndustry, YearsWorkplace, YearsJob,
           ChemHomefreq, JobtaskCat3, Smokecurrent, Alcoholcurrent, 
           ApplyPestHome48Hrs, 
           everabove38, fam_CKD_Status_Group, highestEducation, YearsResidence_cat)) %>% 
  group_by(Biomarker, round.coded) %>% 
  mutate(KIB_quintile = ntile(KIBConc1.pg.mg_UCradj, 5)) %>%
  ungroup() %>% 
  distinct() %>% 
  filter(!MANOS_ID %in% indus$MANOS_ID) %>% 
  left_join(., shift, by = "MANOS_ID")

kid_kib_q <- kid_kib_q %>% 
  filter(!(MANOS_ID == "231" & Smokecurrent == "No")) %>% 
  distinct()

clusters <- read_excel("../original/new cluster assignments MANOS Aug2024.xlsx") %>% 
  mutate(MANOS_ID = as.character(ID), 
         cluster_new = as.character(cluster_new)) %>% 
  dplyr::select(-c("ID")) %>% 
  filter(!is.na(cluster_new)) %>% 
  relocate(cluster_new, .after = "MANOS_ID")

kid_kib_q <- left_join(kid_kib_q, clusters, by = "MANOS_ID") %>%
  mutate(clusters13 = case_when(cluster_new == 1 ~ "High and declining", # 1=declining
                                cluster_new == 2 ~ "Stable and high",
                                cluster_new == 3 ~ "High and declining", # 3=rapidly declining
                                cluster_new == 4 ~ "Low and declining",
                                is.na(cluster_new) ~ NA)) 
kid_kib_q$clusters13 <- relevel(as.factor(kid_kib_q$clusters13), ref = "Stable and high")



df <- kid_kib_q %>% 
  filter(round.coded %in% "13A") %>% 
  mutate(EGF = ifelse(Biomarker == "EGF" & KIB_quintile == "5", "high", 
                      ifelse(Biomarker == "EGF" & KIB_quintile == "1", "low", NA))) %>% 
  group_by(MANOS_ID) %>% 
  fill(EGF, .direction = "updown") %>% 
  ungroup() %>% 
  filter(!is.na(EGF)) %>% 
  select(c(MANOS_ID, EGF)) %>% 
  distinct()

high_five <- kid_kib_q %>% 
  filter(round.coded %in% "13A") %>% 
  filter(!Biomarker %in% "EGF") %>% 
  filter(KIB_quintile %in% c("4", "5")) %>% 
  mutate(KIB_quintile = as.numeric(KIB_quintile)) %>% 
  select(c(MANOS_ID, Biomarker, KIB_quintile, clusters13, Smokecurrent, Alcoholcurrent, fam_CKD_Status_Group, shift_dur_R1))


newdf <- left_join(high_five, df, by = "MANOS_ID") %>%
  filter(!is.na(EGF)) 
newdf$EGF <- relevel(as.factor(newdf$EGF), ref = "low")


multinomi <- function(df){
  nnet::multinom(clusters13 ~ EGF + 
                   Smokecurrent + Alcoholcurrent + fam_CKD_Status_Group + shift_dur_R1, # not age, not industry - already in clusters
                 data=df)
}

multinom_results_lowegf_ngal <- newdf %>% 
  filter(Biomarker %in% "NGAL") %>% 
  multinomi() %>% 
  broom::tidy(conf.int = TRUE) %>%
  filter(term == "EGFhigh") %>% 
  mutate(exponentiated.est = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high)) %>% 
  select(-c(std.error, statistic)) %>% 
  mutate(Biomarker = "NGAL")

multinom_results_lowegf_ykl <- newdf %>% 
  filter(Biomarker %in% "YKL-40") %>% 
  multinomi() %>% 
  broom::tidy(conf.int = TRUE) %>%
  mutate(exponentiated.est = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high)) %>% 
  select(-c(std.error, statistic)) %>% 
  filter(term == "EGFhigh") %>% 
  mutate(Biomarker = "YKL-40")

multinom_results_lowegf_il <- newdf %>% 
  filter(Biomarker %in% "IL-18") %>% 
  multinomi() %>% 
  broom::tidy(conf.int = TRUE) %>%
  mutate(exponentiated.est = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high)) %>% 
  select(-c(std.error, statistic)) %>% 
  filter(term == "EGFhigh") %>% 
  mutate(Biomarker = "IL-18")

multinom_results_lowegf_mcp <- newdf %>% 
  filter(Biomarker %in% "MCP-1") %>% 
  multinomi() %>% 
  broom::tidy(conf.int = TRUE) %>%
  mutate(exponentiated.est = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high)) %>% 
  select(-c(std.error, statistic)) %>% 
  filter(term == "EGFhigh") %>% 
  mutate(Biomarker = "MCP-1")

multinom_results_lowegf_kim <- newdf %>% 
  filter(Biomarker %in% "KIM-1") %>% 
  multinomi() %>% 
  broom::tidy(conf.int = TRUE) %>%
  mutate(exponentiated.est = exp(estimate),
         exp.conf.low = exp(conf.low),
         exp.conf.high = exp(conf.high)) %>% 
  select(-c(std.error, statistic)) %>% 
  filter(term == "EGFhigh") %>% 
  mutate(Biomarker = "KIM-1")

# multinom_results_lowegf_il18 <- newdf %>% 
#   filter(Biomarker %in% "IL-18") %>% 
#   multinomi() %>% 
#   broom::tidy(conf.int = TRUE) %>%
#   mutate(exponentiated.est = exp(estimate),
#          exp.conf.low = exp(conf.low),
#          exp.conf.high = exp(conf.high)) %>% 
#   select(-c(std.error, statistic)) %>% 
#   filter(term == "EGFlow") %>% 
#   mutate(Biomarker = "IL-18")
# multinom_results_lowegf_kim <- newdf %>% 
#   filter(Biomarker %in% "KIM-1") %>% 
#   multinomi() %>% 
#   broom::tidy(conf.int = TRUE) %>%
#   mutate(exponentiated.est = exp(estimate),
#          exp.conf.low = exp(conf.low),
#          exp.conf.high = exp(conf.high)) %>% 
#   select(-c(std.error, statistic)) %>% 
#   filter(term == "EGFlow") %>% 
#   mutate(Biomarker = "KIM-1")
# 
# multinom_results_lowegf_mcp <- newdf %>% 
#   filter(Biomarker %in% "MCP-1") %>% 
#   multinomi() %>% 
#   broom::tidy(conf.int = TRUE) %>%
#   mutate(exponentiated.est = exp(estimate),
#          exp.conf.low = exp(conf.low),
#          exp.conf.high = exp(conf.high)) %>% 
#   select(-c(std.error, statistic)) %>% 
#   filter(term == "EGFlow") %>% 
#   mutate(Biomarker = "MCP-1")

indv_kib <- rbind(multinom_results_lowegf_ngal, multinom_results_lowegf_ykl, multinom_results_lowegf_il, multinom_results_lowegf_mcp, multinom_results_lowegf_kim) %>% 
  mutate(Biomarker = factor(Biomarker, levels = c("YKL-40", "NGAL", "MCP-1", "KIM-1", "IL-18")))


(gg0 <- ggplot(indv_kib %>% filter(!p.value==0), 
               aes(x = exponentiated.est, y = Biomarker, color = y.level)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = exp.conf.low, xmax = exp.conf.high), height = 0.3,
                   position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("High and declining" = "gray64", "Low and declining" = "#C77CFF")) +
    geom_text(aes(label = paste0(round(exponentiated.est, 2), " (", 
                                 round(exp.conf.low, 3), ", ", 
                                 round(exp.conf.high, 2), ")"),
                  group = y.level),  # 
              hjust = -0.1,
              vjust = -0.85,
              size = 3,
              color = "black",
              position = position_dodge(width = 0.5)) +
    labs(x = "Odds Ratio",
         y = NULL,
         color = "eGFR trajectory cluster",
         #shape = NULL,
         title = NULL,
         subtitle = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, hjust = 1),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom"))



