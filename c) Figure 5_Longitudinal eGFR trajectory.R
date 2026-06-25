# Longitudinal models - 3 good kibs for 3 years, 3 bad kibs for 1 year ####


# Setting up ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(tidyr)
library(patchwork)
library(broom)
library(nnet)

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

# Cleaning df a bit & creating scaled KIBs ####
kid_kib_q <- kid_kib_q %>% 
  select(-Smokecurrent, -Alcoholcurrent) %>% 
  left_join(., smoke_alc, by = c("MANOS_ID", "round")) %>% 
  select(c(MANOS_ID, round, round.coded, Biomarker, KIBConc1.pg.mg_UCradj, 
           KIBConc1.pg.mg_UCradj_R1_avged, Country, Site, Industry, age, eGFR_RF, 
           R1R2_stage, sCr, YearsResidence, 
           YearsDept, YearsIndustry, YearsWorkplace, YearsJob,
           ChemHomefreq, JobtaskCat3, Smokecurrent, Alcoholcurrent, 
           ApplyPestHome48Hrs,
           everabove38, highestEducation, YearsResidence_cat, 
          fam_CKD_Status_Group)) %>% 
  group_by(Biomarker, round.coded) %>% 
  mutate(KIB_quintile = ntile(KIBConc1.pg.mg_UCradj, 5)) %>%
  ungroup() %>% 
  distinct() %>% 
  filter(!MANOS_ID %in% indus$MANOS_ID) %>% 
  left_join(., shift, by = "MANOS_ID")



clusters <- read_excel("../original/new cluster assignments MANOS Aug2024.xlsx") %>% 
  mutate(MANOS_ID = as.character(ID), 
         cluster_new = as.character(cluster_new)) %>% 
  dplyr::select(-c("ID")) %>% 
  filter(!is.na(cluster_new)) %>% 
  relocate(cluster_new, .after = "MANOS_ID")

kib_wlastround2 <- left_join(kid_kib_q, clusters, by = "MANOS_ID") 

kib_wlastround2 <- kib_wlastround2 %>%
  mutate(clusters13 = case_when(
    cluster_new == 1 ~ "High and declining", # 1=declining
    cluster_new == 2 ~ "Stable and high",
    cluster_new == 3 ~ "High and declining", # 3=rapidly declining
    cluster_new == 4 ~ "Low and declining",
    is.na(cluster_new) ~ NA)) 
kib_wlastround2$clusters13 <- relevel(as.factor(kib_wlastround2$clusters13), ref = "Stable and high")



biomarkers <- c( "EGF", "IL-18", "KIM-1", "MCP-1", "NGAL", "YKL-40")


justONEround_kib_distinct <- kib_wlastround2 %>% # For AKI since this uses one row per person where ^^ uses 6 and 6-plicates data
  select(c(MANOS_ID, round.coded, Country, Biomarker, KIB_quintile, Site, age, Smokecurrent, Alcoholcurrent, shift_dur_R1, fam_CKD_Status_Group, clusters13)) %>% 
  distinct()

# what about shift duration? No. Remember these are pre-shift values. 
# ARgument for: previous day's work-shift might have contributed to pre-shift values. 

multinom_model <- function(df) {
  df <- as.data.frame(df)
  model <- nnet::multinom(clusters13 ~ KIB_quintile + Smokecurrent + Alcoholcurrent + fam_CKD_Status_Group, data = df)
  tidy_results <- broom::tidy(model, conf.int = TRUE) # tidy() alone doesn't work
  return(tidy_results)
}


biomarker_model_results_adjusted_full <- map_df(biomarkers, function(biomarker) {
  justONEround_kib_distinct %>%
    distinct() %>% 
    filter(Biomarker == biomarker) %>%
    filter(round.coded == "13A") %>% 
    multinom_model() %>%
    as_tibble() %>%  
    mutate(biomarker = biomarker, 
           exponentiated.est = exp(estimate),
           exp.conf.low = exp(conf.low),
           exp.conf.high = exp(conf.high))
}) %>% 
  select(-c(estimate, std.error, statistic, conf.low, conf.high)) %>% 
  filter(term == "KIB_quintile") %>% 
  mutate(combined_label = paste(biomarker, "-" , y.level), 
         biomarker = factor(biomarker, levels = rev(c("EGF", "IL-18", "KIM-1", "MCP-1", "NGAL", "YKL-40")))) 


(gg3 <- ggplot(biomarker_model_results_adjusted_full,
               aes(x = exponentiated.est, y = biomarker, color = y.level)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = exp.conf.low, xmax = exp.conf.high), height = 0.3,
                   position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_text(aes(label = paste0(round(exponentiated.est, 2), " (", 
                                 round(exp.conf.low, 2), ", ", 
                                 round(exp.conf.high, 2), ")"),
                  group = y.level),  # 
              hjust = -0.02,
              vjust = -1,
              size = 2.5,
              color = "black",
              position = position_dodge(width = 0.5))+
    scale_color_manual(values = c("High and declining" = "gray64", "Low and declining" = "#C77CFF")) +
    labs(x = "Odds Ratio",
         y = NULL,
         color = "eGFR trajectory cluster",
         title = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, hjust = 1),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom"))

# minimally different to include or exclude shift duration in model. just keep. 




# #### commented out Repeating a lot of the above code in order to reduce study period to 1 year ####
# 
# 
# kid_kib_q_round_patterns <- kid_kib_q %>%
#   filter(!(round.coded %in% c("11A", "13D", "4", "5", "6"))) %>%  
#   filter(!is.na(eGFR_RF)) %>%                      
#   distinct(MANOS_ID, round) %>%                    # one row per person-round
#   arrange(MANOS_ID, round) %>%                     # sort so the round order is correct
#   group_by(MANOS_ID) %>%
#   summarise(round_pattern = paste(round, collapse = ",")) %>%
#   ungroup()
# 
# kid_kib_q_round_patterns_counts <- kid_kib_q_round_patterns %>%
#   group_by(round_pattern) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))  
# # Just remove those that didn't have R3. 
# # so 29+24=53
# # round_pattern    n
# # 1,2,3            494
# # 1                29
# # 1,2              24
# # 1,3              16
# 
# # Keeping round 123
# kid_kib_q_prop_filtered <- left_join(kid_kib_q, kid_kib_q_round_patterns, by = "MANOS_ID") %>% 
#   filter(!round_pattern %in% c("1", "1,2")) %>% 
#   filter(round.coded %in% c("13A", "2", "3")) %>% 
#   filter(!is.na(Biomarker)) %>% 
#   filter(!is.na(eGFR_RF)) %>% 
#   group_by(MANOS_ID) %>% 
#   mutate(eGFR_R1_13A_copy = eGFR_RF[round.coded == "13A"][1]) %>% 
#   ungroup()
# length(unique(kid_kib_q_prop_filtered$MANOS_ID)) # 455
# 
# 
# differences <- kid_kib_q_prop_filtered %>% 
#   group_by(MANOS_ID) %>%
#   summarize(eGFR_R1 = eGFR_RF[round == 1][1],  # Get eGFR at round 1
#             eGFR_last = eGFR_RF[which.max(round)],  # get eGFR at last available round
#             round_last = round[which.max(round)],  # last round available
#             eGFR_diff = eGFR_last - eGFR_R1,
#             eGFR_percent_diff = ((eGFR_last - eGFR_R1)/eGFR_R1)*100) %>%
#   ungroup()
# 
# kid_kib_q_prop_filtered <- left_join(kid_kib_q_prop_filtered, differences, by = "MANOS_ID") %>% 
#   distinct() %>% 
#   mutate(Biomarker = factor(Biomarker, levels = c( "EGF","IL-18","KIM-1","MCP-1","NGAL","YKL-40"))) %>% 
#   mutate(pct20decline = ifelse(eGFR_percent_diff <= -20, TRUE, FALSE))
# 
# first_to_last_round_egfr_differences <- kid_kib_q_prop_filtered %>% 
#   select(c(MANOS_ID, eGFR_R1, eGFR_last, round_last, eGFR_diff, eGFR_percent_diff, pct20decline, eGFR_R1_13A_copy, shift_dur_R1)) %>% 
#   distinct()
# 
# 
# just13a13d <- kid_kib_q %>% 
#   select(c(MANOS_ID, round.coded, sCr)) %>% 
#   distinct() %>% 
#   filter(round.coded %in% c("13A", "13D")) %>% 
#   pivot_wider(names_from = round.coded, 
#               values_from = sCr) %>% 
#   relocate("13A", "13D", .after = MANOS_ID) %>% 
#   group_by(MANOS_ID) %>% 
#   mutate(sCr_change = `13D` - `13A`) %>% 
#   mutate(sCr_change_pct = 100*sCr_change/`13A`) %>% 
#   relocate(sCr_change, .after = `13D`) %>% 
#   mutate(AKI0.3 = ifelse(sCr_change >= 0.3, TRUE, FALSE), 
#          AKI0.2_20pct = ifelse(sCr_change >= 0.2 | sCr_change_pct >= 20, TRUE, FALSE)) %>% 
#   relocate(sCr_change_pct, AKI0.3, AKI0.2_20pct, .after = sCr_change) %>% #AKI20pct
#   select(c(MANOS_ID, sCr_change, sCr_change_pct, AKI0.3, AKI0.2_20pct)) %>% 
#   distinct() %>% 
#   ungroup()
# 
# kib_wlastround <- left_join(kid_kib_q_prop_filtered, just13a13d, by = "MANOS_ID") %>% 
#   ungroup() %>% 
#   relocate(sCr_change, sCr_change_pct, AKI0.3,AKI0.2_20pct, .after = sCr) 
# 
# 
# lm_pctdecline_full_adj <- kib_wlastround %>%
#   filter(round.coded %in% "13A") %>% 
#   split(.$Biomarker) %>%
#   map(~lm(eGFR_percent_diff ~ KIB_quintile + age + Site + fam_CKD_Status_Group  + Smokecurrent + shift_dur_R1 + Alcoholcurrent+ eGFR_R1_13A_copy,
#           data = .)) %>% 
#   map(., ~tidy(., conf.int = TRUE)) %>% 
#   bind_rows(.id = "Biomarker") %>% 
#   select(-c(std.error, statistic)) %>% 
#   filter(term == "KIB_quintile") %>% 
#   mutate(Biomarker = factor(Biomarker, levels = rev(c("EGF", "IL-18", "KIM-1", "MCP-1", "NGAL", "YKL-40"))))
# 
# glm_30pctdecline_full_adj <- kib_wlastround %>%
#   filter(round.coded %in% "13A") %>% 
#   split(.$Biomarker) %>%
#   map(~glm(pct20decline ~ KIB_quintile + age + Site + fam_CKD_Status_Group  + Smokecurrent + shift_dur_R1 + Alcoholcurrent+ eGFR_R1_13A_copy,
#            data = ., family = "binomial")) %>% #
#   map(., ~tidy(., conf.int = TRUE)) %>% 
#   bind_rows(.id = "Biomarker") %>% 
#   mutate(exponentiated.est = exp(estimate), 
#          exp.conf.low = exp(conf.low), 
#          exp.conf.high = exp(conf.high)) %>% 
#   select(-c(std.error, statistic)) %>% 
#   filter(term == "KIB_quintile") %>% 
#   mutate(Biomarker = factor(Biomarker, levels = rev(c("EGF", "IL-18", "KIM-1", "MCP-1", "NGAL", "YKL-40"))))
# 
# 
# (gg1 <- ggplot(lm_pctdecline_full_adj, aes(x = estimate, y = Biomarker)) + 
#     geom_point(size = 3, position = position_dodge(width = 0.6)) +
#     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
#                    height = 0.2,
#                    position = position_dodge(width = 0.6)) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
#     geom_text(aes(label = paste0(round(estimate, 2), " (", 
#                                  round(conf.low, 2), ", ", 
#                                  round(conf.high, 2), ")")),
#               #hjust = -0.4,      # adjust position horizontally
#               vjust = -2.2,
#               size = 2.5, 
#               color = "black") + # override color for readability
#     labs(title = NULL, #"A. Percent Change in eGFR"
#          x = "Percent Difference",
#          y = NULL) +
#     theme_minimal() + theme(legend.position = "none") + 
#     theme(axis.text.y = element_text(size = 12),
#           axis.text.x = element_text(size = 12,  hjust = 1),
#           plot.title = element_text(hjust = 0.5)))
# 
# 
# (gg2 <- ggplot(glm_30pctdecline_full_adj, aes(x = Biomarker, y = exponentiated.est)) + 
#     geom_point(size = 3, position = position_dodge(width = 0.6)) +
#     geom_errorbar(aes(ymin = exp.conf.low, ymax = exp.conf.high),
#                   width = 0.2,
#                   position = position_dodge(width = 0.6)) +
#     geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
#     geom_text(aes(label = paste0(round(exponentiated.est, 2), " (", 
#                                  round(exp.conf.low, 2), ", ", 
#                                  round(exp.conf.high, 2), ")")),
#               vjust = -2.2,
#               size = 2.5, 
#               color = "black") + 
#     labs(title = NULL,#"B. Odds of 30% eGFR Decline"
#          x = NULL,
#          y = "Odds Ratio",
#          color = "Model Type") +
#     theme_minimal() + theme(legend.position = "none") + 
#     theme(axis.text.x = element_text(size = 12, hjust = 1),
#           axis.text.y = element_text(size = 12),
#           plot.title = element_text(hjust = 0.5)) +
#     coord_flip())
# 
# 
# 
# (gg1 | gg2)
# 
