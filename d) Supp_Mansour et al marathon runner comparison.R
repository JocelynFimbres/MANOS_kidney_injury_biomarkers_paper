# Graphing Mansour's KIB concentrations ####
# Set-up ####
library(tidyverse)
library(tidyr)
library(moments)

# Manually enter Mansour data ####
mansour <- data.frame(Biomarker = rep(c("IL-18", "MCP-1", "YKL-40", "KIM-1", "NGAL"), each = 3),
                   Shift = rep(c("Day 0 (pre-run)", "Day 1 (post-run)", "Day 2 (post-marathon)"), 
                               times = 5),
                   Median = c(6.43, 45.89, 16.95,
                              39.56, 264.47, 186.28,
                              96.25, 865.13, 202.97,
                              132.59, 723.32, 702.42,
                              8000, 37640, 18490),
                   IQR_Low = c(4.24, 23.42, 4.84,
                              26.12, 131.12, 55.91,
                              43.96, 466.84, 55.91,
                              67.61, 459.36, 123.27,
                              4150, 19030, 9250),
                   IQR_High = c(12.26, 63.45, 29.98,
                              79.29, 702.01, 366.74,
                              124.31, 1764.28, 398.81,
                              219.98, 1970.64, 1098.67,
                              30480, 84610, 33690)) %>%
  mutate(Lower = Median - IQR_Low,
         Upper = IQR_High - Median)

     
# Plot Mansour ####
gg_mansour <- ggplot(mansour %>% 
                                filter(Shift %in% c("Day 0 (pre-run)", "Day 1 (post-run)")), 
                              aes(x = Shift, y = Median, fill = Shift)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Biomarker, scales = "free_y") +
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  geom_errorbar(aes(ymax = IQR_High, 
                    ymin = IQR_Low), 
                width = 0.2, position = position_dodge(0.9))  +
  labs(title = "Median (IQR) Before and After Marathon",
       x = "Time", y = "Median Concentration (pg/mL)",
       fill = "Shift")


# MANOS KIBs ####

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

source("processed/pMANOS_KIBs.R")
rm(cor_13A_spear, cor_R3_spear, kibs_long_wide_13A, 
   kibs_long_wide_13A_CorrVars, kibs_long_wide_R3, kibs_long_wide_R3_CorrVars, SumStats)

kibs_long$KIBConc1[is.nan(kibs_long$KIBConc1)] <- NA
# KIBConc1 is in pg/mL

skewness(kibs_long$KIBConc1[kibs_long$Biomarker=="YKL-40" & kibs_long$round.coded == "13A"], na.rm = T) # 10.12533, must do medians for all KIBs

# Summary ####
kibs_long_summary <- kibs_long %>%
  filter(!is.na(Biomarker)) %>%
  group_by(round.coded, Biomarker) %>%
  summarize(median_KIBConc1 = median(KIBConc1, na.rm = TRUE),
            Q1_KIBConc1 = quantile(KIBConc1, 0.25, na.rm = TRUE),
            Q3_KIBConc1 = quantile(KIBConc1, 0.75, na.rm = TRUE),
            .groups = 'drop') 


# 13A & 13D ####
kid_kib_summary_13A_13D <- kibs_long_summary %>% 
  filter(round.coded %in% c("13A", "13D"))

kid_kib_summary_13A_13D$round.coded[kid_kib_summary_13A_13D$round.coded=="13A"] <- "Pre-shift"
kid_kib_summary_13A_13D$round.coded[kid_kib_summary_13A_13D$round.coded=="13D"] <- "Post-shift"
kid_kib_summary_13A_13D <- kid_kib_summary_13A_13D %>% 
  mutate(round.coded = factor(round.coded, levels = c("Pre-shift", "Post-shift")))

# Mansour didn't test EGF so removing here: 
kid_kib_summary_13A_13D <- kid_kib_summary_13A_13D %>% 
  filter(!Biomarker %in% "EGF")

gg_manos_13A_13D <- ggplot(kid_kib_summary_13A_13D, aes(x = round.coded, y = median_KIBConc1, fill = round.coded)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Biomarker, scales = "free_y") +
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  geom_errorbar(aes(ymax = Q3_KIBConc1, 
                    ymin = Q1_KIBConc1), 
                width = 0.2, position = position_dodge(0.9))  +
  labs(title = "MANOS",
       x = "Shift", y = "Median Concentration (IQR) (pg/mL)",
       fill = "Round Coded")



# Combine both datasets to get the same axes ####
mansour_manos <- bind_rows(kid_kib_summary_13A_13D %>% 
                             transmute(Biomarker, Value = Q3_KIBConc1),
                           mansour %>% 
                             filter(Shift %in% c("Day 0 (pre-run)", "Day 1 (post-run)")) %>% 
                             transmute(Biomarker, Value = IQR_High))

global_ymax <- mansour_manos %>%
  group_by(Biomarker) %>%
  summarize(ymax = max(Value, na.rm = TRUE)) %>%
  deframe()

# Plot 1
gg_manos_13A_13D <- ggplot(kid_kib_summary_13A_13D, aes(x = round.coded, y = median_KIBConc1, fill = round.coded)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = Q3_KIBConc1, ymin = Q1_KIBConc1), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_blank(data = kid_kib_summary_13A_13D %>% mutate(ymax = global_ymax[Biomarker]), 
             aes(y = ymax)) +
  facet_wrap(~ Biomarker, scales = "free_y") +
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  labs(title = "Median (IQR) Before and After Work-shift",
       x = "Shift", y = "Median Concentration (pg/mL)",
       fill = "Round Coded")
# Plot 2
gg_mansour <- ggplot(mansour %>% filter(Shift %in% c("Day 0 (pre-run)", "Day 1 (post-run)")), 
                              aes(x = Shift, y = Median, fill = Shift)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = IQR_High, ymin = IQR_Low), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_blank(data = mansour %>% 
               filter(Shift %in% c("Day 0 (pre-run)", "Day 1 (post-run)")) %>% 
               mutate(ymax = global_ymax[Biomarker]), aes(y = ymax)) +
  facet_wrap(~ Biomarker, scales = "free_y") +
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  labs(title = "Median (IQR) Before and After Marathon",
       x = "Time", y = "Median Concentration (pg/mL)",
       fill = "Shift")

# COMBINE INTO ONE GRAPH ####

df1 <- kid_kib_summary_13A_13D %>%
  select(Biomarker, round.coded, median_KIBConc1, Q1_KIBConc1, Q3_KIBConc1) %>%
  rename(Time = round.coded,
         Median = median_KIBConc1,
         Q1 = Q1_KIBConc1,
         Q3 = Q3_KIBConc1) %>%
  mutate(Source = "MANOS participants")

df2 <- mansour %>%
  filter(Shift %in% c("Day 0 (pre-run)", "Day 1 (post-run)")) %>%
  select(Biomarker, Shift, Median, IQR_Low, IQR_High) %>%
  rename(Time = Shift,
         Q1 = IQR_Low,
         Q3 = IQR_High) %>%
  mutate(Source = "Mansour et al. marathon runners")

df2$Time[df2$Time=="Day 0 (pre-run)"] <- "Pre-run"
df2$Time[df2$Time=="Day 1 (post-run)"] <- "Post-run"

mansour_manos_df <- bind_rows(df1, df2) %>%
  mutate(fill_name = paste(Biomarker, Time, sep = "_")) %>% 
  mutate(Time = factor(Time, levels = c("Pre-run",  "Post-run", "Pre-shift", "Post-shift")), 
         Source = factor(Source, levels = c("Mansour et al. marathon runners","MANOS participants")), 
         Biomarker = factor(Biomarker, levels = c("IL-18","KIM-1", "MCP-1", "NGAL", "YKL-40"))) 
  

(gg_combined <- ggplot(mansour_manos_df, 
                      aes(x = Time, y = Median, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), 
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Biomarker, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = NULL,
       x = NULL, y = "Concentration (pg/mL)",
       fill = "Study") )


