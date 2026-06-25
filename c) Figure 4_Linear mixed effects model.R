# Linear mixed effects model with 3 timepoints (13A, 13D, 3) ####

# Set-up ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(moments)
library(broom) 
library(broom.mixed)
library(lme4)

source("Merged dfs/kid_kib_qaire.R")
smoke_alc <- read_excel("Merged dfs/smoke_alcohol_byRound_092325.xlsx")
shifts_r1 <- read_excel("Merged dfs/ind_site_smoke_alc_shift.xlsx") %>% 
  filter(round.coded %in% c("13A", "13D")) %>% 
  select(MANOS_ID, round, shift_dur_R1) %>% 
  distinct() %>% 
  group_by(MANOS_ID) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(round = as.character(round))

shifts_r3 <- read_excel("Merged dfs/ind_site_smoke_alc_shift.xlsx") %>% 
  filter(round.coded %in% c("3")) %>% 
  select(MANOS_ID, round, shift_dur_R1) %>% 
  distinct() %>% 
  group_by(MANOS_ID) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(round = as.character(round))

shifts <- rbind(shifts_r1, shifts_r3)


# Cleaning df & creating scaled KIBs ####

kid_kib_q <- kid_kib_q %>% 
  select(-Smokecurrent, -Alcoholcurrent) %>% 
  left_join(., smoke_alc, by = c("MANOS_ID", "round")) %>% 
  select(c(MANOS_ID, round, round.coded, Biomarker, KIBConc1.pg.mg_UCradj, 
           Country, Site, Industry, age, eGFR_RF, # , Dept3, CurrentJob
           R1R2_stage, sCr, YearsResidence, 
           YearsDept, YearsIndustry, YearsWorkplace, YearsJob,
           ChemHomefreq, JobtaskCat3, Smokecurrent, Alcoholcurrent, 
           ApplyPestHome48Hrs, Shift_Duration,
           everabove38, fam_CKD_Status_Group, highestEducation, YearsResidence_cat, 
           Shift_Duration_cat)) %>% 
  group_by(Biomarker, round.coded) %>% 
  mutate(KIB_quintile = ntile(KIBConc1.pg.mg_UCradj, 5), 
         KIB_tertile = ntile(KIBConc1.pg.mg_UCradj, 3)) %>%
  ungroup() %>% 
  distinct() %>% 
  left_join(., shifts, by = c("MANOS_ID", "round")) %>% 
  filter(!is.na(eGFR_RF), 
         !is.na(Biomarker)) %>% 
  distinct() %>% 
  select(c(MANOS_ID, round.coded, Biomarker, eGFR_RF, KIB_quintile, Site, 
           fam_CKD_Status_Group, age, Smokecurrent, Alcoholcurrent, shift_dur_R1))



# Fully adjusted function - KIB quintiles=character & round.coded=numeric to get average assc between KIB and eGFR ####
fulladjusted_lmer_quint_char <- kid_kib_q %>% 
  mutate(KIB_quintile = as.character(KIB_quintile)) %>% 
  mutate(round.num = case_when(round.coded == "13A" ~ 1, 
                               round.coded == "13D" ~ 1.5, 
                               round.coded == "3" ~ 3)) %>% 
  split(.$Biomarker) %>%
  map(~lmer(eGFR_RF ~ KIB_quintile*round.num + 
              Site + fam_CKD_Status_Group + age + Smokecurrent + Alcoholcurrent + shift_dur_R1 +
              (1 | MANOS_ID), data = .)) %>%
  map(~tidy(., conf.int = TRUE)) %>% 
  bind_rows(.id = "Biomarker") %>% 
  filter(grepl("KIB_quintile", term)) %>% 
  mutate(quintile = case_when(grepl("quintile2", term) ~ "2",
                              grepl("quintile3", term) ~ "3", 
                              grepl("quintile4", term) ~ "4", 
                              grepl("quintile5", term) ~ "5"), 
         int = case_when(grepl("round.num", term) ~ "Time", TRUE ~ "No time"),
         Biomarker = factor(Biomarker, levels = rev(c("EGF", "IL-18", "KIM-1", "MCP-1", "NGAL", "YKL-40")))) %>% 
  select(-c(std.error, statistic))



ggplot(fulladjusted_lmer_quint_char %>% filter(int %in% "No time"), 
       aes(y = Biomarker, x = estimate, color = quintile)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(y = NULL, x= "Estimate (ml/min/1.73 m², 95% CI)", 
       title = NULL, 
       color = "Concentration\nquintile")
