# ####

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(readxl)
library(tidyverse)

source("processed/pMANOS_qaire.R")
rm(R1_D0_D3, R1_D0Q, R1_D0Q_og, R1_D0Qlong, R1_PSQ, R3_DW, R3PSQ, R3PSQ.2, R1_PSQ_og)

source("processed/pMANOS_KIBs.R") # This script changed units: 
rm(kibs_og, kibsLOD, cor_13A_spear, cor_R3_spear, kibs_long_wide_13A, kibs_long_wide_13A_CorrVars,
   kibs_long_wide_R3, kibs_long_wide_R3_CorrVars, SumStats)

source("processed/pMANOS_eGFR.R")

rm(list = setdiff(ls(envir = .GlobalEnv), c("kibs_long", "kid", "R1_R3Q")))

# Cleaning ####
kid1 <- kid %>% 
  dplyr::select(-c(eGFRPre, eGFRPost, Postdiff, Prediff, #PreeGFR_t, PosteGFR_t, 
            everbelow60, roundbelow60, n.roundsbelow60, CKD, CKDround)) %>% 
  pivot_longer(cols = c("eGFRPre_RF", "eGFRPost_RF"), 
               names_to = "round.coded", 
               values_to = "eGFR_RF") %>%
  mutate(MANOS_ID = as.character(MANOS_ID)) %>%
  mutate(round.coded = case_when(
    round.coded == "eGFRPre_RF" & round == "1" ~ "13A",
    round.coded == "eGFRPost_RF" & round == "1" ~ "13D",
    round.coded == "eGFRPre_RF" & round == "2" ~ "2",
    round.coded == "eGFRPost_RF" & round == "2" ~ "2",
    round.coded == "eGFRPre_RF" & round == "3" ~ "3",
    round.coded == "eGFRPost_RF" & round == "3" ~ "3",
    round.coded == "eGFRPre_RF" & round == "4" ~ "4",
    round.coded == "eGFRPost_RF" & round == "4" ~ "4",
    round.coded == "eGFRPre_RF" & round == "5" ~ "5",
    round.coded == "eGFRPost_RF" & round == "5" ~ "5",
    round.coded == "eGFRPre_RF" & round == "6" ~ "6",
    round.coded == "eGFRPost_RF" & round == "6" ~ "6",
    TRUE ~ round.coded)) %>%
  drop_na(eGFR_RF) %>% 
  pivot_longer(cols = c("ScrPre", "ScrPost"), 
               names_to = "round.coded2", 
               values_to = "sCr") %>% 
  filter(!(round.coded == "13A" & round.coded2 == "ScrPost")) %>% 
  filter(!(round.coded == "13D" & round.coded2 == "ScrPre")) %>% 
  dplyr::select(-round.coded2) %>% 
  drop_na("sCr") %>% 
  distinct()



# Merging kid & KIBs ####
kid_kib <- full_join(kibs_long, kid1, by = c("MANOS_ID", "round.coded")) %>%
  dplyr::select(-c(round.y)) %>% 
  rename(round = round.x) %>% 
  group_by(MANOS_ID) %>% 
  fill(Country, Site, CKDu, agecat, Industry, .direction = "downup") %>% 
  mutate(age = ifelse(is.na(age) & round.coded == "11A", age[round.coded == "13A"], age)) %>%
  ungroup()



SumStats1 <- kid_kib%>%
  group_by(Biomarker, round.coded, kibDFlag) %>%
  summarise(n = n())


# Qaire ####


round.coded_df_R13 <- kid_kib %>% 
  select(MANOS_ID, round, round.coded) %>% 
  distinct() %>% 
  filter(round.coded %in% c("11A", "13A", "13D", "3")) %>% 
  mutate(round = case_when(round.coded == "11A" ~ "1", 
                           round.coded == "13A" ~ "1", 
                           round.coded == "13D" ~ "1", 
                           round.coded == "3" ~ "3"))


R1_R3Q <- R1_R3Q %>% 
  mutate(MANOS_ID = as.character(MANOS_ID), 
         YearsJob = as.numeric(YearsJob)) %>% 
  group_by(MANOS_ID) %>% 
  mutate(Age =            ifelse(is.na(Age) &            round == "3", Age[round == "1"]+1,            Age),
         YearsResidence = ifelse(is.na(YearsResidence) & round == "3", YearsResidence[round == "1"]+1, YearsResidence), 
         YearsDept =      ifelse(is.na(YearsDept) &      round == "3", YearsDept[round == "1"]+1,      YearsDept), 
         YearsIndustry =  ifelse(is.na(YearsIndustry) &  round == "3", YearsIndustry[round == "1"]+1,  YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round == "3", YearsWorkplace[round == "1"]+1, YearsWorkplace), 
         YearsJob =       ifelse(is.na(YearsJob) & round == "3", YearsJob[round == "1"]+1, YearsJob)) %>% 
  fill(CurrentJob, Industry, MonthsperYearjob, Alcoholcurrent, ChemHomefreq, 
       DWhomeSource, JobtaskCat1, JobtaskCat2, JobtaskCat3, JobtaskCat4, PestYN, 
       PPEYN, `B9. QAQCed Modified_B9 What is your level of education achieved?`, 
       starts_with("Index"), 
       .direction = "downup") %>% 
  ungroup() %>%
  select(-c(Totalnotwaterwork, Totalwaterwork, FamHxAnyCKD2, Dept3, Sites, Worksite,
            CurrentJob,
            starts_with("# de veces"), industry, starts_with("Freq"), 
            starts_with("Used"))) %>% 
  filter(!is.na(DWhome)) %>% 
  distinct() %>% 
  mutate(round = as.character(round)) %>% 
  ungroup() %>% 
  left_join(., round.coded_df_R13, by = c("MANOS_ID", "round"))


# kid_kib with qaire ####


kid_kib_q <- left_join(kid_kib, R1_R3Q, by = c("MANOS_ID", "round.coded", "round")) %>%
  select(-DWhome) %>%
  distinct() %>%
  rename(Country = Country.x, Industry = Industry.x) %>%
  dplyr::select(-Country.y, -Industry.y)


kid_kib_q <- kid_kib_q %>% 
  group_by(MANOS_ID) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "11A", YearsResidence[round.coded == "3"]-1,YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "11A",  YearsDept[round.coded == "3"]-1, YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "11A", YearsIndustry[round.coded == "3"]-1, YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "11A", YearsWorkplace[round.coded == "3"]-1, YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "11A", YearsJob[round.coded == "3"]-1, YearsJob)) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "13A", YearsResidence[round.coded == "3"]-1,YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "13A",  YearsDept[round.coded == "3"]-1, YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "13A", YearsIndustry[round.coded == "3"]-1, YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "13A", YearsWorkplace[round.coded == "3"]-1, YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "13A", YearsJob[round.coded == "3"]-1, YearsJob)) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "13D", YearsResidence[round.coded == "3"]-1,YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "13D",  YearsDept[round.coded == "3"]-1, YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "13D", YearsIndustry[round.coded == "3"]-1, YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "13D", YearsWorkplace[round.coded == "3"]-1, YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "13D", YearsJob[round.coded == "3"]-1, YearsJob)) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "2", YearsResidence[round.coded == "11A"],YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "2",  YearsDept[round.coded == "11A"], YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "2", YearsIndustry[round.coded == "11A"], YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "2", YearsWorkplace[round.coded == "11A"], YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "2", YearsJob[round.coded == "11A"], YearsJob)) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "4", YearsResidence[round.coded == "3"]+1,YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "4",  YearsDept[round.coded == "3"]+1, YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "4", YearsIndustry[round.coded == "3"]+1, YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "4", YearsWorkplace[round.coded == "3"]+1, YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "4", YearsJob[round.coded == "3"]+1, YearsJob)) %>% 
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "5", YearsResidence[round.coded == "4"]+1,YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "5",  YearsDept[round.coded == "4"]+1, YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "5", YearsIndustry[round.coded == "4"]+1, YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "5", YearsWorkplace[round.coded == "4"]+1, YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "5", YearsJob[round.coded == "4"]+1, YearsJob)) %>%
  mutate(YearsResidence = ifelse(is.na(YearsResidence) & round.coded == "6", YearsResidence[round.coded == "5"],YearsResidence), 
         YearsDept = ifelse(is.na(YearsDept) & round.coded == "6",  YearsDept[round.coded == "5"], YearsDept), 
         YearsIndustry = ifelse(is.na(YearsIndustry) & round.coded == "6", YearsIndustry[round.coded == "5"], YearsIndustry), 
         YearsWorkplace = ifelse(is.na(YearsWorkplace) & round.coded == "6", YearsWorkplace[round.coded == "5"], YearsWorkplace), 
         YearsJob = ifelse(is.na(YearsJob) & round.coded == "6", YearsJob[round.coded == "5"], YearsJob)) %>%
  fill(Country, .direction = "downup") %>% 
  fill(MonthsperYearjob, .direction = "downup") %>% 
  fill(Alcoholcurrent, .direction = "downup") %>% 
  fill(ChemHomefreq, .direction = "downup") %>% 
  fill(Industry, .direction = "downup") %>% 
  fill(DWhomeSource, .direction = "downup") %>% 
  fill(JobtaskCat1, .direction = "downup") %>% 
  fill(JobtaskCat2, .direction = "downup") %>% 
  fill(JobtaskCat3, .direction = "downup") %>% 
  fill(JobtaskCat4, .direction = "downup") %>%
  fill(PestYN, .direction = "downup") %>% 
  fill(PPEYN, .direction = "downup") %>% 
  ungroup() %>% 
  dplyr::select(-Age) 



# Creating stages ####


kid_kib_q <- kid_kib_q %>%
  group_by(MANOS_ID) %>%
  mutate(below60R1R2 = ifelse(any(round.coded == "13A" & eGFR_RF < 60) & any(round.coded == "2" & eGFR_RF < 60),TRUE, FALSE)) %>% 
  relocate(below60R1R2, .after = eGFR_RF) %>% 
  mutate(R1R2_stage = case_when(
    below60R1R2 == FALSE ~ "Stage 1/2/Normal",
    below60R1R2 == TRUE & any(round.coded == "2" & eGFR_RF > 45 & eGFR_RF <= 60) ~ "Stage 3a",
    below60R1R2 == TRUE & any(round.coded == "2" & eGFR_RF > 30 & eGFR_RF <= 45) ~ "Stage 3b",
    below60R1R2 == TRUE & any(round.coded == "2" & eGFR_RF > 15 & eGFR_RF <= 30) ~ "Stage 4",
    below60R1R2 == TRUE & any(round.coded == "2" & eGFR_RF <= 15) ~ "Stage 5",
    TRUE ~ "Other")) %>% 
  relocate(R1R2_stage, .after = below60R1R2)


# Creating average of 11A and 13A & an ACR variable ####
kid_kib_q <- kid_kib_q %>% 
  group_by(MANOS_ID, Biomarker) %>% 
  mutate(round = case_when(round.coded %in% c("11A", "13A", "13D") ~ "1",
                           round.coded == "2" ~ "2",
                           round.coded == "3" ~ "3",
                           round.coded == "4" ~ "4",
                           round.coded == "5" ~ "5",
                           round.coded == "6" ~ "6",
                           TRUE ~ as.character(round.coded))) %>%
  group_by(MANOS_ID, Biomarker, round) %>%
  mutate(KIBConc1.pg.mg_UCradj_R1_avged = ifelse(round == "1" & round.coded != "13D", 
    mean(KIBConc1.pg.mg_UCradj, na.rm = TRUE), KIBConc1.pg.mg_UCradj)) %>%
  mutate(ACR = Albumin / Creatinine) %>% 
  ungroup()


kid_kib_q <- kid_kib_q %>% 
  dplyr::select(-c(LLOD, ULOD)) %>% 
  relocate(KIBConc1.pg.mg_UCradj, KIBConc1.pg.mg_UCradj_R1_avged, .after = KIBConc) %>% 
  relocate(round.coded, .after = MANOS_ID)

