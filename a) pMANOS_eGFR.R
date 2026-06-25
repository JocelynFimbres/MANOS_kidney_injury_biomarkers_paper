# ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(readxl)
library(scales) 
library(moments)
library(janitor)
library(nephro)
library(readxl)
library(dplyr)

kid <- read_xlsx("../original/MANOS_eGFR_1.24.2022_Final.xlsx") %>% 
  dplyr::select(-`Baseline day Blood Sample Taken_Round 1_Pre`, -`Baseline day Blood Sample Taken_Round 1_Post`) %>% 
  rename(
    MANOS_ID = ID,
    R1_ScrPre = `Round 1 Day 3 Pre SCr`,
    R1_ScrPost = `Round 1 Day 3 Post SCr`,
    R2_ScrPre = `Round 2 SCr`,
    R3_ScrPre = `Round 3SCr`,
    R4_ScrPre = `Round 4 SCr`,
    R5_ScrPre = `Round 5 SCr`,
    R6_ScrPre = `Round 6 SCr`,
    R1_age = `Round 1 Age`,
    R2_age = `Round 2 Age`,
    R3_age = `Age Round 3`,
    R4_age = `Age Round 4`,
    R5_age = `Age Round 5`,
    R6_age = `Age Round 6`,
    R1_eGFRPre = `eGFR R1 Pre`,
    R1_eGFRPost = `eGFR R1 Post`,
    R2_eGFRPre = `eGFR R2`,
    R3_eGFRPre = `eGFR R3`,
    R4_eGFRPre = `eGFR R4`,
    R5_eGFRPre = `eGFR R5`,
    R6_eGFRPre = `eGFR R6`
  ) %>% 
  # Calculate CKDu before pivoting
  mutate(CKDu = ifelse(R1_eGFRPre < 60 & R2_eGFRPre < 60, 1, 0)) %>% #if CKD at baseline
  pivot_longer(cols = !c(MANOS_ID, Country, Site, Industry, CKDu), 
               names_to = c("round", ".value"), 
               names_sep = "_") %>% 
  group_by(MANOS_ID, round) %>% 
  mutate(eGFRPre_RF = CKDEpi_RF.creat(creatinine = ScrPre, sex = 1, age = age),
         eGFRPost_RF = CKDEpi_RF.creat(creatinine = ScrPost, sex = 1, age = age),
         Postdiff = (eGFRPost_RF - eGFRPost) / eGFRPost * 100,
         Prediff = (eGFRPre_RF - eGFRPre) / eGFRPre * 100) %>% 
  ungroup() %>% 
  mutate(agecat = case_when(age > 17 & age < 25 ~ "18-24",
                            age > 24 & age < 35 ~ "25-34",
                            age > 34 & age < 48 ~ "35-47")) %>% 
  group_by(round) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(`round` = case_when(
         `round` == "R1" ~ 1,
         `round` == "R2" ~ 2,
         `round` == "R3" ~ 3,
         `round` == "R4" ~ 4,
         `round` == "R5" ~ 5,
         `round` == "R6" ~ 6,    
         TRUE ~ as.numeric(`round`)),
    everbelow60 = case_when( #if eGFR ever <60
      eGFRPre < 60 & `round` == 1 ~ 1,
      eGFRPre < 60 & `round` == 2 ~ 1,
      eGFRPre < 60 & `round` == 3 ~ 1,
      eGFRPre < 60 & `round` == 4 ~ 1,
      eGFRPre < 60 & `round` == 5 ~ 1,
      eGFRPre < 60 & `round` == 6 ~ 1),
    roundbelow60 = case_when( #when eGFR <60
      eGFRPre < 60 & `round` == 1 ~ 1,
      eGFRPre < 60 & `round` == 2 ~ 2,
      eGFRPre < 60 & `round` == 3 ~ 3,
      eGFRPre < 60 & `round` == 4 ~ 4,
      eGFRPre < 60 & `round` == 5 ~ 5,
      eGFRPre < 60 & `round` == 6 ~ 6)) %>% 
  group_by(MANOS_ID) %>% 
  mutate(n.roundsbelow60 = sum(everbelow60, na.rm = TRUE), # how many eGFR were < 60
         CKD = ifelse(n.roundsbelow60 >= 2, 1, 0),    # if at least 2 eGFR < 60
         CKDround = ifelse(CKD == 1, sort(roundbelow60)[2], NA)) # find second lowest round when eGFR < 60


kid_condensed <- kid %>% 
  select(c(MANOS_ID, Country, Industry, Site, age, round, ScrPre, ScrPost, eGFRPre_RF, eGFRPost_RF))

kid_condensed_long <- kid_condensed %>% 
  pivot_longer(cols = c(eGFRPre_RF, eGFRPost_RF), 
               names_to = "round.coded2") %>% 
  filter(!is.na(value)) %>% 
  mutate(round.coded = case_when(round == 1 & round.coded2 == "eGFRPre_RF"  ~ "13A", 
                                 round == 1 & round.coded2 == "eGFRPost_RF" ~ "13D", 
                                 round == 2 ~ "2", 
                                 round == 3 ~ "3", 
                                 round == 4 ~ "4", 
                                 round == 5 ~ "5", 
                                 round == 6 ~ "6")) %>% 
  select(-round.coded2) %>% 
  relocate(round.coded, .after = round) %>% 
  rename(eGFR_RF = value)


kid_condensed_wide <- kid_condensed_long %>% 
  pivot_wider(names_from  = "round.coded", 
              values_from = "eGFR_RF") %>% 
  select(-round) %>% 
  group_by(MANOS_ID) %>% 
  fill("13A", .direction = "updown") %>% 
  fill("13D", .direction = "updown") %>% 
  fill("2", .direction = "updown") %>% 
  fill("3", .direction = "updown") %>% 
  fill("4", .direction = "updown") %>% 
  fill("5", .direction = "updown") %>% 
  fill("6", .direction = "updown") %>% 
  ungroup() %>% 
  select(-c(ScrPre, ScrPost, age)) %>% 
  distinct()

