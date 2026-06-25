# Processing KIB concentration files sent by JHU ####
# Set-up ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(readxl)

# Read in files #### 
kibsLOD <- read_excel("../original/MANOS compiled results.xlsx", sheet=2)
kibs <- read_excel("../original/MANOS compiled results.xlsx") 
kibs_long <- kibs %>% 
  mutate(MANOS_ID = str_extract(Sample, "-\\d{3}"))%>%
  mutate(MANOS_ID = gsub("-", "", MANOS_ID)) %>% 
  mutate(MANOS_ID = as.character(MANOS_ID)) %>% 
  mutate(round = case_when(
    str_detect(Sample, "13A") ~ "1",
    str_detect(Sample, "13D") ~ "1",
    str_detect(Sample, "11A") ~ "1",
    str_detect(Sample, "-3-") ~ "3",
    str_detect(Sample, "OF-606") ~ "3",
    TRUE ~ NA_character_),
    round.coded = case_when(
    str_detect(Sample, "13A") ~ "13A",
    str_detect(Sample, "13D") ~ "13D",
    str_detect(Sample, "11A") ~ "11A",
    str_detect(Sample, "-3-") ~ "3",
    str_detect(Sample, "OF-606") ~ "3",
    TRUE ~ NA_character_)) %>% 
  rename(Albumin = `Albumin (mg/dL)`,
         Creatinine = `Creatinine (mg/dL)`) %>% 
  mutate(NGAL = na_if(NGAL, "NA"),
         EGF = na_if(EGF, "NA"),
         `YKL-40` = na_if(`YKL-40`, "NA"),
         `IL-18` = na_if(`IL-18`, "NA"),
         `MCP-1` = na_if(`MCP-1`, "NA"),
         `KIM-1` = na_if(`KIM-1`, "NA")) %>%  
  mutate(NGAL = as.numeric(NGAL),
         EGF = as.numeric(EGF),
         `YKL-40` = as.numeric(`YKL-40`),
         `IL-18` = as.numeric(`IL-18`),
         `MCP-1` = as.numeric(`MCP-1`),
         `KIM-1` = as.numeric(`KIM-1`)) %>% 
  pivot_longer(cols = c(NGAL, EGF, `YKL-40`, `IL-18`, `KIM-1`, `MCP-1`),
               names_to = "Biomarker",
               values_to = "KIBConc") %>% 
  left_join(.,{kibsLOD},by="Biomarker") %>% 
  mutate(kibDFlag = case_when(is.na(KIBConc) ~ 0, # NAs to 0s, replacing w llod/sqrt(2)
                              KIBConc<LLOD&!is.na(KIBConc)~0.5, # Below lod but above loq. Not changing. 
                              KIBConc>LLOD&KIBConc<ULOD~1, # Detected value.
                              KIBConc>=ULOD~2)) %>%  # Above upper limit but detected. Not changing. 
  select(c(MANOS_ID, Biomarker, KIBConc, round, round.coded, Creatinine, Albumin, kibDFlag, LLOD, ULOD))
  
# Cleaning: non-detects, creatinine normalization, & units ####
kibs_long <- kibs_long %>% 
  mutate(KIBConc = round(KIBConc, digits = 2), 
         KIBConc1 = ifelse(kibDFlag == 0, LLOD/sqrt(2), KIBConc), 
         KIBConc1 = ifelse(kibDFlag == 0.5, LLOD/sqrt(2), KIBConc), 
         KIBConc1 = round(KIBConc1, digits = 2), 
         Creatinine.mg.dl = Creatinine / 100, # Creatinine as mg/ml (from mg/DL)
         KIBConc1.pg.mg_UCradj = KIBConc1 / Creatinine.mg.dl,  # KIBs as pg/mg
         KIBConc1.pg.mg_UCradj = round(KIBConc1.pg.mg_UCradj, digits = 2))

# LOD summary ####
kibs_long$kibDFlag[kibs_long$kibDFlag == 0]   <- "< LOD"
kibs_long$kibDFlag[kibs_long$kibDFlag == 1]   <- "> LOD"
kibs_long$kibDFlag[kibs_long$kibDFlag == 2]   <- "> ULOD"

SumStats <- kibs_long %>%
  group_by(Biomarker, round, round.coded, kibDFlag) %>%
  summarise(n = n())


