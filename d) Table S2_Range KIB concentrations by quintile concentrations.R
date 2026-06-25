# Ranges of KIB concentrations by quintile ####
# Set-up ####
library(tidyverse)
library(glue)

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

source("Merged dfs/kid_kib.R")
rm(list = setdiff(ls(envir = .GlobalEnv), "kid_kib"))

# Create quintiles ####
df_13A <- kid_kib %>% 
  filter(round.coded %in% "13A", 
         !is.na(eGFR_RF), 
         !is.na(KIBConc1.pg.mg_UCradj)) %>% 
  distinct() %>% 
  group_by(Biomarker) %>% 
  mutate(kib_quintile = ntile(KIBConc1.pg.mg_UCradj, 5)) %>% 
  ungroup()

# Summary table ####
kib_quint_sum <- df_13A %>% 
  group_by(Biomarker, kib_quintile) %>% 
  summarise(n = n(), 
            range = glue("{round(min(KIBConc1.pg.mg_UCradj, na.rm = TRUE), 1)}, ",
                         "{round(max(KIBConc1.pg.mg_UCradj, na.rm = TRUE), 1)}"))
