# ####
# Created: Mar 16, 2026
# By: JF
# Checking correlations across timepoints and between kibs

# Setting up  ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(corrplot)

source("processed/pMANOS_KIBs.R")

# Df manipulation ####
kib_wide <- kibs_long %>% 
  filter(!is.na(KIBConc1.pg.mg_UCradj)) %>% 
  pivot_wider(names_from = Biomarker, 
              values_from = KIBConc1.pg.mg_UCradj) %>% 
  select(c(MANOS_ID, round, round.coded, NGAL:`MCP-1`)) %>% # , sCr, eGFR_RF
  mutate(logNGAL = log(NGAL),
         logKIM = log(`KIM-1`),
         logYKL = log(`YKL-40`),
         logEGF = log(EGF),
         logIL18 = log(`IL-18`),
         logMCP = log(`MCP-1`))

kib_wide$round.coded[kib_wide$round.coded == "11A"] <- "Baseline Day 1 Pre-shift"
kib_wide$round.coded[kib_wide$round.coded == "13A"] <- "Baseline Day 3 Pre-shift"
kib_wide$round.coded[kib_wide$round.coded == "13D"] <- "Baseline Day 3 Post-shift"
kib_wide$round.coded[kib_wide$round.coded == "3"] <- "Visit 3 Pre-shift"

kib_wide <- kib_wide %>% 
  mutate(round.coded = factor(round.coded, levels = c("Baseline Day 1 Pre-shift",
                                                      "Baseline Day 3 Pre-shift",
                                                      "Baseline Day 3 Post-shift",
                                                      "Visit 3 Pre-shift")))

# Correlation between time at 6 KIBs (example: corr of NGAL between t1 and t2) ####
par(mfrow = c(2,3))

EGF_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, `EGF`) %>%
  filter(!is.na(`EGF`)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = `EGF`) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
EGF_corr <- EGF_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(EGF_corr,
         method = "color",
         title = "EGF",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)


IL_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, `IL-18`) %>%
  filter(!is.na(`IL-18`)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = `IL-18`) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
IL_corr <- IL_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(IL_corr,
         method = "color",
         title = "IL-18",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)


KIM_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, `KIM-1`) %>%
  filter(!is.na(`KIM-1`)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = `KIM-1`) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
KIM_corr <- KIM_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(KIM_corr,
         method = "color",
         title = "KIM-1",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)


MCP_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, `MCP-1`) %>% 
  filter(!is.na(`MCP-1`)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = `MCP-1`) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
MCP_corr <- MCP_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(MCP_corr,
         method = "color",
         title = "MCP-1",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)


NGAL_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, NGAL) %>%
  filter(!is.na(NGAL)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = NGAL) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
NGAL_corr <- NGAL_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(NGAL_corr,
         method = "color",
         title = "NGAL",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)


YKL_wide <- kib_wide %>%
  select(MANOS_ID, round.coded, `YKL-40`) %>%
  filter(!is.na(`YKL-40`)) %>% 
  distinct() %>% 
  pivot_wider(names_from = round.coded,
              values_from = `YKL-40`) %>% 
  relocate("Baseline Day 1 Pre-shift",
           "Baseline Day 3 Pre-shift",
           "Baseline Day 3 Post-shift",
           .before = "Visit 3 Pre-shift")
YKL_corr <- YKL_wide %>%
  select(-MANOS_ID) %>%
  cor(use = "complete.obs", method = "pearson")
corrplot(YKL_corr,
         method = "color",
         title = "YKL-40",
         mar = c(0,0,2,0),
         type = "upper",
         tl.cex = 1.3,
         tl.srt = 30,
         tl.col = "black",
         addCoef.col = "black",
         number.digits = 2)



pearson_corrs <- rbind(as.data.frame(EGF_corr) %>% mutate(Biomarker = "EGF"), 
                       as.data.frame(IL_corr) %>% mutate(Biomarker = "IL-18"), 
                       as.data.frame(KIM_corr) %>% mutate(Biomarker = "KIM-1"), 
                       as.data.frame(MCP_corr) %>% mutate(Biomarker = "MCP-1"), 
                       as.data.frame(NGAL_corr) %>% mutate(Biomarker = "NGAL"), 
                       as.data.frame(YKL_corr) %>% mutate(Biomarker = "YKL-40"))

# Separating timepoints (for between KIBs corr) ####


kib_wide_R11A <- kib_wide %>% 
  filter(round.coded %in% "Baseline Day 1 Pre-shift")

kib_wide_R13A <- kib_wide %>% 
  filter(round.coded %in% "Baseline Day 3 Pre-shift")

kib_wide_R13D <- kib_wide %>% 
  filter(round.coded %in% "Baseline Day 3 Post-shift")

kib_wide_R3 <- kib_wide %>% 
  filter(round.coded %in% "Visit 3 Pre-shift")


# Correlation between KIBs at 4 timepoints (corr of NGAL and EGF at t=3) ####
par(mfrow = c(2,2))

R11A_CorrVars <- kib_wide_R11A %>%
  select(MANOS_ID, 
         EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`, 
         logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL) %>%
  group_by(MANOS_ID) %>%
  fill(NGAL, `KIM-1`, `MCP-1`, EGF, `YKL-40`, `IL-18`, 
       logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL,
       .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  select(EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`)
R11A_pearson_corr <- cor(R11A_CorrVars,
                         method = "pearson",
                         use = "complete.obs")
cor1 <- corrplot(R11A_pearson_corr,
                 method = "color",
                 type = "upper",
                 tl.cex = 0.9,
                 tl.col = "black",
                 title = "Baseline Day 1 Pre-shift",
                 mar = c(0,0,2,0),
                 tl.srt = 45,
                 addCoef.col = "black",
                 number.digits = 2)



R13A_CorrVars <- kib_wide_R13A %>%
  select(MANOS_ID, 
         EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`, 
         logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL) %>%
  group_by(MANOS_ID) %>%
  fill(NGAL, `KIM-1`, `MCP-1`, EGF, `YKL-40`, `IL-18`, 
       logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL,
       .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  select(EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`)#, eGFR_RF, sCr) #logNGAL, logKIM, logMCP, logEGF, logYKL, logIL18) 
R13A_pearson_corr <- cor(R13A_CorrVars,
                         method = "pearson",
                         use = "complete.obs")
cor2 <- corrplot(R13A_pearson_corr,
                 method = "color",
                 type = "upper",
                 title = "Baseline Day 3 Pre-shift",
                 mar = c(0,0,2,0),
                 tl.cex = 0.9,
                 tl.col = "black",
                 tl.srt = 45,
                 addCoef.col = "black",
                 number.digits = 2)



R13D_CorrVars <- kib_wide_R13D %>%
  select(MANOS_ID, 
         EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`, 
         logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL) %>%
  group_by(MANOS_ID) %>%
  fill(NGAL, `KIM-1`, `MCP-1`, EGF, `YKL-40`, `IL-18`, 
       logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL,
       .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  select(EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`)#, eGFR_RF, sCr) #logNGAL, logKIM, logMCP, logEGF, logYKL, logIL18) 
R13D_pearson_corr <- cor(R13D_CorrVars,
                         method = "pearson",
                         use = "complete.obs")
cor3 <- corrplot(R13D_pearson_corr,
                 method = "color",
                 type = "upper",
                 title = "Baseline Day 3 Post-shift",
                 mar = c(0,0,2,0),
                 tl.cex = 0.9,
                 tl.col = "black",
                 tl.srt = 45,
                 addCoef.col = "black",
                 number.digits = 2)



R3_CorrVars <- kib_wide_R3 %>%
  select(MANOS_ID, 
         EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`, 
         logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL) %>%
  group_by(MANOS_ID) %>%
  fill(NGAL, `KIM-1`, `MCP-1`, EGF, `YKL-40`, `IL-18`, 
       logEGF, logIL18, logKIM, logMCP, logNGAL, logYKL,
       .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  select(EGF, `IL-18`, `KIM-1`, `MCP-1`, NGAL, `YKL-40`)#, eGFR_RF, sCr) #logNGAL, logKIM, logMCP, logEGF, logYKL, logIL18) 
R3_pearson_corr <- cor(R3_CorrVars,
                       method = "pearson",
                       use = "complete.obs")
cor4 <- corrplot(R3_pearson_corr,
                 method = "color",
                 type = "upper",
                 title = "Visit 3 Pre-shift",
                 mar = c(0,0,2,0),
                 tl.cex = 0.9,
                 tl.col = "black",
                 tl.srt = 45,
                 addCoef.col = "black",
                 number.digits = 2)
