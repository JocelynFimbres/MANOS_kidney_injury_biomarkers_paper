# Cleaning questionnaire data ####
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
setwd("..") 
getwd()

library(tidyverse)
library(readxl)

#### read in Round 1, day 0 qaire ####
R1_D0Q_og<-read_excel("../original/Day_0Q_R1_Final_Labels_01.14.2022.xlsx")
R1_D0Q<-R1_D0Q_og %>% 
  rename(MANOS_ID=`ID Unico del Participante / Codigo de participante`,
         Country=`Pais de Entrevista`,
         
         # C1a residential department
         IndustryN=`Industria...6`,
         IndustryES=`Industria...7`,
         WorksiteAZ1_N=`Nombre del Sitio de Trabajo:...8`,
         WorksiteAZ2_N=`especifique otro sitio de trabajo:...9`,
         WorksiteLA_N=`Nombre del Sitio de Trabajo:...10`,
         WorksitePL1_N=`Nombre del Sitio de Trabajo:...11`,
         WorksitePL2_N=`especifique otro sitio de trabajo:...12`,
         WorksiteAZ1_ES=`Nombre del Sitio de Trabajo:...13`,
         WorksiteAZ2_ES=`especifique otro sitio de trabajo:...14`,
         WorksiteMA1_ES=`Nombre del Sitio de Trabajo:...15`,
         WorksiteMA2_ES=`especifique otro sitio de trabajo:...16`,
         WorksiteCO1_ES=`Nombre del Sitio de Trabajo:...17`,
         WorksiteCO2_ES=`especifique otro sitio de trabajo:...18`,
         
         #Age
         Age=`B4. ¿Cuál es su edad actual (en años cumplidos)?`,
         
         # Department
         Dept=`C1a. ¿En que municipio/departamento vive usted actualmente?`,
         YearsResidence=`C2. ¿Cuántos años ha vivido used en su residencia actual?`,
         YearsDept=`C3. ¿Cuántos años ha vivido usted en su departamento que ya nos refirió?`,
         
         # C4 frequncy apply agrichems outside of work
         ApplyagchemHome=`C4. ¿Con que frecuencia usted personalmente aplica agroquimicos (para eliminar malezas, roedores, insectos, y/o hongos) fuera de su lugar de trabajo?`,
         AgChemHomeFreqOther=`si otro, porfavor especifique:`,
         
         # C4b which ones? Gramozone, (2,4D), (cypermethrin), (chlorpyriphos (lorsban)), gespax, (diazinon), (glyphosate), other. () means we have biomonitoring data
         Gramoxone=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Gramoxone)`,
         two4D=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=2,4 D)`,
         Cypermethrin=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Cipermetrina)`,
         Chlorpyrifos=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Clorpirifos (Lorsban))`,
         Gespx=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Gesapax)`,
         Diazinon=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Diazinon)`,
         Glyphosate=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Glifosato (nombre de marca: Round Up))`,
         Other=`C4b. ¿Cuál de los siguientes químicos ha aplicado usted? (choice=Otro)`,
         
         # C5 primary source of DW at home 
         DWhomemuni=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Agua del municipio, entubada y llevada dentro de la casa)`,
         DWhomemunicommon=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Agua del municipio proveida desde una fuente cercana, usada por muchos)`,
         DWhomeprivatewell=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Pozos privados en la propiedas)`,
         DWhomecommonwell=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Pozos comunitarios, usados por muchos)`,
         DWhomeriver=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Agua de una quebrada, rio o poza)`,
         DWhomeother=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=Otro)`,
         DWhomeDK=`C5. ¿Cuál es la fuente primaria para proveer agua de tomar para su casa/residencia? (choice=No se)`,
         
         # C6-source of water consumption at work
         DWwork=`C6. ¿Cuál es la fuente de consume de agua en el trabajo?`, #open response
         
         #D1 - work history
         YearsIndustry=`D1. ¿Cuántos años has trabajado en la industria (caña, maiz, etc.) actual?`,
         YearsIndustryNic=`D1. Nicaragua Total Years Worked in Industry R1 (QA/QCed)`,
         #YearsIndustryNicConf=`D1. Confidence in accuracy of 'Nicaragua Total Years Worked in Industry R1' (d1_yearswork_nic_total_qaqc)(1 = high, 2 = medium, 3 = low)`,
         YearsWorkplace=`D2. ¿Cuántos de esos años ha trabajado usted para su empleador actual (donde el estudio de hoy esta tomando lugar)?`,
        
        # D3-current job title?
         CurrentJob=`D3. Current Job in Spanish (QAQC'd added)`,
         JobtitleCat1=`D3. Category 1 Current Job (QAQC'd added)`,
         JobtitleCat2=`D3. Category 2 Current Job (QAQC'd added)`,
         JobtitleCat3=`D3. Category 3 Current Job (QAQC'd added)`,
         JobtitleCat4=`D3. Category 4 Current Job (QAQC'd added)`,
         
         #D5 Years worked in current job at baseline
         YearsJob=`D5. Years worked in the current job at time of baseline D0 Questionnaire_ QAQC'd`,
         
         #months per year in current job
         MonthsperYearjob=`D5a. ¿Cuántos meses por año trabaja usted en su trabajo actual?`,
         
         # F- what container drink water from at work? Gallon, 2L, 1.5L, 1L, 600mL, less 300mL, other
         DWworkcontainerG=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=A. Galon)`,
         DWworkcontainer2L=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=B. 2L botella)`,
         DWworkcontainer1.5L=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=C. 1.5L botella)`,
         DWworkcontainer1L=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=D. 1L botella)`,
         DWworkcontainer600mL=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=E. 600mL botella)`,
         DWworkcontainer300mL=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=F. < 300 mL botella, vaso, o tasa)`,
         DWworkcontainerother=`Marque el tipo de contenedor(es) del cual tomas mas frecuentemente Agua durante la jornada de trabajo: (choice=G. Otro)`,
         
        # F1- how many containers drink while working each day?
         DrinkGallonswork=`F1a. ¿Cuantos galones de agua (A) usted típicamente toma durante cada día de trabajo?`,
         Drink2Lwork=`F1b. ¿Cuantas 2L botellas de agua (B) usted típicamente toma durante cada día de trabajo?`,
         Drink1.5Lwork=`F1c. ¿Cuantas 1.5 L botellas de agua (C) usted típicamente toma durante cada día de trabajo?`,
         Drink1Lwork=`F1d. ¿Cuantas 1L botellas de agua (D) usted típicamente toma durante cada día de trabajo?`,
         Drink600mLwork=`F1e. ¿Cuantas 600 mL botellas de agua (E) usted típicamente toma durante cada día de trabajo?`,
         Drink300mLwork=`F1f. ¿Cuantas 300mL botella, vaso, o tasas de agua (F) usted típicamente toma durante cada día de trabajo?`,
         DWcontainerothertype=`especifique que otro tipo de contenedor de agua (G):`, #IS THIS A LITER MEASURE?
         Drinkotherwork=`F1g. ¿Cuantos otros contenedores de agua (G) usted típicamente toma durante cada día de trabajo?`,
         
         # G1- drink alcohol currently? In the past?
         Alcoholcurrent=`G1. ¿Actualmente usted toma bebidas alcohólicas?`,
         Alcoholever=`G1a. ¿Usted tomó alguna vez en su vida alcohol?`,
         
         # L1 - currently smoke cigs? Ever regularly?
         Smokecurrent=`L1. ¿Actualmente usted fuma cigarrillos?`,
         Smokeever=`L1a. ¿Usted alguna vez fumó cigarrillos regularmente?`,
         
         # M4- family member CKD?
         FamHxAnyCKD=`Index of CKD Status for Immediate Family. Created by Zoe Petropoulos. for dissertation. Added.`) %>% 
  mutate(MANOS_ID=as.numeric(MANOS_ID),
         YearsIndustry=ifelse(Country=="Nicaragua",YearsIndustryNic,YearsIndustry),
         round=1) %>% 
  mutate(Dept1=case_when(Dept=="ACAJUTLA"~"Ahuachapan",
                         Dept=="aguachapan aguachpan"~"Ahuachapan",
                         Dept=="AHUACHAPAN"~"Ahuachapan",
                         Dept=="ahuachapan"~"Ahuachapan",
                         Dept=="Ahuachapán"~"Ahuachapan",
                         Dept=="Ahuachapa"~"Ahuachapan",
                         Dept=="Acajutla"~"Ahuachapan",
                         Dept=="JUJUTLA"~"Ahuachapan",
                         Dept=="Ahuachapan"~"Ahuachapan",
                         Dept=="cabañas"~"Cabanas",
                         Dept=="Chichigalpa"~"Chinandega",
                         grepl("dega",Dept,ignore.case=TRUE)~"Chinandega",
                         Dept=="Quezaltepeque Cuzcatlan"~"Cuscatlan",
                         grepl("liber",Dept,ignore.case=TRUE)~"LaLibertad",
                         Dept=="santa tecla"~"LaLibertad",
                         grepl("Le?n",Dept)~"Leon",
                         grepl("León",Dept,ignore.case = TRUE)~"Leon",
                         grepl("Managua",Dept,ignore.case=TRUE)~"Managua",
                         Dept=="CIUDAD DELGADO"~"SanSalvador",
                         Dept=="tonacatepeque"~"SanSalvador",
                         grepl("salvador",Dept,ignore.case = TRUE)~"SanSalvador",
                         Dept=="santa ana"~"SantaAna",
                         Dept=="IZALCO"~"Sonsonate",
                         grepl("sons",Dept,ignore.case=TRUE)~"Sonsonate",
                         Dept=="LA POZA"~"Usulutan",
                         grepl("jiquil",Dept,ignore.case=TRUE)~"Usulutan",
                         grepl("usu",Dept,ignore.case=TRUE)~"Usulutan")) %>% 
                         #TRUE~as.character(Dept)))
  mutate(ChemHomefreq=case_when(ApplyagchemHome=="Diariamente"~365, # this is done for a "yearly" frequency because of open responses
                                ApplyagchemHome=="Mensualmente"~12, 
                                ApplyagchemHome=="Nunca"~0, 
                                ApplyagchemHome=="Semanalmente"~52, 
                                # ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="NA"~NA,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CADA 6 MESES."~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada año"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="solo cuando hay milpa"~1, #"only when there is harvest", could =NA
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="4 VECES AL AÑO"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="6 MESES DE INVIERNO"~6, # 6 months of winter
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 VECES POR SEMANA"~104, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="invierno"~1, # winter
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CUANDO HACE MILPA"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="anualmente"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cultivo"~1,  
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="INVIERNO"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="UN MES AL AñO"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 MES AL AñO"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="SOLO EN INVIERNO"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="EN INVIERNO"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="TIEMPO DE COSECHA"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CADA 15 DIAS"~26, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Cada 15 Dias" ~26,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 días por semana solo en invierno"~36, # 3winter months*4weeks/month*3days/week
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Epoca de lluvia"~6, # 6 rainy months
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Cada 6 meses"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="año"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Invierno /Cañales"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Invierno" ~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Cada 15 días"~26,  
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Verano"~6, # 6 summer months
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 veces por año"~3,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Temporada"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada 3 meses"~4,  
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="6meses" ~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="6 meses" ~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada 6 meses"~2,   
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 veces al año"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada 2 meses"~6, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="En invierno, 3 meses seguidos" ~3, # "in the winter, 3 consecutive months
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Temportada"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="al año"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Temporada en invierno" ~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Cada año q vez por semana durante un mes"~5, # "once a year and every week for a month
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 veces al año"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="Solo en invierno"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CADA 3 MESES"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CADA 6 MESES"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="TEMPORADA"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="CADA AÑO"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="participant selected Never but specified in C4a and C4b that they did use agrochemicals so we changed C4 to indicate Other"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="AL AÑO"  ~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="4 ahos"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="primera temporada"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="temporada" ~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="C/6 meses"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 VECES AL AÑO"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="46 MESES"~54, # interpreting as 4-6 times a month = 36-72x/year -> averaging=
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="solo 5 veces en todo el invierno"~5, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="hace 3 años"~1, #" 3 years ago
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 meses en invierno" ~2,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="solo en invierno" ~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 cada 7 meses" ~6, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 veces durante los 6 meses de invierno"~3, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="finca agrícola, solo en invierno (1 vez al año)"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 vez / año para cultivo propio"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="trimestre (4 por año)"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada 6 meses, en invierno 6 veces"~12, # "every 6 months and in the winter 6 times"
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 vez al año"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="3 meses  al año, 3 aplicaciones"~3, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 ocasiones durante el invierno"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 vez cada 3 meses"~4, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="quincenalmente"~26,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="anual"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="anual, 2 veces"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="5 veces al año"~5,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="solo 1 vez al año"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="20 veces al año"~20, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 vez en cada invierno"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="cada 3 días"~121, # "every 3 days"
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="una vez"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 veces por semana"~104, # twice a week
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="1 sola vez"~1,
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="semestralmente"~2, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="una vez al año"~1, 
                                ApplyagchemHome=="Otro"&AgChemHomeFreqOther=="2 veces cada 6 meses"~4))
  




# Selecting qaire variables ####
# Keeping all NSAID related variables, to. 
R1_D0Qlong <- R1_D0Q %>% 
  dplyr::select(MANOS_ID, round, Country, IndustryN, IndustryES, WorksiteAZ1_N, WorksiteAZ2_N, WorksiteLA_N,
         WorksitePL1_N, WorksitePL2_N, WorksiteAZ1_ES, WorksiteAZ2_ES, WorksiteMA1_ES, WorksiteMA2_ES,
         WorksiteCO1_ES, WorksiteCO2_ES, Age, Dept1, YearsResidence, YearsDept, DWhomemuni,
         DWhomemunicommon, DWhomeprivatewell, DWhomecommonwell, DWhomeriver, DWhomeother, DWhomeDK,
         YearsIndustry, YearsWorkplace, CurrentJob, YearsJob, MonthsperYearjob, Alcoholcurrent, FamHxAnyCKD, 
         ChemHomefreq, `B9. QAQCed Modified_B9 What is your level of education achieved?`, 
         `Index of CKD Status for all Brothers (sum). Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for Father and Brother. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for Mother. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for Uncle. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for Sister. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for Cousin. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `Index of CKD Status for All Family. Created by Zoe Petropoulos. for dissertation. Added.`, 
         `# de veces suplementos de potasio usado en la última semana:`,
         `c. AINES/ analgésicos: ibuprofen (dorival), dipirona (dolofor), naproxen (aleve), metamizole (novalgina), meloxicam, indometacina (motrin), diclofenac (voltaren), ketoprofen (oruvail)`, 
         `# de veces ibuprofen (dorival) usados en la última semana:`, 
         `# de veces dipirona (dolofor) usados en la última semana:`, 
         `# de veces naproxen (aleve) usados en la última semana:`,
         `# de veces metamizole (novalgina) usados en la última semana:`,
         `# de veces meloxicam usados en la última semana:`,
         `# de veces indometacina (motrin) usados en la última semana:`,
         `# de veces diclofenac (voltaren) usados en la última semana:`,
         `# de veces ketoprofen (oruvail) usados en la última semana:`, 
         `# de veces otro usados en la última semana:`, 
         `# de veces AINES (tipo no especificado) usados en la última semana:`,
         `# de veces acetaminofén usado en la última semana:`,
         `# de veces aspirina usada en la última semana:`,
         `# de veces opioides usados en la última semana:`,
          `# de veces aminoglicósidos usados en la última semana:`,
         `# de veces diuréticos usados en la última semana:`) %>% 
  unique() %>% 
  mutate(Industry = ifelse(Country == "Nicaragua", IndustryN,
                           ifelse(Country == "El Salvador", IndustryES, NA))) %>% 
  dplyr::select(-IndustryN, -IndustryES) %>% 
  pivot_longer(cols = c(WorksiteAZ1_N:WorksiteCO2_ES), names_to = "Sites", values_to = "Worksite") %>%  
  filter(!is.na(Worksite)) %>% 
  filter(Worksite != "Otro") %>% 
  filter(Worksite != "otro") %>% 
  pivot_longer(cols = c("DWhomemuni", "DWhomemunicommon", "DWhomeprivatewell", 
                        "DWhomecommonwell", "DWhomeriver", "DWhomeother", "DWhomeDK"),
               names_to = "DWhome", values_to = "DWhomeSource") %>% 
  filter(DWhomeSource == "Checked")

#### read in Round 1, day 3 post shift qaire ####                           
R1_PSQ_og <- read_excel("../original/PSQ_R1_Final_Labels_01.14.2022.xlsx") 
R1_PSQ <- R1_PSQ_og %>% 
  rename(MANOS_ID=`ID Unico del Participante / Codigo de participante`,
         ObservationDay=`Event Name`,
         JobtaskCat1=`C1. Days 1-3 Job Task. Category 1: English Translation of originally reported Job Task. QA/QC'ed for correctness`,
         JobtaskCat2=`C1. Days 1-3 Job Task. Category 2. QA/QCed`,
         JobtaskCat3=`C1. Days 1-3 Job Task. Category 3. QA/QCed`,
         JobtaskCat4=`C1. Days 1-3 Job Task. Category 4. QA/QCed`,
         PestYN=`C1. Exposed to agrichemicals? (Pesticides or Fertilizers) Based on Day 1-3 Reported Job Task`,
         NicSugCoveralls=`During the work shift today, did the participant wear coveralls? Estimated based on team reporting. FOR NICARAGUAN SUGAR CANE WORKERS ONLY. Created by SK for PSQ dataset.`,
         ProtectLeg=`b. Protector de piernas`,
         Guantes=`d. Guantes plásticos o impermeables`,
         Mangas=`e. Mangas largas`,
         ProtectArm=`f. Protector de brazos`,
         ProtectEyes=`g. Anteojos de seguridad ó protectores para los ojos`,
         Mask=`m. Mascarillas para protección respiratoria`,
         Totalwaterwork=`Total amount of Water consumed during the work day (in L).  Created by Zoe Petropoulos. for dissertation. Added.`,
         Totalnotwaterwork=`SUMMARY VARIABLE Total amount of Non-Water Bevereages consumed during the work day (in L). Combines multiple reported beverages. Created by Zoe Petropoulos. for dissertation. Added.`) %>% 
  mutate(Day=str_extract(`ID_Day Created by SK to make unique row identifier for PSQ dataset`,"_\\d")) %>% 
  mutate(Day=gsub("_","",Day)) %>% 
  filter(Day=="3") %>% 
  mutate(PPEYN=ifelse(NicSugCoveralls=="Si"|ProtectLeg=="Si"|Guantes=="Si"|Mangas=="Si"|
                        ProtectArm=="Si"|ProtectEyes=="Si"|Mask=="Si",1,0),
         PestYN=ifelse(PestYN=="Yes",1,
                       ifelse(PestYN=="No",0,NA)),
         PestYN=as.numeric(PestYN)) %>% 
  dplyr::select(MANOS_ID,JobtaskCat1,JobtaskCat2,JobtaskCat3,JobtaskCat4,PestYN,PPEYN,Totalwaterwork,Totalnotwaterwork) %>% 
  unique()
# C1- job title today
# C2- list of job tasks today
# C3- same task today as yesterday?
# C5- today typical this week or last?
# C7- hydration today similar to this week or last?
# C8- length/intensity today similar to this week or last?
# C9- take breaks during workday today?
# D1- what container drink from today? Gallon, 2L, 1.5L, 1L, 600mL, less 300mL, other
# D1- how many containers typically drink?
# D2- drink any other liquid than water today? What kind/how much?
# F2- any medications today?
# G1- PPE use today?
# G2- PPE use this or last week?

#### merge Round 1 day 0 and Round 1,day3 post shift q-aire ####
R1_D0_D3<-left_join(R1_D0Qlong,R1_PSQ, by="MANOS_ID")
  

#### read in Round 3, preshift qaire ####
R3PSQ<-read_excel("../original/Copy of R3Q_Labels_Final_01.14.2022.xlsx") %>% 
  rename(MANOS_ID=`ID Unico del Participante / Codigo de participante`,
         Age=`B2.a. ¨Cu ntos a¤os cumplidos tiene?`,
         Country=`Pais de Entrevista`,
         IndustryN=`Industria...13`,
         IndustryES=`Industria...14`,
         WorksiteAZ1_N=`Nombre del Sitio de Trabajo:...21`,
         WorksiteAZ2_N=`especifique otro sitio de trabajo:...22`,
         WorksiteLA_N=`Nombre del Sitio de Trabajo:...23`,
         WorksitePL1_N=`Nombre del Sitio de Trabajo:...24`,
         WorksitePL2_N=`especifique otro sitio de trabajo:...25`,
         WorksiteAZ1_ES=`Nombre del Sitio de Trabajo:...26`,
         WorksiteAZ2_ES=`especifique otro sitio de trabajo:...27`,
         WorksiteMA1_ES=`Nombre del Sitio de Trabajo:...28`,
         WorksiteMA2_ES=`especifique otro sitio de trabajo:...29`,
         WorksiteCO1_ES=`Nombre del Sitio de Trabajo:...30`,
         WorksiteCO2_ES=`especifique otro sitio de trabajo:...31`,
         
         #Have you moved since last visit?
         Moved=`C1.  ¨Ha cambiado su direcci¢n desde nuestra £ltima visita?`, #after rbind, if no, then Dept=Dept[round==1]
         # C1a new residential department
         Dept=`C1a. Nuevo departamento`,
         
         # # C7 primary source of DW at home 
         DWhomeNic=`C7. ¨Cu l es la fuente primaria del agua de beber en su casa?...56`,
         DWhomeES=`C7. ¨Cu l es la fuente primaria del agua de beber en su casa?...57`,
        
         
         # # E- what container drink water from? Gallon, 2L, 1.5L, 1L, 600mL, less 300mL, other
         DWworkcontainerG=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=A. Galon)`,
         DWworkcontainer2L=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=B. 2L botella)`,
         DWworkcontainer1.5L=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=C. 1.5L botella)`,
         DWworkcontainer1L=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=D. 1L botella)`,
         DWworkcontainer600mL=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=E. 600mL botella)`,
         DWworkcontainer300mL=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=F. < 300 mL botella, vaso, o tasa)`,
         DrinkothervolL=`¨De qu‚ cantidad de liquido es su contenedor G para E1a Agua que toma en el trabajo)? Por favor especifica en LITROS. 500mL = .5 Litros. (Trabajo por pago)`,
         DWworkcontainerother=`E. Marque el contenedor que el participante usa con m s frecuencia en un d¡a de trabajo. Si toma de diferentes contendores, marque seg£n sea apropiado. Si el participante dice que ‚l bebe de otro tipo de contenedor, escribe en la letra 'G' el tama¤o del contenedor. (Trabajo por pago) (choice=G. Otro)`,
        
         # # - how many containers drink while working each day?
         DrinkGallonswork=`E1a. ¨Cuantos galones de agua (A) usted t¡picamente toma durante cada d¡a de trabajo? (Trabajo por pago)`,
         Drink2Lwork=`E1b. ¨Cuantas 2L botellas de agua (B) usted t¡picamente toma durante cada d¡a de trabajo? (Trabajo por pago)`,
         Drink1.5Lwork=`E1c. ¨Cuantas 1.5 L botellas de agua (C) usted t¡picamente toma durante cada d¡a de trabajo? (Trabajo por pago)`,
         Drink1Lwork=`E1d. ¨Cuantas 1L botellas de agua (D) usted t¡picamente toma durante cada d¡a de trabajo? (Trabajo por pago)`,
         Drink600mLwork=`E1e. ¨Cuantas 600 mL botellas de agua (E) usted t¡picamente toma durante cada d¡a de trabajo?  (Trabajo por pago)`,
         Drink300mLwork=`E1f. ¨Cuantas 300mL botella, vaso, o tasas de agua (F) usted t¡picamente toma durante cada d¡a de trabajo?  (Trabajo por pago)`,
         DWcontainerothertype=`especifique que otro tipo de contenedor de agua (G). (Trabajo por pago)`,
         Drinkotherwork=`E1g. ¨Cuantos otros contenedores de agua (G) usted t¡picamente toma durante cada d¡a de trabajo?  (Trabajo por pago)`,
         # Drink non-water?
         Bolis=`1. Bolis...114`,
         MineralWater=`2. Agua Mineral...115`,
         BottleJuice=`3. Jugo embotellado`,
         NaturalJuice=`4. Jugo Natural`,
         Soda=`5. Soda - cola, Fanta Roja, naranja, uva, fresca...118`,
         Milk=`6. Leche...119`,
         EnergyDrink=`7. Bebidas energ‚ticas con cafe¡na`,
         Suero=`8. Soluciones Electrol¡ticas (suero oral)...121`,
         HerbalTea=`9. T‚ herbal...122`,
         Coffee=`10. Caf‚/t‚ negro o verde...123`,
         Other=`11. Otro (cacao, pinolillo, etc.): por favor especificar...124`,
         
         # How much non-water?
         QuantBolis=`¨Cuantos bolis?...125`,
         QuantMineralWater=`¨Cuantos contenedores de agua mineral toma?`,
         SizeMineralWater=`¨De que tama¤o? Etiqueta:__________...127`,
         QuantBottleJuice=`¨Cuantos contenedores de jugo embotellado toma?`,
         SizeBottleJuice=`¨De que tama¤o? Etiqueta:__________...129`,
         QuantNaturalJuice=`¨Cuantos contenedores de jugo natural toma?`,
         SizeNaturalJuice=`¨De que tama¤o? Etiqueta:__________...131`,
         QuantSoda=`¨Cuantos contenedores de Soda - cola, Fanta Roja, naranja, uva, fresca, toma?`,
         SizeSoda=`¨De que tama¤o? Etiqueta:__________...133`,
         QuantMilk=`¨Cuantos contenedores de leche toma?`,
         SizeMilk=`¨De que tama¤o? Etiqueta:__________...135`,
         QuantEnergyDrink=`¨Cuantos contenedores de bebidas energ‚ticas toma?`,
         SizeEnergyDrink=`¨De que tama¤o? Etiqueta:__________...137`,
         QuantSuero=`¨Cuantos contenedores de soluciones electroliticas (suero oral) toma?`,
         SizeSuero=`¨De que tama¤o? Etiqueta:__________...139`,
         QuantHerbalTea=`¨Cuantos contenedores de te herbal toma?`,
         SizeHerbalTea=`¨De que tama¤o? Etiqueta:__________...141`,
         QuantCoffee=`¨Cuantos contenedores de caf‚/t‚ negro o verde toma?`,
         SizeCoffee=`¨De que tama¤o? Etiqueta:__________...143`,
         WhatOtherLiquids=`Especifique cuales otros liquidos toma:`,
         QuantOther=`¨Cuantos contenedores de [otro] toma?...145`,
         SizeOther=`¨De que tama¤o? Etiqueta:__________...146`,
         
         #F- chemicals
          Usedpesticides=`F1. ¨Desde nuestra £ltima visita de toma de muestra ha usado qu¡micos para matar malezas, roedores, insectos y / o hongos en el trabajo o fuera del trabajo?`, #at work or outside of work
          FreqPestUseWork_everymonth=`F3. Desde nuestra £ltima visita, ¨con qu‚ frecuencia, ha usado estos qu¡micos en el TRABAJO? (choice=Cada mes)`,
          FreqPestUseWork_everyweek=`F3. Desde nuestra £ltima visita, ¨con qu‚ frecuencia, ha usado estos qu¡micos en el TRABAJO? (choice=Cada semana)`,
          FreqPestUseWork_everyday=`F3. Desde nuestra £ltima visita, ¨con qu‚ frecuencia, ha usado estos qu¡micos en el TRABAJO? (choice=Cada dia (diariamente))`,
          FreqPestUseWork_other=`F3. Desde nuestra £ltima visita, ¨con qu‚ frecuencia, ha usado estos qu¡micos en el TRABAJO? (choice=Otro)`,
          UsedParaquat=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Gramoxone o Paraquat Aleman o Agresivo o Superxone o Flama o Rafaga (ingrediente activo: paraquat))`, #at work
          UsedAmentrina=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Gesapax o Ametrex o Amefor (ingrediente activo: ametrina))`,
          UsedGlyphosate=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Roundup o Rival o Ranger o Forastero o Guerrero o Llanero (ingrediente activo: glifosato))`,
          UsedAcetoclor=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Acetofor (ingrediente activo: acetoclor))`,
          Used24D=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Espuela o Foram o Pastura (ingrediente activo: 2,4-D))`,
          UsedAtrazine=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Atrazina)`,
          UsedTerbutrina=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Terbutrina)`,
          UsedDiuron=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Diuron)`,
          UsedPendimetalina=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Pendimetalina)`,
          UsedotherHerbicide=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=Otra (especifique):)`,
          Usedxxx=`F4a. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Herbicidas (choice=No s‚ (describa, si es posible):)`,
          Usedother_name=`F4a. Si otro, porfavor especifique:`,
          UsedImidacloprides=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Jade o Joker o Defiende (ingrediente activo: imidacloprides))`,
          UsedChlorpyrifos=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Lorsban o Dursban o Forafos o Impacto (ingrediente activo: clorpirif¢s))`,
          UsedCipermetrina=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Zipper o Foratox o Foravan o Combate (ingrediente activo: cipermetrina))`,
          UsedDiazinon=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Diazide o Dianon o Diazifor (ingrediente activo: diazin¢n))`,
          UsedMetamidafos=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Metamidafos genericos o MTD600 o Monitor (ingrediente activo: Metamidafos))`,
          UsedTerbufoses=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Counter o Terbukill o Terbufos genericos (ingrediente activo: Terbufoses))`,
          UsedDeltamethrin=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Deltametrina)`,
          UsedPermethrin=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Permetrina)`,
          UsedParathion_or_MethylParathion=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Paration o Metil paration)`,
          UsedEndosulfan=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Endosulfan)`,
          UsedOtherinsecticide=`F4b. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Insecticidas: (choice=Otro (especifique):)`,
          UsedTricarbamate=`F4c. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Fungicidas: (choice=Mazante (ingrediente activo: Tiocarbamatos))`,
          UsedMetalaxyl=`F4c. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Fungicidas: (choice=Foraxil (ingrediente activo: metalaxyl))`,
          UsedOxicloruros=`F4c. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Fungicidas: (choice=Cupravit (ingrediente activo: Oxicloruros de cobre))`,
          UsedCaptan=`F4c. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Fungicidas: (choice=Captan)`,
          UsedOtherFungicide=`F4c. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Fungicidas: (choice=Otro (especifique):)`,
          
          UsedRacumin=`F4d. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Rodenticidas: (choice=Racumin)`,
          UsedBioRat=`F4d. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Rodenticidas: (choice=BioRat)`,
          UsedotherRodenticide=`F4d. ¨Cuales de los siguientes agroquimicos usted aplico en el trabajo?    Rodenticidas: (choice=Otro (especifique):)`,
         
         PestYN=`F5. ¨Usted aplico quimicos en EL TRABAJO en las ultimas 48 horas? (choice=Si)`, #at work
         PPEguantes=`F5a. ¨Usted hizo algo para protegerse de estar expuesto a estos qu¡micos? (NO LEA LAS OPCIONES, marque todo lo que aplica) (choice=Llevo guantes)`,
         PPEmascara=`F5a. ¨Usted hizo algo para protegerse de estar expuesto a estos qu¡micos? (NO LEA LAS OPCIONES, marque todo lo que aplica) (choice=Llevo mascara)`,
         PPEgafas=`F5a. ¨Usted hizo algo para protegerse de estar expuesto a estos qu¡micos? (NO LEA LAS OPCIONES, marque todo lo que aplica) (choice=Llevo gafas de proteccion)`,
         PPEropa=`F5a. ¨Usted hizo algo para protegerse de estar expuesto a estos qu¡micos? (NO LEA LAS OPCIONES, marque todo lo que aplica) (choice=Llevo ropa larga, cubrio su cuerpo)`,
         PPEnone=`F5a. ¨Usted hizo algo para protegerse de estar expuesto a estos qu¡micos? (NO LEA LAS OPCIONES, marque todo lo que aplica) (choice=Solo aplico cuando no habia viento)`,
         
         #Home use
          FreqPestUseHome_everymonth=`F6. ¨Desde nuestra £ltima visita de toma de muestra, con qu‚ frecuencia us¢ qu¡micos para matar malezas, roedores, insectos y / o hongos cuando NO estaba en el trabajo? (choice=Cada mes)`,
          FreqPestUseHome_everyweek=`F6. ¨Desde nuestra £ltima visita de toma de muestra, con qu‚ frecuencia us¢ qu¡micos para matar malezas, roedores, insectos y / o hongos cuando NO estaba en el trabajo? (choice=Cada semana)`,
          FreqPestUseHome_everyday=`F6. ¨Desde nuestra £ltima visita de toma de muestra, con qu‚ frecuencia us¢ qu¡micos para matar malezas, roedores, insectos y / o hongos cuando NO estaba en el trabajo? (choice=Cada dia (diariamente))`,
          FreqPestUseHome_other=`F6. ¨Desde nuestra £ltima visita de toma de muestra, con qu‚ frecuencia us¢ qu¡micos para matar malezas, roedores, insectos y / o hongos cuando NO estaba en el trabajo? (choice=Otro)`,
         
         #UsedHerbicides-Rodenticides=LG:MS
          ApplyPestHome48Hrs=`F9. ¨Usted aplico agroquimicos FUERA DEL TRABAJO en las ultimas 48 horas?`,
         
         # # G1- drink alcohol in the last 30 days?
         Alcoholcurrent=`G1. ¨Usted ha tomado bebidas alcoh¢licas en los £ltimos 30 d¡as?`,
         # # L1 - currently smoke cigs? Ever regularly?
         Smokecurrent=`H2. ¨Usted fuma cigarrillos, actualmente`) %>% 
  mutate(MANOS_ID=as.numeric(MANOS_ID),
         round=3,
         PestYN=ifelse(PestYN=="Checked",1,
                       ifelse(PestYN=="Unchecked",0,NA))) %>% 
  mutate(Dept1=case_when(Dept=="ACAJUTLA"~"Ahuachapan",
                         Dept=="aguachapan aguachpan"~"Ahuachapan",
                         Dept=="AHUACHAPAN"~"Ahuachapan",
                         Dept=="AHUCHAPAN"~"Ahuachapan",
                         Dept=="ahuachapan"~"Ahuachapan",
                         Dept=="Ahuachapán"~"Ahuachapan",
                         Dept=="Ahuachapa"~"Ahuachapan",
                         Dept=="Acajutla"~"Ahuachapan",
                         Dept=="JUJUTLA"~"Ahuachapan",
                         Dept=="Ahuachapan"~"Ahuachapan",
                         Dept=="cabañas"~"Cabanas",
                         Dept=="Carazo"~"Carazo",
                         Dept=="Chichigalpa"~"Chinandega",
                         grepl("dega",Dept,ignore.case=TRUE)~"Chinandega",
                         Dept=="Quezaltepeque Cuzcatlan"~"Cuscatlan",
                         grepl("liber",Dept,ignore.case=TRUE)~"LaLibertad",
                         Dept=="santa tecla"~"LaLibertad",
                         grepl("Le?n",Dept)~"Leon",
                         grepl("León",Dept,ignore.case = TRUE)~"Leon",
                         Dept=="Le¢n"~"Leon",
                         grepl("Managua",Dept,ignore.case=TRUE)~"Managua",
                         Dept=="CIUDAD DELGADO"~"SanSalvador",
                         Dept=="tonacatepeque"~"SanSalvador",
                         grepl("salvador",Dept,ignore.case = TRUE)~"SanSalvador",
                         Dept=="santa ana"~"SantaAna",
                         Dept=="IZALCO"~"Sonsonate",
                         grepl("sons",Dept,ignore.case=TRUE)~"Sonsonate",
                         Dept=="LA POZA"~"Usulutan",
                         grepl("jiquil",Dept,ignore.case=TRUE)~"Usulutan",
                         grepl("usu",Dept,ignore.case=TRUE)~"Usulutan")) %>% 
  mutate(industry=ifelse(Country=="Nicaragua",IndustryN,
                         ifelse(Country=="El Salvador",IndustryES, NA)),
         DWhome=ifelse(Country=="Nicaragua",DWhomeNic,
                       ifelse(Country=="El Salvador",DWhomeES, NA)))

R3PSQ.2<-R3PSQ %>%
  dplyr::select(MANOS_ID,Age,round,Country,Moved,Dept1,industry,DWhome,
         PestYN,PPEguantes,PPEmascara,PPEgafas,PPEropa,PPEnone,Alcoholcurrent,
         WorksiteAZ1_N,WorksiteAZ2_N,WorksiteLA_N,WorksitePL1_N,WorksitePL2_N, 
         WorksiteAZ1_ES,WorksiteAZ2_ES,WorksiteMA1_ES,WorksiteMA2_ES, WorksiteCO1_ES,
         WorksiteCO2_ES, UsedGlyphosate, 
         Usedpesticides,FreqPestUseWork_everymonth,FreqPestUseWork_everyweek,FreqPestUseWork_everyday,
         FreqPestUseWork_other,UsedParaquat,UsedAmentrina,UsedGlyphosate,UsedAcetoclor,Used24D,UsedAtrazine,UsedTerbutrina,
         UsedDiuron,UsedPendimetalina,UsedotherHerbicide,Usedxxx,Usedother_name,UsedImidacloprides,UsedChlorpyrifos,
         UsedCipermetrina,UsedDiazinon,UsedMetamidafos,UsedTerbufoses,UsedDeltamethrin,UsedPermethrin,UsedParathion_or_MethylParathion,
         UsedEndosulfan,UsedOtherinsecticide,UsedTricarbamate,UsedMetalaxyl,UsedOxicloruros,UsedCaptan,UsedOtherFungicide,
         UsedRacumin,UsedBioRat,UsedotherRodenticide,FreqPestUseHome_everymonth,FreqPestUseHome_everyweek,FreqPestUseHome_everyday,FreqPestUseHome_other,
         FreqPestUseWork_everyday,FreqPestUseWork_other,Smokecurrent,ApplyPestHome48Hrs) %>% 
  mutate(PPEYN=ifelse(PPEguantes=="Checked"|PPEmascara=="Checked"|PPEgafas=="Checked"|PPEropa=="Checked",1,
                      ifelse(PPEnone=="Checked",0,NA))) %>%
  dplyr::select(-PPEguantes,-PPEmascara,-PPEgafas,-PPEropa,-PPEnone) %>%
  pivot_longer(cols=("WorksiteAZ1_N":"WorksiteCO2_ES"),names_to="Sites",values_to = "Worksite") # %>%


R3_DW<-R3PSQ %>%
  dplyr::select(MANOS_ID,DWworkcontainerG,DWworkcontainer2L,DWworkcontainer1.5L,DWworkcontainer1L,DWworkcontainer600mL,
         DWworkcontainer300mL,DWworkcontainerother,DrinkGallonswork,Drink2Lwork,Drink1.5Lwork,Drink1Lwork,
         Drink600mLwork,Drink300mLwork,DrinkothervolL,Drinkotherwork) %>%
  mutate(Drinkotherwork=ifelse(MANOS_ID==142|MANOS_ID==152,2,Drinkotherwork)) %>%
  mutate(DWWorkContainerL=case_when(DWworkcontainerG=="Unchecked"&DWworkcontainer2L=="Unchecked"&
                                    DWworkcontainer1.5L=="Unchecked"&DWworkcontainer1L=="Unchecked"&
                                    DWworkcontainer600mL=="Unchecked"&DWworkcontainer300mL=="Unchecked"&
                                    DWworkcontainerother=="Unchecked"~0)) %>%
  pivot_longer(cols=c("DWworkcontainerG","DWworkcontainer2L","DWworkcontainer1.5L","DWworkcontainer1L","DWworkcontainer600mL","DWworkcontainer300mL","DWworkcontainerother"),
                 names_to = "DWworkContainer",values_to = "DWworkContainerSize") %>%
  mutate(DWworkContainerL.2=case_when(DWworkContainer=="DWworkcontainerG"&DWworkContainerSize=="Checked"~3.79,
                                   DWworkContainer=="DWworkcontainer2L"&DWworkContainerSize=="Checked"~2,
                                   DWworkContainer=="DWworkcontainer1.5L"&DWworkContainerSize=="Checked"~1.5,
                                   DWworkContainer=="DWworkcontainer1L"&DWworkContainerSize=="Checked"~1,
                                   DWworkContainer=="DWworkcontainer600mL"&DWworkContainerSize=="Checked"~.66,
                                   DWworkContainer=="DWworkcontainer300mL"&DWworkContainerSize=="Checked"~.33,
                                   DWworkContainer=="DWworkcontainerother"&DWworkContainerSize=="Checked"~DrinkothervolL)) %>%
  dplyr::select(-DWworkContainer,-DWworkContainerSize) %>% unique() %>%
  pivot_longer(cols=c("DrinkGallonswork","Drink2Lwork","Drink1.5Lwork","Drink1Lwork","Drink600mLwork","Drink300mLwork","Drinkotherwork"),
               names_to = "DrinkWorkVol",values_to = "DrinkWorkFreq") %>%
  dplyr::select(-DrinkWorkVol) %>%
  mutate(DWWorkVolL=ifelse(!is.na(DrinkWorkFreq),DWworkContainerL.2*DrinkWorkFreq,
                          ifelse(DWworkContainerL.2==0,0,NA))) %>%
  dplyr::select(-DWWorkContainerL,-DWworkContainerL.2,-DrinkWorkFreq) %>%
  unique() %>%
  group_by(MANOS_ID) %>%
  mutate(Totalwaterwork=sum(DWWorkVolL,na.rm = TRUE)) %>%
  dplyr::select(-DWWorkVolL) %>% unique() %>%
  dplyr::select(MANOS_ID,Totalwaterwork) %>%
  left_join(.,R3PSQ.2,by="MANOS_ID")
 
#### rbind R1 and R3 ####
R1_R3Q<-bind_rows(R1_D0_D3,R3_DW) %>%
  group_by(MANOS_ID) %>%
  mutate(Dept2=ifelse(Moved=="No",Dept1[round==1],
                     ifelse(Moved=="Si",Dept1[round==3])),
         FamHxAnyCKD2=ifelse(is.na(FamHxAnyCKD),FamHxAnyCKD[round==1],
                             ifelse(!is.na(FamHxAnyCKD),FamHxAnyCKD,NA))) %>%
  ungroup() %>%
  mutate(Dept3=ifelse(is.na(Moved),Dept1,Dept2)) %>%
  dplyr::select(-Dept1,-Dept2,-Moved,-FamHxAnyCKD)


