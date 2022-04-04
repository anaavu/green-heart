#######################################################################################
## Author: Anagha Uppal
## Date: Summer 2021
## Purpose: Loop through Models for Hypertension Analysis
## Inputs: 
## Outputs: 
#######################################################################################
## To do: 

library(caret)
library(leaps)
library(tidyverse)
library(MASS)
library(GGally)
library(bootStepAIC)
library(plotly)
library(mlbench)
library(lubridate) # dates
library(safejoin)
library(dplyr)
library(lubridate)


########## Grab the data
# Edit all lines to identify location of data
participants <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHData_AU_20201221.csv")
participants <- participants[-213,]
#shemp <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GH_shemp_variable.csv")

greenness <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/participants_allgreenness_20210214.csv")
greenness <- greenness[-256,]
datacanopy2 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/participants_allgreenness_20210809.csv")[, c(5, 178:199)]
datacanopy2 <- datacanopy2[-256,]

trafficdata1 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficGISData_AU_20210224.csv")
trafficdata2 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficGISData_AU_20210304.csv")
trafficdata3 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficExposure_AU_20210630.csv")
trafficdata3$StudyID <- trafficdata3$ï..StudyID

#addtm <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/addtm_column.csv")
climatedata <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_climate_data_DR_062821.csv")

addcols <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHDataAdd_RY_20210628.csv")
expdata <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHDataAdd_RY_20210727.csv")
rentown_new <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHParticipants_newrentown.csv")
# Prep some data
rentown_new <- rentown_new %>%
  mutate(propval_cat = ntile(CUR_TOTAL, 3)) %>%
  mutate(propval_cat = if_else(propval_cat == 1, 'Low', if_else(propval_cat == 2, 'Medium', 'High'))) %>%
  arrange(propval_cat)
# climatedata[duplicated(climatedata$stid), ]

########## Create new columns
participants <- participants %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>% 
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
  #mutate(shemp.desc = as.factor(if_else(shemp == 1, 'Yes', 'No'))) %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL)))))) %>%
  mutate(inc.desc2 = 
           case_when(
             inc.desc == "< $20,000" | inc.desc == "20,000-$45,000" ~ "Low",
             inc.desc == "$45,000-$65,000" | inc.desc == ">$65,000" ~ "High",
             TRUE ~ NA_character_,)) %>%
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(edu.desc2 = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                       if_else(edu>=4 & edu<=8, 
                                               'Some college or more', NULL)))) %>%
  mutate(edu.desc3 = as.factor(if_else(edu>=1 & edu<=2, '<=High School Diploma',
                                       if_else(edu>=3 & edu<=8, 
                                               'HS or more', NULL)))) %>%
  mutate(medu.desc = as.factor(if_else(medu>=1 & medu<=3, '<=High School Diploma',
                                       if_else(medu==4 | medu==5, 'Some college',
                                               if_else(medu>=6 & medu<=8, '>=4-year degree', NULL))))) %>%
  mutate(medu.desc2 = as.factor(if_else(medu>=1 & medu<=2, '<=High School Diploma',
                                        if_else(medu>=3 & medu<=8, 
                                                'Some college or more', NULL)))) %>%
  mutate(rentown.desc = as.factor(if_else(rentown == 1, 'Rent', 'Own'))) %>%
  mutate(bc.desc = as.factor(if_else(bc>=1 & bc<=4, 'Single home or duplex',
                                     if_else(bc==5 | bc==6, 'Apartment', NULL)))) %>%
  mutate(hznum.desc = as.factor(if_else(hznum==1 | hznum==2, 'Small',
                                        if_else(hznum>=3 & hznum<=5, 'Medium',
                                                if_else(hznum>=6, 'Large', NULL))))) %>%
  mutate(exr.desc = as.factor(if_else(exr == 1, 'Yes', 
                                      if_else(exr == 2, 'No', NULL)))) %>%
  mutate(obese.desc = as.factor(if_else(bmi >= 30, 'Yes', 
                                        if_else(bmi < 30, 'No', NULL)))) %>%
  mutate(nowsmk.desc = as.factor(if_else(u_cot_n > 40, 'Yes', 'No'))) %>%
  #mutate(eh10.desc = as.factor(if_else(eh10 == 1, 'Yes', 'No'))) %>%
  #mutate(nhlp.desc = as.factor(if_else(nhlp==3 | nhlp==4, 'Yes', 'No'))) %>%
  #mutate(nwtch.desc = as.factor(if_else(nwtch==3 | nwtch==4, 'Yes', 'No'))) %>%
  #mutate(crsit.desc = as.factor(if_else(crsit==3 | crsit==4, 'Yes', 'No'))) %>%
  mutate(sdwlk.desc = as.factor(if_else(sdwlk==3 | sdwlk==4, 'Yes', 'No'))) %>%
  #mutate(illbhv.desc = as.factor(if_else(illbhv==3 | illbhv==4, 'Yes', 'No'))) %>%
  mutate(litt.desc = as.factor(if_else(litt==3 | litt==4, 'Yes', 'No'))) %>%
  mutate(saf1.desc = as.factor(if_else(saf1==1 | saf1==2, 'Yes', 'No'))) %>%
  mutate(saf2.desc = as.factor(if_else(saf2==1 | saf2==2, 'Yes', 'No'))) %>%
  mutate(saf3.desc = as.factor(if_else(saf3==1 | saf3==2, 'Yes', 'No'))) %>%
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281) %>%
  mutate(ace.desc = as.factor(if_else(ace == 0, 'No', 
                                      if_else(ace == 1, 'Yes', NULL)))) %>%
  mutate(diur.desc = as.factor(if_else(diur == 0, 'No', 
                                       if_else(diur == 1, 'Yes', NULL)))) %>%
  mutate(htn2 = as.factor(if_else(htn == 1, 'Yes', 'No'))) %>%
  mutate(htn3 = as.factor(if_else(htn == 1, 'Yes', 
                                  if_else(htn == 2, 'No', NULL)))) %>%
  mutate(wkout.desc = as.factor(if_else(wkout <= 5, 'Low',
                                        if_else(wkout> 5 & wkout<=10, 'Medium',
                                                if_else(wkout > 10, 'High', NULL))))) %>%
  mutate(dmorHbA1c = as.factor(if_else(dm == 1, "Yes",
                                       if_else(dm == 2, "No",
                                               if_else((dm == 3 | dm == 4) & HbA1c > 6.5, "Yes", "No"))))) %>%
  mutate(htn4 = as.factor(if_else(htn == 1, "Yes",
                                  if_else(htn == 2, "No",
                                          if_else((htn == 3 | htn == 4) & sbpavg > 140, "Yes", "No"))))) %>%
  mutate(anyDiurACECCB = as.factor(if_else(diur == 1 | psdiur == 1 | ace == 1 | ccb == 1, "Yes", "No"))) %>%
  mutate(MAP = (0.333*sbpavg)+(0.6666*dbpavg)) %>%
  mutate(wrkarea.desc = 
           case_when(
             wrkarea == 1 ~ "Outdoors",
             wrkarea == 2 ~ "Travel",
             wrkarea ==3 ~ "Drive",
             wrkarea == 4 ~ "Other",
             wrkarea == 7 ~ "Indoors",
             wrkarea == 8 ~ "Unemployed",
             TRUE ~ NA_character_,
           )) %>%
  mutate(wrkarea.desc2 = 
           case_when(
             wrkarea == 1 | wrkarea == 2 | wrkarea == 3 | wrkarea == 4 | wrkarea == 7  ~ "Employed",
             wrkarea == 8 ~ "Unemployed",
             TRUE ~ NA_character_,
           ))

########## Join dataframes
participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
participant_greenness <- safe_left_join(participant_greenness, datacanopy2, by=c("stid" = "StudyID"),
                                        conflict = coalesce)
participant_greenness$de <- as.Date(mdy(participant_greenness$de))
# participant_greenness <- left_join(participant_greenness, shemp, by=c("stid"="stid"))
participant_greenness  <- left_join(participant_greenness, trafficdata1, by=c("stid"="StudyID"))
participant_greenness  <- safe_left_join(participant_greenness, trafficdata2, by=c("stid"="StudyID"),
                                         conflict = coalesce)
participant_greenness$Roadlength[is.na(participant_greenness$Roadlength)] = 0
participant_greenness$RoadlengthtimesTraffic[is.na(participant_greenness$RoadlengthtimesTraffic)] = 0
participant_greenness  <- left_join(participant_greenness, trafficdata3, by=c("stid"="StudyID"))

# participant_greenness  <- left_join(participant_greenness, addtm, by=c("stid"="stid"))
#participant_greenness  <- left_join(participant_greenness, addcols, by=c("stid"="stid"))
participant_greenness  <- safe_left_join(participant_greenness, expdata, by=c("stid"="stid"),
                                         conflict = coalesce)
participant_greenness  <- left_join(participant_greenness, climatedata, by=c("stid"="stid"))
participant_greenness  <- left_join(participant_greenness, rentown_new, by=c("stid"="StudyID"))
# participant_greenness[,261:262] <- sapply(participant_greenness[,261:262],as.numeric) # u_mu

participant_greenness <- participant_greenness %>% 
  mutate(chem_fume_expos = as.factor(if_else(chem == 1 | fume == 1, 'Yes', 'No')))
participant_greenness$rentown_parcel[participant_greenness$rentown_parcel == ""] <- NA

participant_greenness$inc.desc <- factor(participant_greenness$inc.desc, levels = c("< $20,000", "20,000-$45,000", "$45,000-$65,000", ">$65,000"))
participant_greenness$inc.desc2 <- factor(participant_greenness$inc.desc2, levels = c("Low", "High"))
participant_greenness$edu.desc <- factor(participant_greenness$edu.desc, levels = c('<=High School Diploma', "Some college", ">=4-year degree"))
participant_greenness$medu.desc <- factor(participant_greenness$medu.desc, levels = c('<=High School Diploma', "Some college", ">=4-year degree"))
participant_greenness$hznum.desc <- factor(participant_greenness$hznum.desc, levels = c("Small", "Medium", "Large"))
participant_greenness$wkout.desc <- factor(participant_greenness$wkout.desc, levels = c("Low", "Medium", "High"))
participant_greenness$propval_cat <- factor(participant_greenness$propval_cat, levels = c("Low", "Medium", "High"))
participant_greenness$wrkarea.desc <- factor(participant_greenness$wrkarea.desc, levels = c("Indoors", "Outdoors", "Drive", "Travel",
                                                                                            "Unemployed", "Other"))
participant_greenness <- participant_greenness %>%
  mutate(NDVIcat = ntile(Participant_200m_Mean_ndvi, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("Low", "Medium", "High")))

participant_greenness <- participant_greenness %>%
  mutate(SPL_THEMES_cat = ntile(SPL_THEMES, 2)) %>%
  mutate(SPL_THEMES_cat = if_else(SPL_THEMES_cat == 1, 'Low', 'High')) %>%
  mutate(SPL_THEMES_cat = factor(SPL_THEMES_cat) %>% fct_relevel(c("Low", "High")))

participant_greenness = participant_greenness %>% select(-starts_with("HEAL"),
                                                         -starts_with("Heal"),
                                                         -starts_with("FocalStatistics"),
                                                         -starts_with("Andrea"),
                                                         -starts_with("ghKDE"),
                                                         -starts_with("ParcelID"),
                                                         -starts_with("FIPS"),
                                                         -starts_with("Mean_"),
                                                         -starts_with("Sum_"))

########## Subsets and stratifications
nonsmokers <- participant_greenness %>% filter(nowsmk.desc=="No")
smokers <- participant_greenness %>% filter(nowsmk.desc=="Yes")

bc_home <- participant_greenness %>% filter(bc.desc == "Single home or duplex")
bc_rent <- participant_greenness %>% filter(bc.desc == "Apartment")
bc_home_filtered <- bc_home %>% filter(CUR_TOTAL < 700000 & CUR_TOTAL > 0 & Parcel_ndvi_Sum < 90000 & Parcel_canopy_Sum < 100000)

evry1 <- participant_greenness

meds <- participant_greenness %>% filter(anyDiurACECCB=="Yes")
nomeds <- participant_greenness %>% filter(anyDiurACECCB=="No")

diabetes <- participant_greenness %>% filter(dmorHbA1c=="Yes")
nodiabetes <- participant_greenness %>% filter(dmorHbA1c=="No")

smokers <- participant_greenness %>% filter(nowsmk.desc=="Yes")
nonsmokers <- participant_greenness %>% filter(nowsmk.desc=="No")

smokers1 <- participants_greenness %>% filter(nowsmk.desc=="Yes")
nonsmokers1 <- participants_greenness %>% filter(nowsmk.desc=="No")

smokers2 <- htn2 %>% filter(nowsmk.desc=="Yes")
nonsmokers2 <- htn2 %>% filter(nowsmk.desc=="No")

home <- participant_greenness %>% filter(bc.desc == "Single home or duplex")

########## Identify greenness columns
colnames_green <- colnames(participant_greenness)
# Edit this line--replace numbers with the correct items referring to greenness information
colnames_green <-  colnames_green[346:510]

########## Loop
# Edit this--add folder name for each greenness result to go in. make sure to create folder first
setwd("I:/Backups/GIS/Local_Computer_Stuff/GIS/models/htn_all_model0")
lapply(colnames_green,
       
       function(var) {
         #  ~ inc.desc+edu.desc+age+race.desc+gend.desc+NEAR_DIST_MajorRoads
         # subset= rentown.desc=="Rent"
         # Rent, mdu, hznum, cur_total
         # model0 <- lm(sbpavg ~greenness
         # model1 <- lm(sbpavg ~greenness+race.desc+age+gend.desc
         # model2 <- lm(sbpavg ~greenness+race.desc+age+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)
         # model3 <- lm(sbpavg ~greenness+race.desc+age+gend.desc+edu.desc+
         #(edu.desc*inc.desc)+as.factor(cope)+bmi+nowsmk.desc+as.factor(etoh1)+as.factor(dmorHbA1c)+as.factor(lip)+exr.desc
         # model3.1 <- lm(sbpavg ~greenness+race.desc+age+gend.desc+edu.desc+
         #(edu.desc*inc.desc)+as.factor(cope)+bmi+nowsmk.desc+as.factor(etoh1)+as.factor(dmorHbA1c)+as.factor(lip)+exr.desc+as.factor(anyDiurACECCB)
         # model4 <- lm(sbpavg ~greenness+race.desc+age+gend.desc+edu.desc+
         #(edu.desc*inc.desc)+as.factor(cope)+bmi+nowsmk.desc+as.factor(etoh1)+as.factor(dmorHbA1c)+as.factor(lip)+
         #as.factor(TrafficExposure)+Temp_AVG
         
         formula <- as.formula(paste("sbpavg ~ ", var, ""))
         model1 <- glm(formula, data = participants_greenness)
         conf1 <- confint(model1)
         write.csv(tidy(model1), paste0(var,".csv"))
         write.csv(conf1, paste0(var,"_confint.csv"))
         toselect.x <- subset(tidy(model1), p.value < 0.05)
         write.csv(toselect.x, paste0(var, "_selected.csv"))
       })



