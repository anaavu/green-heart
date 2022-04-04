#######################################################################################
## Author: Anagha Uppal
## Date: Nov 2020-Feb 2021
## Purpose: Table 1 for SES 2nd Paper
## Inputs: participants, greenness
## Outputs: tables
#######################################################################################
## To do: 

## Install packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyverse")) install.packages("tidyverse")

library(ggplot2) # for plotting
library(dplyr) # for data cleaning
library(gtsummary) # for pub-ready tables
library(purrr)
library(tidyverse)
library(rcompanion)
library(corrr) # 
library(devtools)

######################PREP##################################
# Input data file locations
participants <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/GHData_AU_20201221.csv")
bgdems <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/GHParticipants_bg.csv")
greenness <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participants_allgreenness_20210309.csv")

## clean participants table to be usable
# this creates columns to convert numeric categories to their meaningful counterparts
participants <- participants %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>% 
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
  #mutate(shemp.desc = as.factor(if_else(shemp == 1, 'Yes', 'No'))) %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL)))))) %>%
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(medu.desc = as.factor(if_else(medu>=1 & medu<=3, '<=High School Diploma',
                                       if_else(medu==4 | medu==5, 'Some college',
                                               if_else(medu>=6 & medu<=8, '>=4-year degree', NULL))))) %>%
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
  mutate(dm.desc = as.factor(if_else(dm == 1, 'Yes', 
                                     if_else(dm == 2, 'No', NULL)))) %>%
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
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)

## clean greenness table to be usable
# this divides numeric NDVI into low/med/high categorical 
greenness <- greenness %>%
  mutate(NDVIcat = ntile(Participant_200m_Mean_ndvi, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))

SVI <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/ghgeocodes_SVI.csv")


# join greenness values to participants
participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
participant_greenness  <- safe_left_join(participant_greenness, SVI, by=c("stid"="StudyID"),
                                         conflict = coalesce)
############################################################

## pull from the participants greenness tables those columns you'll use for table 1 (dems)
pg_SES <- participant_greenness %>% select(gend.desc,race.desc,ethn.desc,
                                           inc.desc,
                                           edu.desc,
                                           medu.desc,
                                           wrkarea.desc2,
                                           hznum.desc,
                                           exr.desc,
                                           #dm.desc,
                                           rentown.desc,bc.desc,
                                           nowsmk.desc,
                                           #nhlp.desc,nwtch.desc,illbhv.desc,crsit.desc,eh10.desc,shemp.desc,
                                           litt.desc,
                                           sdwlk.desc,
                                           saf1.desc,saf2.desc,saf3.desc,
                                           #cope.desc,etoh1.desc,lip.desc,
                                           age,bmi,
                                           #sbpavg,dbpavg,
                                           NEAR_DIST_MajorRoads,
                                           #LASTCNT_MajorRoadsTraffic,
                                           PopDen,
                                           NDVIcat, 
                                           SPL_THEMES,
                                           #RPL_THEMES,
                                           #biomasscat,laicat,leafcat,parcellaicat,
                                           #parcelcat,nobuildingcat,lessvcat,morevcat
)

## tbl_summary (from gtsummary is used)
## to create pub-ready tables
# Use gtsummary vignettes to explore and edit the parameters
setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables")
set_gtsummary_theme(theme_gtsummary_compact())
# theme_gtsummary_journal(journal = "jama")
#reset_gtsummary_theme()
pg_SES %>% tbl_summary(by = NDVIcat,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                    ethn.desc ~ "Ethnicity", exr.desc ~ "Exercise",
                                    age ~ "Age",
                                    #dm.desc ~ "Diabetes",
                                    bmi ~ "BMI",
                                    #sbpavg ~ "Systolic BP", 
                                    #dbpavg ~ "Diastolic BP",
                                    inc.desc ~ "Income", edu.desc ~ "Education",
                                    medu.desc ~ "Mother's education",
                                    wrkarea.desc2 ~ "Employment",
                                    nowsmk.desc ~ "Current smoker",
                                    hznum.desc ~ "Household size",
                                    bc.desc ~ "Building type", 
                                    #shemp.desc ~ "Currently employed?",
                                    # eh10.desc ~ "Food source not grocery store",
                                    #nhlp.desc ~ "Caretaking neighbors",
                                    #nwtch.desc ~ "Concerned neighbors",
                                    #crsit.desc ~ "Bad traffic conditions",
                                    sdwlk.desc ~ "Poorly maintained sidewalks",
                                    litt.desc ~ "Trash/litter presence",
                                    #illbhv.desc ~ "Disruptive neighbors",
                                    saf1.desc ~ "Safe to walk neigborhood",
                                    saf2.desc ~ "No violence in neighborhood",
                                    saf3.desc ~ "No crime in neighborhood",
                                    #cope.desc ~ "Stress levels",
                                    #etoh1.desc ~ "Alcohol consumption",
                                    #lip.desc ~ "High cholesterol",
                                    rentown.desc ~ "Rent or own home",
                                    NEAR_DIST_MajorRoads ~ "Distance to major road",
                                    #LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                    PopDen ~ "Area Population density",
                                    SPL_THEMES ~ "Social Vulnerability Index"
                       ),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p() %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "SEStable2.png",
    expand = 1,
  )

######################TABLE 1 HTN##################################

## pull from the participants greenness tables those columns you'll use for table 1 (dems)
pg_SES <- participant_greenness %>% select(gend.desc,race.desc,ethn.desc,
                                           inc.desc,
                                           edu.desc,
                                           #medu.desc,
                                           wrkarea.desc2,
                                           hznum.desc,
                                           exr.desc,
                                           dm.desc,
                                           rentown.desc,bc.desc,
                                           nowsmk.desc,
                                           #nhlp.desc,nwtch.desc,illbhv.desc,crsit.desc,eh10.desc,shemp.desc,
                                           #litt.desc,
                                           #sdwlk.desc,
                                           #saf1.desc,saf2.desc,saf3.desc,
                                           cope.desc,etoh1.desc,lip.desc,
                                           age,bmi,
                                           sbpavg,dbpavg,
                                           NEAR_DIST_MajorRoads,
                                           #LASTCNT_MajorRoadsTraffic,
                                           PopDen,
                                           NDVIcat, 
                                           SPL_THEMES,
                                           #RPL_THEMES,
                                           #biomasscat,laicat,leafcat,parcellaicat,
                                           #parcelcat,nobuildingcat,lessvcat,morevcat
                                           )

## tbl_summary (from gtsummary is used)
## to create pub-ready tables
# Use gtsummary vignettes to explore and edit the parameters
setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables")
set_gtsummary_theme(theme_gtsummary_compact())
# theme_gtsummary_journal(journal = "jama")
#reset_gtsummary_theme()
pg_SES %>% tbl_summary(by = NDVIcat,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                    ethn.desc ~ "Ethnicity", exr.desc ~ "Exercise",
                                    age ~ "Age",
                                    dm.desc ~ "Diabetes",
                                    bmi ~ "BMI",
                                    sbpavg ~ "Systolic BP", 
                                    dbpavg ~ "Diastolic BP",
                                    inc.desc ~ "Income", edu.desc ~ "Education",
                                    #medu.desc ~ "Mother's education",
                                    wrkarea.desc2 ~ "Employment",
                                    nowsmk.desc ~ "Current smoker",
                                    hznum.desc ~ "Household size",
                                    bc.desc ~ "Building type", 
                                    #shemp.desc ~ "Currently employed?",
                                    # eh10.desc ~ "Food source not grocery store",
                                    #nhlp.desc ~ "Caretaking neighbors",
                                    #nwtch.desc ~ "Concerned neighbors",
                                    #crsit.desc ~ "Bad traffic conditions",
                                    #sdwlk.desc ~ "Poorly maintained sidewalks",
                                    #litt.desc ~ "Trash/litter presence",
                                    #illbhv.desc ~ "Disruptive neighbors",
                                    #saf1.desc ~ "Safe to walk neigborhood",
                                    #saf2.desc ~ "No violence in neighborhood",
                                    #saf3.desc ~ "No crime in neighborhood",
                                    cope.desc ~ "Stress levels",
                                    etoh1.desc ~ "Alcohol consumption",
                                    lip.desc ~ "High cholesterol",
                                    rentown.desc ~ "Rent or own home",
                                    NEAR_DIST_MajorRoads ~ "Distance to major road",
                                    #LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                    PopDen ~ "Area Population density",
                                    SPL_THEMES ~ "Social Vulnerability Index"
                                    ),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p() %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "HTNtable1.png",
    expand = 1,
  )

## type and value for displaying dichotomous in one row
## Use tbl_stack to create group headers between sections of table

############################################################
# Other versions
# AllYard
participant_greenness <- participant_greenness %>%
  mutate(NDVIcat_allyard = ntile(Nobuilding_ndvi_Mean, 3)) %>%
  mutate(NDVIcat_allyard = if_else(NDVIcat_allyard == 1, 'Low', 
                           if_else(NDVIcat_allyard == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat_allyard = factor(NDVIcat_allyard) %>% fct_relevel(c("High", "Medium", "Low")))

## pull from the participants greenness tables those columns you'll use for table 1 (dems)
pg_SES <- participant_greenness %>% select(gend.desc,race.desc,ethn.desc,
                                           inc.desc,
                                           edu.desc,
                                           medu.desc,
                                           wrkarea.desc2,
                                           hznum.desc,
                                           exr.desc,
                                           #dm.desc,
                                           rentown.desc,bc.desc,
                                           nowsmk.desc,
                                           #nhlp.desc,nwtch.desc,illbhv.desc,crsit.desc,eh10.desc,shemp.desc,
                                           litt.desc,
                                           sdwlk.desc,
                                           saf1.desc,saf2.desc,saf3.desc,
                                           #cope.desc,etoh1.desc,lip.desc,
                                           age,bmi,
                                           #sbpavg,dbpavg,
                                           NEAR_DIST_MajorRoads,
                                           #LASTCNT_MajorRoadsTraffic,
                                           PopDen,
                                           NDVIcat_allyard, 
                                           SPL_THEMES,
                                           #RPL_THEMES,
                                           #biomasscat,laicat,leafcat,parcellaicat,
                                           #parcelcat,nobuildingcat,lessvcat,morevcat
)

## tbl_summary (from gtsummary is used)
## to create pub-ready tables
# Use gtsummary vignettes to explore and edit the parameters
setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables")
set_gtsummary_theme(theme_gtsummary_compact())
# theme_gtsummary_journal(journal = "jama")
#reset_gtsummary_theme()
pg_SES %>% tbl_summary(by = NDVIcat_allyard,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                    ethn.desc ~ "Ethnicity", exr.desc ~ "Exercise",
                                    age ~ "Age",
                                    #dm.desc ~ "Diabetes",
                                    bmi ~ "BMI",
                                    #sbpavg ~ "Systolic BP", 
                                    #dbpavg ~ "Diastolic BP",
                                    inc.desc ~ "Income", edu.desc ~ "Education",
                                    medu.desc ~ "Mother's education",
                                    wrkarea.desc2 ~ "Employment",
                                    nowsmk.desc ~ "Current smoker",
                                    hznum.desc ~ "Household size",
                                    bc.desc ~ "Building type", 
                                    #shemp.desc ~ "Currently employed?",
                                    # eh10.desc ~ "Food source not grocery store",
                                    #nhlp.desc ~ "Caretaking neighbors",
                                    #nwtch.desc ~ "Concerned neighbors",
                                    #crsit.desc ~ "Bad traffic conditions",
                                    sdwlk.desc ~ "Poorly maintained sidewalks",
                                    litt.desc ~ "Trash/litter presence",
                                    #illbhv.desc ~ "Disruptive neighbors",
                                    saf1.desc ~ "Safe to walk neigborhood",
                                    saf2.desc ~ "No violence in neighborhood",
                                    saf3.desc ~ "No crime in neighborhood",
                                    #cope.desc ~ "Stress levels",
                                    #etoh1.desc ~ "Alcohol consumption",
                                    #lip.desc ~ "High cholesterol",
                                    rentown.desc ~ "Rent or own home",
                                    NEAR_DIST_MajorRoads ~ "Distance to major road",
                                    #LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                    PopDen ~ "Area Population density",
                                    SPL_THEMES ~ "Social Vulnerability Index"
                       ),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p() %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "SEStable2_allyard.png",
    expand = 1,
  )


#bg
ghbg <- ghbg %>%
  mutate(NDVIcat = ntile(bgndvi, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                                   if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))

## pull from the participants greenness tables those columns you'll use for table 1 (dems)
ghbg_select <- ghbg %>% select(Per_Capita_Income, Median_HH_income, 
                               F__Owner_Occupied_Household, 
                               hsgrad_pct, SPL_THEMES, NDVIcat,
                               minority_pct, age, roadlength
)
setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables")
set_gtsummary_theme(theme_gtsummary_compact())
# theme_gtsummary_journal(journal = "jama")
#reset_gtsummary_theme()
ghbg_select %>% tbl_summary(by = NDVIcat,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(Per_Capita_Income ~ "Per Capita Income", 
                                    Median_HH_income ~ "Median HH Income", 
                                    F__Owner_Occupied_Household ~ "% Owner Occupied Households", 
                                    hsgrad_pct ~ "% HS Grad",
                                    age ~ "Median Age", 
                                    minority_pct ~ "Minority %", 
                                    SPL_THEMES ~ "Social Vulnerability Index",
                                    roadlength ~ "Length of roads"
                       ),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI-Block Group**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p() %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "SEStable2_BG.png",
    expand = 1,
  )

#SVI
## pull from the participants greenness tables those columns you'll use for table 1 (dems)
pg_SES <- participant_greenness %>% select(gend.desc,race.desc,ethn.desc,
                                           inc.desc,
                                           edu.desc,
                                           medu.desc,
                                           wrkarea.desc2,
                                           hznum.desc,
                                           exr.desc,
                                           #dm.desc,
                                           rentown.desc,bc.desc,
                                           nowsmk.desc,
                                           #nhlp.desc,nwtch.desc,illbhv.desc,crsit.desc,eh10.desc,shemp.desc,
                                           litt.desc,
                                           sdwlk.desc,
                                           saf1.desc,saf2.desc,saf3.desc,
                                           #cope.desc,etoh1.desc,lip.desc,
                                           age,bmi,
                                           #sbpavg,dbpavg,
                                           NEAR_DIST_MajorRoads,
                                           #LASTCNT_MajorRoadsTraffic,
                                           PopDen,
                                           SPL_THEMES_cat,
                                           Participant_200m_Mean_ndvi
                                           #RPL_THEMES,
                                           #biomasscat,laicat,leafcat,parcellaicat,
                                           #parcelcat,nobuildingcat,lessvcat,morevcat
)

## tbl_summary (from gtsummary is used)
## to create pub-ready tables
# Use gtsummary vignettes to explore and edit the parameters
setwd("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables")
set_gtsummary_theme(theme_gtsummary_compact())
# theme_gtsummary_journal(journal = "jama")
#reset_gtsummary_theme()
pg_SES %>% tbl_summary(by = SPL_THEMES_cat,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                    ethn.desc ~ "Ethnicity", exr.desc ~ "Exercise",
                                    age ~ "Age",
                                    #dm.desc ~ "Diabetes",
                                    bmi ~ "BMI",
                                    #sbpavg ~ "Systolic BP", 
                                    #dbpavg ~ "Diastolic BP",
                                    inc.desc ~ "Income", edu.desc ~ "Education",
                                    medu.desc ~ "Mother's education",
                                    wrkarea.desc2 ~ "Employment",
                                    nowsmk.desc ~ "Current smoker",
                                    hznum.desc ~ "Household size",
                                    bc.desc ~ "Building type", 
                                    #shemp.desc ~ "Currently employed?",
                                    # eh10.desc ~ "Food source not grocery store",
                                    #nhlp.desc ~ "Caretaking neighbors",
                                    #nwtch.desc ~ "Concerned neighbors",
                                    #crsit.desc ~ "Bad traffic conditions",
                                    sdwlk.desc ~ "Poorly maintained sidewalks",
                                    litt.desc ~ "Trash/litter presence",
                                    #illbhv.desc ~ "Disruptive neighbors",
                                    saf1.desc ~ "Safe to walk neigborhood",
                                    saf2.desc ~ "No violence in neighborhood",
                                    saf3.desc ~ "No crime in neighborhood",
                                    #cope.desc ~ "Stress levels",
                                    #etoh1.desc ~ "Alcohol consumption",
                                    #lip.desc ~ "High cholesterol",
                                    rentown.desc ~ "Rent or own home",
                                    NEAR_DIST_MajorRoads ~ "Distance to major road",
                                    #LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                    PopDen ~ "Area Population density",
                                    Participant_200m_Mean_ndvi ~ "NDVI"
                       ),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**SVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p() %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "SEStable2_SVI.png",
    expand = 1,
  )

######################TABLE 2##################################

## identify the continuous variables only
# this will be used for shapiro test
# this tests for normality of the data
cont_vars <- pg_filtered %>% select(age,bmi,sbpavg,dbpavg)
shapiro_test <- as.data.frame(sapply(cont_vars, shapiro.test))

## histograms
# none of these are normally distributed
hist(participant_greenness$age)
hist(participant_greenness$bmi)
hist(participant_greenness$sbpavg)
hist(participant_greenness$dbpavg)

## Creates a visual normality table representation of the continuous variables
normalityTable <- data.frame(statistic = matrix(c(normalityVar1[1:4])))
normalityTable <- cbind(normalityVar1, p_val=matrix(c(normalityVar1[5:8])))

# for continuous data: VIF for collinearity
car::vif(full.model)
# for categorical data: chi.sq of independence
table <- table(participant_greenness$gend.desc,participant_greenness$inc.desc)
chisq.test(table)

######################STOP HERE##################################
######################move to all_corr.R##################################

#boxcox()
t1 <- data.frame(statistic = matrix(normalityVar1[1:7]),
                 p_value = matrix(normalityVar1[8:14]))
normalityVar1
#anova

# test for normality
shapiro.test(df$Cn)

sstable <- Anova(model, type = 3) 
tbl_regression(model, label = NDVIcat1 ~ "Low NDVI")


# generalized linear model
library(regclass)
model <- glm(NDVIcat ~ gend + age + bmi + nowsmk, 
             data = participant_greenness, 
             na.action = na.exclude,
             family = "binomial")

library(car)
sstable <- Anova(model, type = 3) 

tbl_regression(model, label = gend ~ "Gender")


######################OTHER##################################
model <- glm(NDVIcat ~ inc * rentown * nowsmk * litt,
             data = participant_greenness, 
             na.action = na.exclude,
             family = "binomial")
summary(model)


# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

assoc <- mixed_assoc(pg_SES)


pg_SES %>%
  # select(- name) %>%
  mixed_assoc() %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot()
