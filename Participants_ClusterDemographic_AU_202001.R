###################################################
## Name: Anagha Uppal
## Topic: Tables for SES Paper
## Date: Nov, 20
###################################################
## Install packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyverse")) install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(gtsummary)
library(purrr)
library(tidyverse)
library(gt)

######################PREP##################################
# Input data file locations
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_RY_041720.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/HEALgreennessMetrics_RY_110920.csv")
GISdata <- read.csv("C:/Users/a0uppa01/Documents/GIS/HEAL_addGISData_AU_20201202.csv")
GISdata <- GISdata %>% select(NEAR_DIST_MajorRoads, LASTCNT_MajorRoadsTraffic, 
                              PopDen, StudyID)
GHcluster <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_whichcluster.csv")

participants <- left_join(participants, GISdata, by=c("stid"="StudyID"))
participants <- left_join(participants, GHcluster, by=c("stid"="StudyID"))

whichbg <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHParticipants_whichbg.csv")
bgdems <- read.csv("C:/Users/a0uppa01/Documents/GIS/bg_Jefferson_dems_filtered.csv")[-1,]
bgdems$FIPS <- as.numeric(bgdems$FIPS)
whichbg <- whichbg %>% select(StudyID, FIPS)
participants <- left_join(participants, whichbg, by=c("stid"="StudyID"))
participants <- left_join(participants, bgdems, by=c("FIPS"="FIPS"))


participants <- participants %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>% 
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
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
  mutate(bc.desc = as.factor(if_else(bc>=1 & edu<=4, 'Single home or duplex',
                                     if_else(bc==5 | bc==6, 'Apartment', NULL)))) %>%
  mutate(hznum.desc = as.factor(if_else(hznum==1 | hznum==2, 'Small',
                                        if_else(medu>=3 & medu<=5, 'Medium',
                                                if_else(medu>=6, 'Large', NULL))))) %>%
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)


greenness <- greenness %>%
  mutate(NDVIcat = ntile(FocalStatistics_OutRaster_Full7inNDVI042020200m, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
############################################################


######################TABLE 1##################################

pg_Cluster <- participant_greenness %>% select(gend.desc,Male,Female,
                                          race.desc,ethn.desc,
                                          Pct_White, Pct_Black,Pct_Hispanic,
                                          inc.desc, Per_Capita_Income,
                                          Median_HH_income,
                                          edu.desc,medu.desc,
                                          Percent_Less_than_Equal_to_HS_Diploma,
                                          Percentage_Some_College,
                                          Percentage_Bachelors_and_Above,
                                          hznum.desc,rentown.desc, Own,Rent,
                                          bc.desc,age,Median_age,
                                          FIPS,
                                          NEAR_DIST_MajorRoads, LASTCNT_MajorRoadsTraffic,
                                          PopDen,ZoneID,NDVIcat, 
                                          FocalStatistics_OutRaster_Full7inNDVI042020200m)
theme_gtsummary_compact()
pg_Cluster %>% tbl_summary(by = ZoneID,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(race.desc ~ "Race-GH", gend.desc ~ "Gender-GH", 
                                    Male ~ "% Male-BG",Female ~ "% Female-BG",
                                    ethn.desc ~ "Ethnicity-GH", 
                                    Pct_White ~ "% White-BG",
                                    Pct_Black ~ "% Black-BG",
                                    Pct_Hispanic ~ "% Hispanic-BG",
                                    age ~ "Age-GH", Median_age ~ "Median age-BG",
                                    inc.desc ~ "Income-GH", 
                                    Per_Capita_Income ~ "Per Capita Income-BG",
                                    Median_HH_income ~ "Median HH Income-BG",
                                    edu.desc ~ "Education-GH",
                                    medu.desc ~ "Mother's education-GH",
                                    hznum.desc ~ "Household size-GH",
                                    bc.desc ~ "Building type-GH", 
                                    rentown.desc ~ "Rent or own home-GH",
                                    Own ~ "% Own-BG", Rent ~ "% Rent-BG",
                                    Percent_Less_than_Equal_to_HS_Diploma ~ "% <= HS Diploma-BG",
                                    Percentage_Some_College ~ "% Some College-BG",
                                    Percentage_Bachelors_and_Above ~ "% >= BA (BG)",
                                    NEAR_DIST_MajorRoads ~ "Distance to major road",
                                    LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                    PopDen ~ "Area Population density",
                                    FocalStatistics_OutRaster_Full7inNDVI042020200m ~ "NDVI"),
                       missing = "no", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  #modify_footnote(ends_with("-BG") ~ "Census Block group statistics",) %>%
  modify_spanning_header(starts_with("stat_") ~ "**GH Cluster**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()

## type and value for displaying dichotomous in one row
## Use tbl_stack to create group headers between sections of table

############################################################

######################TABLE 2##################################

cont_vars <- pg_filtered %>% select(age,bmi,sbpavg,dbpavg)
shapiro_test <- as.data.frame(sapply(cont_vars, shapiro.test))

# none of these are normally distributed
hist(participant_greenness$age)
hist(participant_greenness$bmi)
hist(participant_greenness$sbpavg)
hist(participant_greenness$dbpavg)

normalityTable <- data.frame(statistic = matrix(c(normalityVar1[1:4])))
normalityTable <- cbind(normalityVar1, p_val=matrix(c(normalityVar1[5:8])))
#boxcox()
t1 <- data.frame(statistic = matrix(normalityVar1[1:7]),
                 p_value = matrix(normalityVar1[8:14]))
normalityVar1
#anova


shapiro.test(df$Cn)

model <- lm(gend ~ NDVIcat, 
            data = participant_greenness, 
            na.action = na.exclude)

sstable <- Anova(model, type = 3) 
tbl_regression(model, label = NDVIcat1 ~ "Low NDVI")



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
