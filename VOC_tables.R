#######################################################################################
## Author: Anagha Uppal
## Date: Nov 2020-Feb 2021
## Purpose: Tables for VOC 2nd Paper
## Inputs: 
## Outputs: 
#######################################################################################
## To do: 

## Install packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("purrr")) install.packages("purrr")
if (!require("EnvStats")) install.packages("EnvStats")
if (!require("broom")) install.packages("broom")
if (!require("factoextra")) install.packages("factoextra")


library(ggplot2)
library(dplyr)
library(gtsummary)
library(purrr)
library(EnvStats)
library(broom)
library(tidyverse)
library(factoextra)
library()

######################PREP##################################
# Input data file locations
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_AU_20201221.csv")
# greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/HEALgreennessMetrics_RY_110920.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_allgreenness_20210204.csv")

participants <- participants %>%
  subset(u_cot_n <= 40) %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>%
  mutate(exr.desc = as.factor(if_else(exr == 1, 'Yes', 
                                      if_else(exr == 2, 'No', NULL)))) %>%
  mutate(obese.desc = as.factor(if_else(bmi >= 30, 'Yes', 
                                        if_else(bmi < 30, 'No', NULL)))) %>%
  mutate(lip.desc = as.factor(if_else(lip == 1, 'Yes', 
                                      if_else(lip == 2, 'No', NULL)))) %>%
  mutate(dm.desc = as.factor(if_else(dm == 1, 'Yes', 
                                     if_else(dm == 2, 'No', NULL)))) %>%
  mutate(htn.desc = as.factor(if_else(htn == 1, 'Yes', 
                                      if_else(htn == 2, 'No', NULL)))) %>%
  mutate(hxmi.desc = as.factor(if_else(hxmi == 1, 'Yes', 
                                       if_else(hxmi == 2, 'No', NULL)))) %>%
  mutate(hxcva.desc = as.factor(if_else(hxcva == 1, 'Yes', 
                                        if_else(hxcva == 2, 'No', NULL)))) %>%
  mutate(cabg.desc = as.factor(if_else(cabg == 1, 'Yes', 
                                       if_else(cabg == 2, 'No', NULL)))) %>%
  mutate(ptca.desc = as.factor(if_else(ptca == 1, 'Yes', 
                                       if_else(ptca == 2, 'No', NULL)))) %>%
  mutate(hxchf.desc = as.factor(if_else(hxchf == 1, 'Yes', 
                                        if_else(hxchf == 2, 'No', NULL)))) %>%
  mutate(hxcad.desc = as.factor(if_else(hxcad == 1, 'Yes', 
                                        if_else(hxcad == 2, 'No', NULL)))) %>%
  mutate(cea.desc = as.factor(if_else(cea == 1, 'Yes', 
                                      if_else(cea == 2, 'No', NULL)))) %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL)))))) %>%
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(hznum.desc = as.factor(if_else(hznum==1 | hznum==2, 'Small',
                                        if_else(hznum>=3 & hznum<=5, 'Medium',
                                                if_else(hznum>=6, 'Large', NULL))))) %>%
  mutate(u_mu_n = as.numeric(u_mu_n)) %>%
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)

greenness <- greenness %>%
  mutate(NDVIcat = ntile(FocalStatistics_OutRaster_Full7inNDVI042020200m, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
############################################################
############################################################
############################################################
######################TABLE 1###############################

pg_VOC_t1 <- participant_greenness %>% dplyr::select(gend.desc,race.desc,exr.desc,
                                                     obese.desc,lip.desc,
                                                     dm.desc, htn.desc,
                                                     hxmi.desc,hxcva.desc,cabg.desc,
                                                     ptca.desc,hxchf.desc,hxcad.desc,
                                                     cea.desc,inc.desc,edu.desc,hznum.desc,
                                                     age,bmi,
                                                     sbpavg,dbpavg,
                                                     NEAR_DIST_MajorRoads,
                                                     LASTCNT_MajorRoadsTraffic,PopDen,
                                                     NDVIcat)
pg_VOC_t1 %>% tbl_summary(by = NDVIcat,
                          statistic = list(all_continuous() ~ "{mean} ({sd})",
                                           all_categorical() ~ "{n} / {N} ({p}%)"),
                          digits = all_continuous() ~ 2,
                          label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                       exr.desc ~ "Exercise",
                                       obese.desc ~ "Obese",
                                       lip.desc ~ "Hyperlipidemia", 
                                       dm.desc ~ "Diabetes", htn.desc ~ "Hypertension",
                                       hxmi.desc ~ "Myocardial Infarction",
                                       hxcva.desc ~ "Stroke", 
                                       cabg.desc ~ "CABG", 
                                       hznum.desc ~ "Household size",
                                       ptca.desc ~ "PCI/Stents",
                                       hxchf.desc ~ "Heart Failure", 
                                       hxcad.desc ~ "Angina", age ~ "Age",
                                       bmi ~ "BMI", sbpavg ~ "Systolic BP", dbpavg ~ "Diastolic BP",
                                       inc.desc ~ "Income", edu.desc ~ "Education",
                                       cea.desc ~ "CEA",
                                       NEAR_DIST_MajorRoads ~ "Distance to major road",
                                       LASTCNT_MajorRoadsTraffic ~ "Traffic count",
                                       PopDen ~ "Area Population density"),
                          missing = "no", #can switch to ifany
                          missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()

## type and value for displaying dichotomous in one row
## Use tbl_stack to create group headers between sections of table
## theme of table gt::tab_header("Journal Theme (JAMA)") jama or lancet
## gt::tab_header("Compact Theme")


############################################################
############################################################
######################Prep for TABLE 2##################################

cont_vars <- pg_filtered %>% select(age,bmi,sbpavg,dbpavg)
shapiro_test <- as.data.frame(sapply(cont_vars, shapiro.test))

# none of these are normally distributed
hist(participant_greenness$age)
hist(participant_greenness$bmi)
hist(participant_greenness$sbpavg)
hist(participant_greenness$dbpavg)

lapply(c())
boxcox(participant_greenness$age) # No transform
boxcox(participant_greenness$bmi, optimize = TRUE) # reciprocal square root
boxcox(participant_greenness$sbpavg, optimize = TRUE)
boxcox(participant_greenness$dbpavg, optimize = TRUE)
boxcox_test <- as.data.frame(sapply(cont_vars, boxcox))


normalityTable <- data.frame(statistic = matrix(c(normalityVar1[1:4])))
normalityTable <- cbind(normalityVar1, p_val=matrix(c(normalityVar1[5:8])))
t1 <- data.frame(statistic = matrix(normalityVar1[1:7]),
                 p_value = matrix(normalityVar1[8:14]))
normalityVar1


shapiro.test(df$Cn)

model <- lm(gend ~ NDVIcat, 
            data = participant_greenness, 
            na.action = na.exclude)

sstable <- Anova(model, type = 3) 
tbl_regression(model, label = NDVIcat1 ~ "Low NDVI")
#############################################################
############################################################
############################################################
######################TABLE 2##################################
pg_VOC_t2 <- participant_greenness %>% dplyr::select(u_cema_n,u_3hpma_n,u_aama_n,u_cyma_n,
                                                     u_mu_n,u_bpma_n,u_dhbma_n,u_mhbma3_n,
                                                     u_hpmma_n,u_amcc_n,u_pga_n,
                                                     u_2hpma_n,u_phema_n,u_ma_n,
                                                     u_bma_n,u_2mha_n,u_34mha_n,NDVIcat)
pg_VOC_t2$u_mu_n[pg_VOC_t2$u_mu_n == ""] <- NA
pg_VOC_t2$u_mu_n <- as.numeric(pg_VOC_t2$u_mu_n)
pg_VOC_t2 %>% tbl_summary(by = NDVIcat,
                          statistic = list(all_continuous() ~ "{mean} ({sd})",
                                           all_categorical() ~ "{n} / {N} ({p}%)"),
                          digits = all_continuous() ~ 2,
                          label = list(u_cema_n ~ "CEMA", u_3hpma_n ~ "3HPMA", 
                                       u_aama_n ~ "AAMA", u_cyma_n ~ "CYMA", 
                                       u_mu_n ~ "MU", u_bpma_n ~ "BPMA",
                                       u_dhbma_n ~ "DHBMA", u_mhbma3_n ~ "MHBMA3",
                                       u_hpmma_n ~ "HPMMA", u_amcc_n ~ "AMCC",
                                       u_pga_n ~ "PGA", u_2hpma_n ~ "2HPMA", 
                                       u_phema_n ~ "PHEMA", u_ma_n ~ "MA",
                                       u_bma_n ~ "BMA", u_2mha_n ~ "MHA",
                                       u_34mha_n ~ "3MHA+4MHA"),
                          missing = "no", #can switch to ifany
                          missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()


library(regclass)
model <- glm(NDVIcat ~ gend + age + bmi + nowsmk, 
             data = participant_greenness, 
             na.action = na.exclude,
             family = "binomial")

library(car)
sstable <- Anova(model, type = 3) 

tbl_regression(model, label = gend ~ "Gender")

############################################################
############################################################
############################################################
######################TABLE 3##################################
pg_VOC_t2$NDVI_log <- log10(pg_VOC_t2$FocalStatistics_OutRaster_Full7inNDVI042020200m)
model <- lm(NDVI_log ~ inc * edu,
            data = pg_VOC_t2, 
            na.action = na.exclude)
############################################################
############################################################
######################OTHER##################################
model <- glm(NDVIcat ~ inc * edu,
             data = participant_greenness, 
             na.action = na.exclude,
             family = "binomial")

############

#princomp(na.omit(pg_VOC_t2[1:17]), cor = FALSE, scores = TRUE)
res.pca <- prcomp(na.omit(pg_VOC_t2[1:17]), scale = FALSE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

forpcatable <- pg_VOC_t2[1:16]
res.pca <- prcomp(na.omit(forpcatable), scale = FALSE)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)