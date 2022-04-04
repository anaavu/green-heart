#######################################################################################
## Author: Anagha Uppal
## Date: Nov 2020-Feb 2021
## Purpose: Table 2 for SES 2nd Paper
## Inputs: participants, greenness
## Outputs: a csv file of all unadjusted correlations
#######################################################################################
## To do: 

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
library(rcompanion)
library(corrr)
library(devtools)
library(corrP)

######################PREP##################################
# Input data file locations
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_AU_20201221.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_allgreenness_20210204.csv")

# same cleanup code
participants <- participants %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>% 
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
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
  mutate(sdwlk.desc = as.factor(if_else(sdwlk==3 | sdwlk==4, 'Yes', 'No'))) %>%
  mutate(litt.desc = as.factor(if_else(litt==3 | litt==4, 'Yes', 'No'))) %>%
  mutate(saf1.desc = as.factor(if_else(saf1==1 | saf1==2, 'Yes', 'No'))) %>%
  mutate(saf2.desc = as.factor(if_else(saf2==1 | saf2==2, 'Yes', 'No'))) %>%
  mutate(saf3.desc = as.factor(if_else(saf3==1 | saf3==2, 'Yes', 'No'))) %>%
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)

## choose the useful columns for the table 2
participants <- participants %>% select(gend.desc,race.desc,ethn.desc,
                                        inc.desc,edu.desc,medu.desc,hznum.desc,
                                        exr.desc,rentown.desc,bc.desc,
                                        dm.desc,nowsmk.desc,litt.desc,sdwlk.desc,
                                        saf1.desc,saf2.desc,saf3.desc,age,bmi,
                                        NEAR_DIST_MajorRoads, LASTCNT_MajorRoadsTraffic,
                                        PopDen, stid)

## clean greenness table to be usable
# this divides numeric NDVI into low/med/high categorical 
greenness <- greenness %>%
  mutate(NDVIcat = ntile(FocalStatistics_OutRaster_Full7inNDVI042020200m, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))
# join greenness values to participants
participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
############################################################
# participant_greenness <- participant_greenness %>%
#   mutate(NDVIcat2 = ntile(Participant_200m_Mean_ndvi, 3)) %>%
#   mutate(NDVIcat2 = if_else(NDVIcat2 == 1, 'Low', 
#                             if_else(NDVIcat2 == 2, 'Medium', 'High'))) %>%
#   mutate(NDVIcat2 = factor(NDVIcat2) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(biomasscat = ntile(Participant_200m_Mean_biomass, 3)) %>%
#   mutate(biomasscat = if_else(biomasscat == 1, 'Low', 
#                               if_else(biomasscat == 2, 'Medium', 'High'))) %>%
#   mutate(biomasscat = factor(biomasscat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(laicat = ntile(Participant_200m_Mean_LAI, 3)) %>%
#   mutate(laicat = if_else(laicat == 1, 'Low', 
#                           if_else(laicat == 2, 'Medium', 'High'))) %>%
#   mutate(laicat = factor(laicat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(leafcat = ntile(Participant_200m_Mean_leafarea, 3)) %>%
#   mutate(leafcat = if_else(leafcat == 1, 'Low', 
#                            if_else(leafcat == 2, 'Medium', 'High'))) %>%
#   mutate(leafcat = factor(leafcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(leafcat = ntile(Participant_200m_Mean_leafarea, 3)) %>%
#   mutate(leafcat = if_else(leafcat == 1, 'Low', 
#                            if_else(leafcat == 2, 'Medium', 'High'))) %>%
#   mutate(leafcat = factor(leafcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(parcelcat = ntile(Parcel_ndvi_Mean, 3)) %>%
#   mutate(parcelcat = if_else(parcelcat == 1, 'Low', 
#                              if_else(parcelcat == 2, 'Medium', 'High'))) %>%
#   mutate(parcelcat = factor(parcelcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(nobuildingcat = ntile(Nobuilding_ndvi_Mean, 3)) %>%
#   mutate(nobuildingcat = if_else(nobuildingcat == 1, 'Low', 
#                                  if_else(nobuildingcat == 2, 'Medium', 'High'))) %>%
#   mutate(nobuildingcat = factor(nobuildingcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(lessvcat = ntile(lessvisible_ndvi_MEAN, 3)) %>%
#   mutate(lessvcat = if_else(lessvcat == 1, 'Low', 
#                             if_else(lessvcat == 2, 'Medium', 'High'))) %>%
#   mutate(lessvcat = factor(lessvcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(morevcat = ntile(morevisible_ndvi_MEAN, 3)) %>%
#   mutate(morevcat = if_else(morevcat == 1, 'Low', 
#                             if_else(morevcat == 2, 'Medium', 'High'))) %>%
#   mutate(morevcat = factor(morevcat) %>% fct_relevel(c("High", "Medium", "Low")))
# participant_greenness <- participant_greenness %>%
#   mutate(parcellaicat = ntile(Parcel_LAI_Mean, 3)) %>%
#   mutate(parcellaicat = if_else(parcellaicat == 1, 'Low', 
#                                 if_else(parcellaicat == 2, 'Medium', 'High'))) %>%
#   mutate(parcellaicat = factor(parcellaicat) %>% fct_relevel(c("High", "Medium", "Low")))
# blockgroup
# ggscatmat(greenness,columns = 1:ncol(greenness))

## pick the useless columns to get rid of
sapply(participant_greenness, function(x) sum(is.na(x)))
drop <- c("Field", "FocalStatistics_OutRaster_Full7inNDVI052620500m", "stid",
          "USER_Home_Address", "OBJECTID_1", "X", "Y", "SubjectID", "ObjectID",
          "Subject_ID", "Home_Address", "Study_ID", "AndreaCanopyPercFocal500m052920",
          "AndreaCanopyPercFocal400m060320", "AndreaCanopyPercFocal300m052920",
          "AndreaCanopyPercFocal100m060320", "AndreaCanopyPercFocalTest50m052920c",
          "ImperviousyPercFocal500m060320", "ImperviousyPercFocal300m060320",
          "ImperviousyPercFocal50m060320", "ParcelID_biomass", "FIPS_biomass",
          "ParcelID_ndvi", "FIPS_ndvi", "ParcelID_leafarea", "FIPS_leafarea",
          "ParcelID_LAI", "FIPS_LAI", "MEAN_12", "ParcelID_NDVI_L19", 
          "FIPS_NDVI_L19", "Event_Name", "OBJECTID", "ghKDE20mLeafArea",
          "ghKDE20mLAI", "ghKDE20mBiomass", "ghKDE20mBiomass_1", "ghKDE50mBiomass",
          "ghKDE50mLeafArea", "ghKDE50mLAI", "ghKDE500mLeafArea", "ghKDE500mLAI",
          "ghKDE500mBiomass", "HealParcelsNDVI061520", 
          "FocalStatistics_OutRaster_Full7inNDVI05262050m", 
          "FocalStatistics_OutRaster_Full7inNDVI042020100m",
          "FocalStatistics_OutRaster_Full7inNDVI042020200m",
          "FocalStatistics_OutRaster_Full7inNDVI042020300m",
          "HEALLessVisbParcelNDVI061520b", "HEALMoreVisbParcelNDVI061520b",
          "HEALLessVisbParcelNDVI061520b_1","HEALMoreVisbParcelNDVI061520b_1",
          "NEAREST_MAJORROAD_FID")
pg_for_corr = participant_greenness[,!(names(participant_greenness) %in% drop)]
## creates an unadjusted corr table and outputs wherever you choose in "write.csv"
pg_corr = corrP(pg_for_corr,parallel = TRUE, n.cores = 4, p.value = 0.05)
corrplot::corrplot(pg_corr)
corrgram::corrgram(pg_corr)
pgH = rh_corrP(df=pg_for_corr,corrmat=pg_corr,cutoff=0.5)
write.csv(pg_corr, "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/all_correlations.csv")

######################SUBSET CORR##############################
pg_for_corr_local = pg_for_corr %>% select(-starts_with("Participant_20m"), 
                                             -starts_with("Parcel_"),
                                             -starts_with("Nobuilding_"),
                                             -starts_with("lessvisible_"),
                                             -starts_with("morevisible_"))
pg_corr_local = corrP(pg_for_corr_local,parallel = TRUE, n.cores = 4, p.value = 0.05)
corrplot::corrplot(pg_corr_local)
corrgram::corrgram(pg_corr_local)
write.csv(pg_corr_local, "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/all_correlations_local.csv")


pg_for_corr_personal = pg_for_corr %>% select(-contains("00m_"), 
                                          -contains("50m_"),
                                          -starts_with("Blockgroup_"))
pg_corr_personal = corrP(pg_for_corr_personal,parallel = TRUE, n.cores = 4, p.value = 0.05)
corrplot::corrplot(pg_corr_personal)
corrgram::corrgram(pg_corr_personal)
write.csv(pg_corr_personal, "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/all_correlations_personal.csv")


######################TABLE 2##################################

cont_vars <- pg_filtered %>% select(age,bmi,sbpavg,dbpavg)
shapiro_test <- as.data.frame(sapply(cont_vars, shapiro.test))

######################STOP HERE##################################
##################move to SES_table_3_model_greenness.R#########################



library(regclass)
model <- glm(NDVIcat ~ gend + age + bmi + nowsmk, 
             data = participant_greenness, 
             na.action = na.exclude,
             family = "binomial")

library(car)
sstable <- Anova(model, type = 3) 

tbl_regression(model, label = gend ~ "Gender")


######################OTHER##################################



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
