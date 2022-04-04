#######################################################################################
## Author: Anagha Uppal
## Date: Nov 2020-Feb 2021
## Purpose: Table 3 for VOC 2nd Paper
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
library(tidyverse)
library(broom)
library(reghelper)
library(effectsize)


# Grab the data
participants <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHData_AU_20201221.csv")
participants <- participants[-213,]
shemp <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GH_shemp_variable.csv")

greenness <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/participants_allgreenness_20210214.csv")
greenness <- greenness[-256,]
greenness20mLA <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GH_newLA_20m.csv")[, c(7, 64, 72, 151, 159, 235, 243, 591)]
greenness20mLA$New_LA_20m <- greenness20mLA$New_LA
greenness20mLA <- greenness20mLA[ -c(8) ]
greenness200mLA <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GH_200m_newLA.csv")[, c(7, 589)]
greenness200mLA$New_LA_200m <- greenness200mLA$New_LA
greenness200mLA <- greenness200mLA[ -c(2) ]

datacanopy2 <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/participants_allgreenness_20210809.csv")[, c(5, 178:199)]
datacanopy2 <- datacanopy2[-256,]

dataadd <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHDataAdd_RY_20210727.csv")[, c(5, 178:199)]

trafficdata1 <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficGISData_AU_20210224.csv")
trafficdata2 <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficGISData_AU_20210304.csv")
trafficdata3 <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_addTrafficExposure_AU_20210630.csv")
trafficdata3$StudyID <- trafficdata3$ï..StudyID

addtm <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/addtm_column.csv")
climatedata <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/HEAL_climate_data_DR_062821.csv")

addcols <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHDataAdd_RY_20210628.csv")
expdata <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHDataAdd_RY_20210727.csv")
rentown_new <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GHParticipants_newrentown.csv")
rentown_new <- rentown_new %>%
  mutate(propval_cat = ntile(CUR_TOTAL, 3)) %>%
  mutate(propval_cat = if_else(propval_cat == 1, 'Low', if_else(propval_cat == 2, 'Medium', 'High'))) %>%
  arrange(propval_cat)
# climatedata[duplicated(climatedata$stid), ]
parcelinfo <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/GH_Parcels.csv")


SVI <- read.csv("E:/GIS/Local_Computer_Stuff/GIS/GH/participant_data/ghgeocodes_SVI.csv")

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
  mutate(cope.desc = as.factor(if_else(cope == 0, 'Never', 
                                      if_else(cope == 1, 'Almost never', 
                                              if_else(cope == 2, 'Sometimes',
                                                      if_else(cope==3 | cope==4, 'Fairly Often or Often', NULL))))) %>%
    fct_relevel(c("Never", "Almost never", "Sometimes", "Fairly Often or Often"))) %>%
  mutate(etoh1.desc = as.factor(if_else(etoh1 == 1 | etoh1 == 2, 'Less than monthly', 
                                       if_else(etoh1 == 3, '2 to 4 times a month', 
                                               if_else(etoh1 == 4, '2 to 3 times a week',
                                                       if_else(etoh1==5, '4 or more times a week', NULL))))) %>%
    fct_relevel(c("Less than monthly", "2 to 4 times a month", "2 to 3 times a week", "4 or more times a week"))) %>%
  mutate(lip.desc = as.factor(if_else(lip == 2 | lip == 3, 'No', 
                                      if_else(lip == 1, 'Yes', NULL)))) %>%
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
  mutate(dm.desc = as.factor(if_else(dm == 1, 'Yes', 
                                     if_else(dm == 2, 'No', NULL)))) %>%
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

shemp <- shemp %>%
  mutate(employed.desc = as.factor(if_else(shemp == 1, "Yes",
                                       if_else(shemp == 0, "No",NA_character_))))

participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))
participant_greenness <- safe_left_join(participant_greenness, greenness20mLA, by=c("stid" = "StudyID"),
                                        conflict = coalesce)
participant_greenness <- safe_left_join(participant_greenness, greenness200mLA, by=c("stid" = "StudyID"),
                                        conflict = coalesce)
participant_greenness <- safe_left_join(participant_greenness, datacanopy2, by=c("stid" = "StudyID"),
                                        conflict = coalesce)
participant_greenness$de <- as.Date(mdy(participant_greenness$de))
participant_greenness <- left_join(participant_greenness, shemp, by=c("stid"="stid"))
participant_greenness  <- left_join(participant_greenness, trafficdata1, by=c("stid"="StudyID"))
participant_greenness  <- safe_left_join(participant_greenness, trafficdata2, by=c("stid"="StudyID"),
                                         conflict = coalesce)
participant_greenness$Roadlength[is.na(participant_greenness$Roadlength)] = 0
participant_greenness$RoadlengthtimesTraffic[is.na(participant_greenness$RoadlengthtimesTraffic)] = 0
participant_greenness  <- left_join(participant_greenness, trafficdata3, by=c("stid"="StudyID"))

participant_greenness  <- left_join(participant_greenness, addtm, by=c("stid"="stid"))
#participant_greenness  <- left_join(participant_greenness, addcols, by=c("stid"="stid"))
participant_greenness  <- safe_left_join(participant_greenness, expdata, by=c("stid"="stid"),
                                         conflict = coalesce)
participant_greenness  <- left_join(participant_greenness, climatedata, by=c("stid"="stid"))
participant_greenness  <- left_join(participant_greenness, rentown_new, by=c("stid"="StudyID"))
participant_greenness  <- safe_left_join(participant_greenness, parcelinfo, by=c("PARCELID"="PARCELID"),
                                         conflict = coalesce)


# participant_greenness[,261:262] <- sapply(participant_greenness[,261:262],as.numeric) # u_mu
participant_greenness  <- safe_left_join(participant_greenness, SVI, by=c("stid"="StudyID"),
                                         conflict = coalesce)

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

# participant_greenness <- participant_greenness %>%
#   mutate(SPL_THEMES_cat = ntile(SPL_THEMES, 2)) %>%
#   mutate(SPL_THEMES_cat = if_else(SPL_THEMES_cat == 1, 'Low', 'High')) %>%
#   mutate(SPL_THEMES_cat = factor(SPL_THEMES_cat) %>% fct_relevel(c("Low", "High")))

participant_greenness <- participant_greenness %>%
  mutate(SPL_THEMES_cat = if_else(SPL_THEMES < 9.392, 'Low', 'High')) %>%
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

nonsmokers <- participant_greenness %>% filter(nowsmk.desc=="No")
smokers <- participant_greenness %>% filter(nowsmk.desc=="Yes")

participant_greenness$Participant_200m_Mean_NDVI_L19
ggplot(gather(nonsmokers[,182:208]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


# taking all variables from SES table 
# checking the NA count for each
# and removing rentown.desc and bc.desc (type of building) 
# these two variables have n=300 missing values
pg_for_lm <- participant_greenness %>% dplyr::select(de,Parcel_ndvi_Mean,Participant_300m_Mean_ndvi,
                                                     gend.desc,race.desc,ethn.desc,
                                                     exr.desc,Participant_50m_Mean_ndvi,
                                                     #rentown.desc,bc.desc,
                                                     age,bmi,Participant_100m_Mean_NDVI_L19,Participant_50m_Mean_NDVI_L19,
                                                     NEAR_DIST_MajorRoads, LASTCNT_MajorRoadsTraffic,
                                                     u_aama_n,u_dhbma_n,u_amcc_n,
                                                     u_ma_n,u_cyma_n,u_cema_n,u_mu_n,u_cot_n) 
                                                     #crf, copd, hep, exp)
#PopDen)

## removing rentown=333 NA
drop <- c("rentown.desc")
pg_for_lm = pg_for_lm[,!(names(pg_for_lm) %in% drop)]
d <- sapply(pg_for_lm, function(x) sum(is.na(x)))
pg_for_lm <- na.omit(pg_for_lm) #574 observations

ggscatmat(pg_for_lm, columns = 1:ncol(pg_for_lm))

# ndvi <- bg level
full.model <- glm(Participant_300m_Mean_ndvi ~(gend.desc+race.desc+ethn.desc+
                                               exr.desc+age+bmi+u_aama_n+u_dhbma_n+
                                               u_ma_n+u_cyma_n+u_cema_n+u_mu_n+u_cot_n), data = pg_for_lm,
                  family = gaussian())
model <- lm(Participant_300m_Mean_NDVI_2016 ~Participant_300m_Mean_ndvi, data = participant_greenness, family = gaussian())
participant_greenness$Participant_300m_Mean_NDVI_L19
participant_greenness$Participant_50m_Mean_ndvi
model <- lm(sbpavg ~Participant_300m_Mean_ndvi+age+nowsmk+
              gend+race+obese.desc, data = participant_greenness)
ggplot(participant_greenness, aes(x=sbpavg, y=age)) + geom_point() + geom_smooth(method = lm)
ggplot(participant_greenness, aes(x=as.factor(htn), y=Participant_300m_Mean_ndvi)) + geom_boxplot()

p<-ggplot(participant_greenness, aes(x=as.factor(htn), y=Participant_300m_Mean_ndvi)) + 
  geom_dotplot(binaxis='y', stackdir='center')
# ndvi <- stuff
# Fit the full model 
full.model <- glm(Parcel_ndvi_Mean ~(gend.desc+race.desc+ethn.desc+
                                       inc.desc+edu.desc+medu.desc+hznum.desc+exr.desc+
                                       sdwlk.desc+litt.desc+saf1.desc+saf2.desc+bc.desc+
                                       saf3.desc+bmi+NEAR_DIST_MajorRoads+age+
                                       LASTCNT_MajorRoadsTraffic+PopDen), data = pg_for_lm,
                  family = gaussian())
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
bootstep.model <- boot.stepAIC(full.model, pg_for_lm, B = 100, alpha = 0.05, 
                               direction = "both", k = 2, verbose = TRUE)
summary(step.model) #inc, exr, litt, neardist, lastcnt
summary(bootstep.model)
bootstep.model

full.model2 <- glm(Parcel_ndvi_Mean ~gend.desc+race.desc+ethn.desc+
                     age+NEAR_DIST_MajorRoads+
                     race.desc*NEAR_DIST_MajorRoads+
                     ethn.desc*NEAR_DIST_MajorRoads, data = pg_for_lm,
                   family = gaussian())
step.model2 <- stepAIC(full.model2, direction = "both", trace = FALSE)
summary(step.model2) #
summary(full.model2)

## Visualization
fit.boot <- bootstep.model
nBoot <- summary(fit.boot)[8,1]
origModel <- paste(names(coef(fit.boot$OrigModel)), collapse = " + ")
stepModel <- paste(names(coef(fit.boot$OrigStepAIC)), collapse = " + ")
# Names of covariates
covariates <- rownames(fit.boot$Covariates)
nCovariates <- length(covariates)
# Matrix of number of times each covariate was picked
coef.pick <- fit.boot$Covariates
# Matrix for the consistency of sign on each covariate
coef.sign <- fit.boot$Sign
# Change names since many are factors
rownames(coef.sign)[1] <- "exr.desc"
rownames(coef.sign)[5] <- "inc.desc"
rownames(coef.sign)[6] <- "saf3.desc"
rownames(coef.sign)[7] <- "edu.desc"
rownames(coef.sign)[8] <- "saf2.desc"
rownames(coef.sign)[9] <- "obese.desc"
rownames(coef.sign)[10] <- "ethn.desc"
rownames(coef.sign)[13] <- "gend.desc"
rownames(coef.sign)[14] <- "race.desc"
rownames(coef.sign)[15] <- "race.desc"
rownames(coef.sign)[16] <- "medu.desc"
rownames(coef.sign)[17] <- "hznum.desc"
rownames(coef.sign)[18] <- "sdwlk.desc"
rownames(coef.sign)[20] <- "saf1.desc"
rownames(coef.sign)[21] <- "hznum.desc"
rownames(coef.sign)[22] <- "inc.desc"
rownames(coef.sign)[23] <- "inc.desc"
rownames(coef.sign)[25] <- "litt.desc"
rownames(coef.sign)[26] <- "medu.desc"

coef.sign <- coef.sign[match(rownames(coef.pick), rownames(coef.sign)),]
# Matrix for statistical significance
coef.stat <- fit.boot$Significance
# Change name for "chas" since it is a factor
rownames(coef.stat)[2] <- "litt.desc"
rownames(coef.stat)[4] <- "medu.desc"
rownames(coef.stat)[5] <- "hznum.desc"
rownames(coef.stat)[6] <- "saf2.desc"
rownames(coef.stat)[8] <- "exr.desc"
rownames(coef.stat)[9] <- "saf3.desc"
rownames(coef.stat)[10] <- "race.desc"
rownames(coef.stat)[11] <- "ethn.desc"
rownames(coef.stat)[12] <- "edu.desc"
rownames(coef.stat)[13] <- "hznum.desc"
rownames(coef.stat)[14] <- "gend.desc"
rownames(coef.stat)[15] <- "sdwlk.desc"
rownames(coef.stat)[16] <- "inc.desc"
rownames(coef.stat)[17] <- "obese.desc"
rownames(coef.stat)[21] <- "race.desc"
rownames(coef.stat)[22] <- "inc.desc"
rownames(coef.stat)[23] <- "inc.desc"
rownames(coef.stat)[24] <- "saf1.desc"
rownames(coef.stat)[25] <- "medu.desc"
rownames(coef.stat)[26] <- "edu.desc"


coef.stat <- coef.stat[match(rownames(coef.pick), rownames(coef.stat)),]
# Make into long form for charting later
coef.stat.long <- data.frame()
for(i in 1:length(coef.stat)){
  n <- round(coef.stat[i],0)
  vec <- seq(0, n, by = 2)
  mat <- data.frame(rep(names(coef.stat)[i], length(vec)), vec, paste("% Sig", n))
  names(mat) <- c("variable", "sig", "text")
  
  # We'll use mode = "line". NA helps separate line segments
  coef.stat.long <- rbind(coef.stat.long, mat, c(NA, NA))
}
# Convert to dataframes
coef.pick <- as.data.frame(coef.pick)
coef.stat <- as.data.frame(coef.stat)
coef.sign <- as.data.frame(coef.sign)
names(coef.pick) <- "pick"
names(coef.sign) <- c("pos", "neg")
names(coef.stat) <- "stat"



# Base plot for number of times a variable was picked by stepAIC
plot_ly(coef.pick, x = rownames(coef.pick), y = pick,
        type = "bar", opacity = 0.75, name = "Times picked (%)",
        hoverinfo = "text", text = pick.text,
        marker = list(color = "#00994d", line = list(width = 2))) %>% 
  
  # Layer for number of times a variable was statistically significant at 5%
  add_trace(data = coef.stat.long, x = variable, y = sig, 
            type = "scatter", mode = "markers + line", name = "Stat. Sig (%)",
            line = list(color = "#ffdb4d", width = 15),
            hoverinfo = "text", text = text) %>% 
  
  # Layer for number of times a variable's coefficient was positive
  add_trace(data = coef.sign, x = rownames(coef.pick), y = rep(-5, nCovariates), 
            type = "scatter", mode = "markers", name = "Coef Sign(% pos)",
            marker = list(symbol = "triangle-up", size = pos/scale, color = "#4da6ff",
                          line = list(color = "black", width = 2)),
            hoverinfo = "text", text = sign.text.up) %>% 
  
  # Layer for number of times a variable's coefficient was negative
  add_trace(data = coef.sign, x = rownames(coef.pick), y = rep(-10, nCovariates), 
            type = "scatter", mode = "markers", name = "Coef Sign(% neg)",
            marker = list(symbol = "triangle-down", size = neg/scale, color = "#ff704d",
                          line = list(color = "black", width = 2)),
            hoverinfo = "text", text = sign.text.down) %>% 
  
  # Layout, annotations, axis options etc
  layout(xaxis = list(title = "<b>Covariates</b>"),
         yaxis = list(title = "<b>Percentage(%)</b>",
                      tickmode = "array", 
                      tickvals = round(seq(0, 100, length.out = 10), 0),
                      domain = c(0.2, 1)),
         plot_bgcolor = "#e1efc3",
         paper_bgcolor = "#e1efc3",
         
         annotations = list(
           list(x = 0.1, y = 1, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top",
                ax = 0, ay = 0,
                text = "Visualizing <em>boot.stepAIC()</em>",
                font = list(family = "serif", size = 30)),
           
           list(x = 0.3, y = 0.1, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top",
                ax = 0, ay = 0,
                text = paste("<em>Original Model:</em>", origModel),
                font = list(family = "PT Sans Narrow", size = 15)),
           
           list(x = 0.21, y = 0.05, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top", align = "left",
                ax = 0, ay = 0,
                text = paste("<em>Stepwise Model:</em>", stepModel),
                font = list(family = "PT Sans Narrow", size = 15)),
           
           list(x = 0.8, y = 0.90, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top", align = "left",
                ax = 0, ay = 0,
                text = paste0("<em>No. of Covariates:</em>", nCovariates, "<br>",
                              "<em>No. of bootstrap samples:</em>", nBoot, "<br>"),
                font = list(family = "PT Sans Narrow", size = 15))
         ))
