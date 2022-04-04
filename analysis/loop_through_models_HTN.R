library(tidyverse)
library(broom)
library(reghelper)
library(effectsize)

# income <- green
# Fit the full model 
# full.model <- multinom(inc.desc ~., data = pg_for_lm)
# summary(full.model)
full.model <- glm(inc.desc ~., data = pg_for_lm, family=binomial())
# Stepwise regression model
# step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
# Skipping stepwise and bootstrap just in the Rmd because it is computationally intensive, not appropriate for markdown files
# bootstep.model <- boot.stepAIC(full.model, pg_for_lm, B = 100, alpha = 0.05, 
#                                direction = "both", k = 2, verbose = TRUE)
print(summary(full.model))
# print(summary(step.model)) #inc, exr, litt, neardist, lastcnt
# print(summary(bootstep.model))
# print(bootstep.model)

print(anova(full.model, test="Chisq"))
# McFadden's pseudo-R2
print(pR2(full.model))

anova(full.model, full.model1)
#vcov/influence() 

participants_greenness <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/participants_greenness_AU_063021.csv")
htndata <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/htndata.csv")
htndata2 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/htndata2.csv")
htndata_filtered <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/htndata_filtered.csv")

participants_greenness <- participants_greenness %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>% 
  mutate(ethn.desc = as.factor(if_else(ethn == 1, 'Hispanic/Latino', 'Non Hispanic/Latino'))) %>%
  #mutate(shemp.desc = as.factor(if_else(shemp == 1, 'Yes', 'No'))) %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL)))))) %>%
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(rentown.desc = as.factor(if_else(rentown == 1, 'Rent', 'Own'))) %>%
  mutate(bc.desc = as.factor(if_else(bc>=1 & bc<=4, 'Single home or duplex',
                                     if_else(bc==5 | bc==6, 'Apartment', NULL)))) %>%
  mutate(exr.desc = as.factor(if_else(exr == 1, 'Yes', 
                                      if_else(exr == 2, 'No', NULL)))) %>%
  mutate(obese.desc = as.factor(if_else(bmi >= 30, 'Yes', 
                                        if_else(bmi < 30, 'No', NULL)))) %>%
  mutate(nowsmk.desc = as.factor(if_else(u_cot_n > 40, 'Yes', 'No'))) %>%
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


##########################################################

# stargazer(pg_lm_personal, pg_lm_local, title = "Results", align = TRUE,
#           type = "text")

model <- lm(PopDen ~inc.desc+edu.desc+race.desc+gend.desc, 
            data = pg_for_lm)

write.table(colnames(participant_greenness),file = "C:/Users/a0uppa01/Documents/GIS/pgcolnames.csv")
bc_home <- participant_greenness %>% filter(bc.desc == "Single home or duplex")
bc_rent <- participant_greenness %>% filter(bc.desc == "Apartment")
bc_home_filtered <- bc_home %>% filter(CUR_TOTAL < 700000 & CUR_TOTAL > 0 & Parcel_ndvi_Sum < 90000 & Parcel_canopy_Sum < 100000)

evry1 <- participant_greenness

meds <- participants_greenness %>% filter(anyDiurACECCB=="Yes")
nomeds <- participants_greenness %>% filter(anyDiurACECCB=="No")

diabetes <- participants_greenness %>% filter(dmorHbA1c=="Yes")
nodiabetes <- participants_greenness %>% filter(dmorHbA1c=="No")

smokers <- participants_greenness %>% filter(nowsmk.desc=="Yes")
nonsmokers <- participants_greenness %>% filter(nowsmk.desc=="No")

smokers1 <- participants_greenness %>% filter(nowsmk.desc=="Yes")
nonsmokers1 <- participants_greenness %>% filter(nowsmk.desc=="No")

smokers2 <- htndata2 %>% filter(nowsmk.desc=="Yes")
nonsmokers2 <- htndata2 %>% filter(nowsmk.desc=="No")

home <- participants_greenness %>% filter(bc.desc == "Single home or duplex")

modelx <- glm(sbpavg ~Participant_50m_Mean_ndvi+inc.desc+edu.desc+
               race.desc+PopDen+age+gend.desc, data=participant_greenness)
+
               (edu.desc*inc.desc)+as.factor(cope)+bmi+as.factor(etoh1)+as.factor(dmorHbA1c)+as.factor(lip)+as.factor(anyDiurACECCB), 
             data = participant_greenness, subset= nowsmk.desc=="Yes")
beta(modelx, x = FALSE, y = TRUE)
modely <- lm(Participant_300m_Mean_ndvi ~inc.desc+edu.desc+
               race.desc+NEAR_DIST_MajorRoads+hznum+medu.desc+CUR_TOTAL,
             data = pg_for_lm)
model_test <- lm(Participant_300m_Mean_ndvi ~inc.desc,
                 data = participant_greenness)

resultdf = data.frame(a=1:10)
colnames_green <- colnames(participant_greenness)
colnames_green <-  colnames_green[346:510]
#colnames_green <-  colnames_green[-1:-8]
setwd("I:/Backups/GIS/Local_Computer_Stuff/GIS/models/htn_smk_model3")
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
         
         
         formula <- as.formula(paste("sbpavg ~ ", var, "+race.desc+age+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+bmi+as.factor(etoh1)+as.factor(dmorHbA1c)+as.factor(lip)+exr.desc"))
         model1 <- glm(formula, data = participant_greenness)
         conf1 <- confint(model1)
         write.csv(tidy(model1), paste0(var,".csv"))
         write.csv(conf1, paste0(var,"_confint.csv"))
         toselect.x <- subset(tidy(model1), p.value < 0.05)
         write.csv(toselect.x, paste0(var, "_selected.csv"))
       })

summary(pg.model)$coeff[-1,4] < 0.05
summary(pg.model)[summary(pg.model)$coeff[-1,4] < 0.05,]
subset(tidy(pg.model), p.value < 0.05)
filter(tidy(pg.model), p.value < 0.05)

summary(participants$Pop)