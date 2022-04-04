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

##########################################################

# stargazer(pg_lm_personal, pg_lm_local, title = "Results", align = TRUE,
#           type = "text")

model <- lm(PopDen ~inc.desc+edu.desc+race.desc+gend.desc, 
             data = pg_for_lm)

write.table(colnames(participant_greenness),file = "C:/Users/a0uppa01/Documents/GIS/pgcolnames.csv")
bc_home <- participant_greenness %>% filter(bc.desc == "Single home or duplex")
bc_home_filtered <- bc_home %>% filter(CUR_TOTAL < 700000 & CUR_TOTAL > 0 & Parcel_ndvi_Sum < 90000 & Parcel_canopy_Sum < 100000)
bc_apt <- participant_greenness %>% filter(bc.desc == "Apartment")
bc_apt_filtered <- bc_apt %>% filter(Parcel_ndvi_Sum < 90000)
bc_own <- participant_greenness %>% filter(rentown_parcel == "Own" & CUR_TOTAL > 0)
bc_own_filtered <- bc_own %>% filter(Parcel_ndvi_Sum < 50000)
bc_rent <- participant_greenness %>% filter(rentown_parcel == "Rent")
bc_rent_filtered <- bc_rent %>% filter(Parcel_ndvi_Sum < 8000)
bc_homeown <- bc_home_filtered %>% filter(rentown_parcel == "Own")
bc_homerent <- bc_home_filtered %>% filter(rentown_parcel == "Rent")
bc_nonhomeown <- subset(participant_greenness, !(stid %in% bc_homeown$stid))
black_pop <- participant_greenness %>% filter(race.desc == "Black")
white_pop <- participant_greenness %>% filter(race.desc == "White")
young_pop <- participant_greenness %>% filter(age < 65)
old_pop <- participant_greenness %>% filter(age >= 65)
low_svi <- participant_greenness %>% filter(SPL_THEMES_cat == "Low")
high_svi <- participant_greenness %>% filter(SPL_THEMES_cat == "High")


modelx <- lm(Participant_300m_Mean_ndvi ~inc.desc+edu.desc+
               race.desc+gend.desc+NEAR_DIST_MajorRoads+PopDen, 
                  data = participant_greenness,)
modely <- lm(Participant_300m_Mean_ndvi ~inc.desc+edu.desc+
               race.desc+NEAR_DIST_MajorRoads+hznum+medu.desc+CUR_TOTAL,
             data = pg_for_lm)
model_test <- lm(Participant_200m_Mean_ndvi ~inc.desc,
             data = participant_greenness)
coef(model_test)
confint(model_test)
colnames_green <- colnames(participant_greenness)
colnames_green <-  colnames_green[350:514]
setwd("E:/GIS/Local_Computer_Stuff/GIS/models/no_interactions_ses_unadj_propval_svihigh")
sesvar <- "propval"
population <- "svihigh"
resultdf = data.frame(a=1:10)
lapply(colnames_green,
       
       function(var) {
         #  ~ inc.desc+edu.desc+age+race.desc+gend.desc+NEAR_DIST_MajorRoads
         # subset= rentown.desc=="Rent"
         formula <- as.formula(paste(var, " ~ CUR_TOTAL"))
         model1 <- glm(formula, data = high_svi)
         conf1 <- confint(model1)
         write.csv(tidy(model1), paste0(var,"_", sesvar, "_", population, ".csv"))
         write.csv(conf1, paste0(var,"_", sesvar, "_", population, "_confint.csv"))
         #write.csv(beta(model1, x=FALSE, y=TRUE)$coefficients, paste0(var,"_", sesvar, "_", population, "_standardz.csv"))
         # col1 <- paste0(var, "_estimate")
         # colnames(resultdf) <- col1
         # resultdf[, col1] <- NA
         # resultdf[, col1] <- tidy(model1)$estimate
         # toselect.x <- summary(model1)$coeff[-1,4] < 0.05
         toselect.x <- subset(tidy(model1), p.value < 0.05)
         # select sig. variables
         # relevant.x <- names(toselect.x)[toselect.x == TRUE] 
         # write.csv(relevant.x, paste0(var, "_selected.csv"))
         write.csv(toselect.x, paste0(var, "_", sesvar, "_", population, "_selected.csv"))
         # formula with only sig variables
         # model2 <- as.formula(paste(var, " ~ ",paste(relevant.x, collapse= "+"))) 
         # summary(model2)$coeff[-1,4] < 0.05
       })
         
summary(pg.model)$coeff[-1,4] < 0.05
summary(pg.model)[summary(pg.model)$coeff[-1,4] < 0.05,]
subset(tidy(pg.model), p.value < 0.05)
filter(tidy(pg.model), p.value < 0.05)

summary(participants$Pop)