library(broom)

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

modelx <- lm(Participant_300m_Mean_ndvi ~inc.desc+edu.desc+
               race.desc+gend.desc+NEAR_DIST_MajorRoads+PopDen, 
             data = pg_for_lm, subset= rentown_parcel=="Own")
modely <- lm(Participant_300m_Mean_ndvi ~inc.desc+edu.desc+
               race.desc+NEAR_DIST_MajorRoads+hznum+medu.desc+CUR_TOTAL,
             data = pg_for_lm)
participant_greenness = participant_greenness %>% select(-starts_with("HEAL"),
                                 -starts_with("Heal"),
                                 -starts_with("FocalStatistics"),
                                 -starts_with("Andrea"),
                                 -starts_with("ghKDE"),
                                 -starts_with("ParcelID"),
                                 -starts_with("FIPS_")) 
sapply(pg_for_lm, function(x) sum(is.na(x)))
setwd("I:/Backups/GIS/Local_Computer_Stuff/GIS/models/no_interactions_voc")
resultdf = data.frame(a=1:10)
colnames_green <- colnames(participant_greenness)
colnames_green <-  colnames_green[338:480]
colnames_voc <- colnames(participant_greenness)
colnames_voc <-  colnames_voc[182:263]

lapply(colnames_voc,
       
       function(var_voc) {

  lapply(colnames_green,
       
       function(var) {
         
         formula <- as.formula(paste0(var_voc, " ~ s(", var,")+s(Participant_300m_Mean_ndvi)+s(age)+
  race.desc+gend.desc+s(bmi)+s(TrafficExposure)"))
         print(formula)
         model1 <- mgcv::gam(formula, data = nonsmokers)
         # model1 <- glm(formula, data = pg_for_lm, subset= rentown.desc=="Rent")
         #conf1 <- confint(model1)
         write.csv(tidy(model1), paste0(var_voc,"_",var,".csv"))
         write.csv(conf1, paste0(var_voc,"_",var,"_confint.csv"))
         # col1 <- paste0(var, "_estimate")
         # colnames(resultdf) <- col1
         # resultdf[, col1] <- NA
         # resultdf[, col1] <- tidy(model1)$estimate
         # toselect.x <- summary(model1)$coeff[-1,4] < 0.05
         toselect.x <- subset(tidy(model1), p.value < 0.05)
         # select sig. variables
         # relevant.x <- names(toselect.x)[toselect.x == TRUE] 
         # write.csv(relevant.x, paste0(var, "_selected.csv"))
         write.csv(toselect.x, paste0(var_voc,"_",var, "_selected.csv"))
         # formula with only sig variables
         # model2 <- as.formula(paste(var, " ~ ",paste(relevant.x, collapse= "+"))) 
         # summary(model2)$coeff[-1,4] < 0.05
       })
       })

summary(pg.model)$coeff[-1,4] < 0.05
summary(pg.model)[summary(pg.model)$coeff[-1,4] < 0.05,]
subset(tidy(pg.model), p.value < 0.05)
filter(tidy(pg.model), p.value < 0.05)

summary(participants$Pop)