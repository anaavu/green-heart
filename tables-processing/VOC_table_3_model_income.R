#######################################################################################
## Author: Anagha Uppal
## Date: Feb 2021
## Purpose: Table 3 for SES 2nd Paper
## Inputs: 
## Outputs: 
#######################################################################################
## To do: 

library(leaps)
library(tidyverse)
library(MASS)
library(GGally)
library(bootStepAIC)
library(plotly)
library(pscl)
library(styler)
library(ggpubr)
library(tidymv)

install.packages("remotes")
remotes::install_github("samclifford/mgcv.helper")

styler::style_file()

# Grab the data
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_AU_20201221.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_allgreenness_20210204.csv")

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
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)

participants <- participants %>% dplyr::select(inc.desc, stid, age, gend.desc,
                                               race.desc, ethn.desc, edu.desc,
                                               NEAR_DIST_MajorRoads)

participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))


# taking all variables from SES table 
# checking the NA count for each
# and removing rentown.desc and bc.desc (type of building) 
# these two variables have n=300 missing values
drop <- c("Nobuilding_NDVI_L19_Sum","lessvisible_NDVI_L19_MEAN", 
          "morevisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean", "Parcel_NDVI_L19_Sum",
          "Parcel_NDVI_L19_Mean","lessvisible_LAI_MEAN", "morevisible_LAI_MEAN",
          "morevisible_leafarea_MEAN", "lessvisible_leafarea_MEAN", "lessvisible_ndvi_MEAN",
          "morevisible_biomass_MEAN", "lessvisible_biomass_MEAN", 
          "inc", "SubjectID",
          "Field", "Event_Name", "Study_ID", "stid",
          "USER_Home_Address", "OBJECTID_1", "X", "Y", "ObjectID",
          "Subject_ID", "Home_Address", 
          "ImperviousyPercFocal500m060320", "ImperviousyPercFocal300m060320",
          "ImperviousyPercFocal50m060320",
          "MEAN_12", "Event_Name", "OBJECTID")
pg_for_lm = participant_greenness[,!(names(participant_greenness) %in% drop)]
pg_for_lm = pg_for_lm %>% select(-starts_with("HEAL"),
                                 -starts_with("Heal"),
                                 -starts_with("FocalStatistics"),
                                 -starts_with("Andrea"),
                                 -starts_with("ghKDE"),
                                 -starts_with("ParcelID"),
                                 -starts_with("FIPS")) 
sapply(pg_for_lm, function(x) sum(is.na(x)))


pg_for_lm <- na.omit(pg_for_lm) #641 observations

# income <- green
# Fit the full model 
full.model <- glm(inc.desc ~., data = pg_for_lm, family=binomial())
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
bootstep.model <- boot.stepAIC(full.model, pg_for_lm, B = 100, alpha = 0.05, 
                               direction = "both", k = 2, verbose = TRUE)
summary(step.model) #inc, exr, litt, neardist, lastcnt
summary(bootstep.model)
bootstep.model
summary(full.model)

anova(full.model, test="Chisq")
# McFadden's pseudo-R2
pR2(full.model)

######################SUBSET CORR##############################
pg_for_lm_local = pg_for_lm %>% select(-starts_with("Participant_20m"), 
                                       -starts_with("Parcel_"),
                                       -starts_with("Nobuilding_"),
                                       -starts_with("lessvisible_"),
                                       -starts_with("morevisible_"))
pg_lm_local = glm(inc.desc ~., data = pg_for_lm_local, family=binomial())
summary(pg_lm_local)
pR2(pg_lm_local)

pg_for_lm_personal = pg_for_lm %>% select(-contains("00m_"), 
                                          -contains("50m_"),
                                          -starts_with("Blockgroup_"))
pg_lm_personal = glm(inc.desc ~., data = pg_for_lm_personal, family=binomial())
summary(pg_lm_personal)
step.model2 <- stepAIC(pg_lm_personal, direction = "both", trace = FALSE)

stargazer(pg_lm_personal, pg_lm_local, title = "Results", align = TRUE,
          type = "text")
pR2(pg_lm_personal)

######################ONE GREEN##############################
# income <- green
# Fit the full model 
full.model <- lm(u_cot_n ~Participant_50m_Mean_NDVI_L19+gend.desc+race.desc+
                    ethn.desc+exr.desc+age+bmi+NEAR_DIST_MajorRoads+LASTCNT_MajorRoadsTraffic, 
                  data = participant_greenness, subset= nowsmk.desc=="No")
full.model <- lm(u_cot_n ~Participant_200m_Mean_ndvi+gend.desc+race.desc+bmi+
                   exr.desc+age+edu.desc+NEAR_DIST_MajorRoads+LASTCNT_MajorRoadsTraffic, 
                 data = participant_greenness, subset= nowsmk.desc=="No")
#dietary restrictions #replacing education/income
summary(full.model)
# exp, chem, fume, eh, eh9
# Stepwise regression model


step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
bootstep.model <- boot.stepAIC(full.model, pg_for_lm, B = 100, alpha = 0.05, 
                               direction = "both", k = 2, verbose = TRUE)
summary(step.model) #inc, exr, litt, neardist, lastcnt
summary(bootstep.model)
bootstep.model
summary(full.model)

anova(full.model, test="Chisq")
# McFadden's pseudo-R2
pR2(full.model)
car::vif(full.model) 

library(mgcv)
train.data <- nonsmokers
test.data <- subset(nonsmokers, !(Study_ID %in% train.data$Study_ID))
test.data <- train.data
setwd("I:/Backups/GIS/Local_Computer_Stuff/GIS/models/voc")
bma_model_basic <- lm(u_bma_n ~ Participant_300m_Mean_ndvi+age+
                               race.desc+gend.desc+bmi+TrafficExposure+tmout,
                             data = train.data)
# Build the model
# 3hpma and bma are solid
bma_model_basic <- mgcv::gam(u_bma_n ~ s(Participant_300m_Mean_ndvi)+s(age)+
                     race.desc+gend.desc+as.factor(etoh1)+s(bmi)+exr.desc+s(TrafficExposure),
                    data = train.data)
summary(bma_model_basic)
write.csv(tidy(bma_model_basic), paste0("bma_model_basic",".csv"))
gam.check(bma_model_basic)
plot(bma_model_basic, all.terms = TRUE)

bma_model_env <- mgcv::gam(u_bma_n ~ s(Participant_300m_Mean_ndvi)+s(age)+
                     race.desc+gend.desc+s(TrafficExposure)+
                     s(dailymeanPM25)+s(Temp_AVG)+
                     as.factor(chem_fume_expos)+
                     as.factor(exp)+s(bmi)+, data = train.data)
summary(bma_model_env)
gam.check(bma_model_env)
bma_model_full <- mgcv::gam(u_bma_n ~ s(Participant_300m_Mean_ndvi)+s(age)+
                             race.desc+gend.desc+s(TrafficExposure)+
                             s(dailymeanPM25)+s(Temp_AVG)+
                             as.factor(chem_fume_expos)+
                             as.factor(exp)+s(bmi)+edu.desc2, data = train.data)
summary(bma_model_full)
gam.check(bma_model_full)

bma_model_full <- mgcv::gam(u_bma_n ~ s(Parcel_ndvi_Mean)+s(age)+
                      race.desc+s(bmi)+gend.desc+s(TrafficExposure, k=20)+
                      s(dailymeanPM25)+s(Temp_AVG, k=20)+
                      edu.desc2, data = train.data)
summary(model3)
gam.check(model3)


model4 <- mgcv::gam(as.factor(htn4) ~s(Participant_100m_Mean_ndvi)+gend.desc+race.desc+
                      s(bmi)+s(TrafficExposure)+s(addtm)+s(age)+
                      as.factor(etoh1)+as.factor(dm)+as.factor(lip)+as.factor(cope)+
                      edu.desc2, data = train.data, family = binomial)
summary(model4) 
sum(is.na(participant_greenness$Participant_100m_Mean_ndvi))
for_glm <- participant_greenness %>% filter(!(is.na(Participant_100m_Mean_ndvi)))
Participant_300m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
  bmi+nowsmk.desc+as.factor(dm)+as.factor(etoh1)+as.factor(lip)

model4 <- glm(as.factor(htn4) ~gend.desc+race.desc+Participant_100m_Mean_ndvi+I(Participant_100m_Mean_ndvi^2)+
                      bmi+TrafficExposure+addtm+age+
                      edu.desc, data = for_glm, family = binomial)
summary(model4) 
plot(model4, all.terms = TRUE)
gam.check(model4)
poly(Participant_100m_Mean_ndvi,2, raw=TRUE)
model5 <- mgcv::gam(as.factor(ace) ~s(Participant_300m_Mean_ndvi, k=-1)+gend.desc+race.desc+
                      s(bmi)+s(TrafficExposure)+s(addtm)+s(age)+
                      s(dailymeanPM25)+s(Temp_AVG)+
                      edu.desc, data = train.data, family = binomial())
summary(model5) # I removed ozone, because it doesn't matter
plot(model5, all.terms = TRUE)
gam.check(model5)

plot_smooths(
  model = model5,
  series = Participant_300m_Mean_ndvi,
  #comparison = fac
) +
  theme(legend.position = "top")

plot_smooths(
  model = model4,
  series = Participant_100m_Mean_ndvi,
  #comparison = fac
) +
  theme(legend.position = "top")

#infl <- c(FALSE, diff(diff(out)>0)!=0)
#points(xl[infl ], out[infl ], col="blue")


# Make predictions
predictions <- model4 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$u_bma_n, na.rm = TRUE),
  R2 = R2(predictions, test.data$u_bma_n, na.rm = TRUE)
)
ggplot(test.data, aes(age, u_bma_n) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
car::avPlots(model = model1)

CO2_pred <- data.frame(x = train.data$TrafficExposure,
                       y = train.data$Participant_300m_Mean_ndvi,
                       predicted_values = predict(model3, newdata = test.data))
ggplot(CO2_pred, aes(x = x)) +
  geom_point(aes(y = y), size = 1, alpha = 0.5) + geom_line(aes(y = predicted_values), colour = "red")
#transform = log? 
#make some various samples of basis function + predicted values
vif.gam(model3) # package for this not loaded
#+s(eh9)
# try a random forest on this
# try this exact model on SES, and maybe htn
# ask Satish what to do with such noisy data
model <- mgcv::gam(u_bma_n ~ s(NEAR_DIST_MajorRoads)+
                     s(LASTCNT_MajorRoadsTraffic), data = train.data)
model <- mgcv::gam(u_bma_n ~ s(NEAR_DIST_MajorRoads)+
                     s(LASTCNT_MajorRoadsTraffic), data = train.data)
model <- lm(u_mhbma3_n ~ NEAR_DIST_MajorRoads+
                     LASTCNT_MajorRoadsTraffic, data = train.data)
summary(model1)
gam.check(model)
plot(model, all.terms=TRUE)
participant_greenness$s(Participant_300m_Mean_ndvi)+
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$u_cot_n, na.rm = TRUE),
  R2 = R2(predictions, test.data$u_cot_n, na.rm = TRUE)
)
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
car::avPlots(model = full.model)



###############################################################

full.model <- glm(inc.desc ~(Parcel_ndvi_Mean+age+
                               race.desc+
                               NEAR_DIST_MajorRoads)^2, 
                  data = pg_for_lm, family=binomial(), 
                  subset= gend.desc=="Male")
summary(full.model)
full.model <- glm(inc.desc ~(Parcel_ndvi_Mean+age+
                               race.desc+ethn.desc+
                               NEAR_DIST_MajorRoads)^2, 
                  data = pg_for_lm, family=binomial(), 
                  subset= gend.desc=="Female")

car::vif(full.model) 
alias(full.model) #Based on this, had to remove ethnicity
# Exclude interactions or do something else?
anova(full.model, test="Chisq")
alias(full.model) #Based on this, had to remove ethnicity
# McFadden's pseudo-R2
pR2(full.model)

###############################################################
personal.model <- glm(Parcel_ndvi_Mean ~(inc.desc+edu.desc+age+race.desc)^2, 
                      data = pg_for_lm, 
                      subset= gend.desc=="Male")
summary(personal.model)
bs(women$height, df = 5)
summary(fm1 <- lm(weight ~ bs(height, df = 5), data = women))
personal.model <- glm(Parcel_ndvi_Mean ~(inc.desc+edu.desc+age+race.desc)^2, 
                      data = pg_for_lm, 
                      subset= gend.desc=="Female")
summary(personal.model)
personal.model <- glm(Parcel_ndvi_Mean ~inc.desc+edu.desc+age+race.desc, 
                      data = pg_for_lm, 
                      subset= gend.desc=="Male")
summary(personal.model)
personal.model <- glm(Parcel_ndvi_Mean ~inc.desc+age+race.desc, 
                      data = pg_for_lm)
summary(personal.model)
personal.model <- glm(inc.desc ~(Parcel_ndvi_Mean+age+
                                   race.desc+ethn.desc+
                                   NEAR_DIST_MajorRoads)^2, 
                      data = pg_for_lm, family=binomial(), 
                      subset= gend.desc=="Female")

car::vif(full.model) 
alias(full.model) #Based on this, had to remove ethnicity
# Exclude interactions or do something else?
anova(full.model, test="Chisq")
alias(full.model) #Based on this, had to remove ethnicity
# McFadden's pseudo-R2
pR2(full.model)


# Let's try a gam




###############################################################
glmnet()
###############################################################
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
rownames(coef.sign)[22] <- "inc.desc"
rownames(coef.sign)[23] <- "inc.desc"


coef.sign <- coef.sign[match(rownames(coef.pick), rownames(coef.sign)),]
# Matrix for statistical significance
coef.stat <- fit.boot$Significance
# Change name for "chas" since it is a factor

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

write.csv(pg_for_lm, "C:/Users/a0uppa01/Documents/GIS/pg_for_lm_AU_20201221.csv")
