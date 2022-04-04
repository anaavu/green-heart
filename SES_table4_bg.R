library(dplyr)
library(MASS)
library(broom)
library(pscl)
library(safejoin)
library(tidyverse)

# Grab the data
ghbg <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/bg_in_GH.csv")
svibg <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/participant_data/BG_SVI.csv")
bgresndvi <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/bgresidential_NDVIdata_20210609.csv")
bgresndvi$bgresndvi <- bgresndvi$Participant_5m_Mean_NDVI
bgrescanopy <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/bgresidential_canopydata_20210825.csv")
bgrescanopy$bgrescanopy <- bgrescanopy$Participant_5m_Mean_canopy
bgcanopy <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/BG_canopydata_20210825.csv")
bgcanopy$bgcanopy <- bgcanopy$BG_5m_Mean_canopy
bgndvi <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/BG_NDVIdata_20210825.csv")
bgndvi$bgndvi <- bgndvi$BG_5m_Mean_NDVI
bgroads <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/bg_roads.csv")

bgroads[is.na(bgroads)] <- 0

ghbg <- safe_left_join(ghbg, bgresndvi, 
                          by = c("FIPS" = "FIPS"),
                          check = "", conflict =  coalesce)
ghbg <- safe_left_join(ghbg, bgrescanopy, 
                       by = c("FIPS" = "FIPS"),
                       check = "", conflict =  coalesce)
ghbg <- safe_left_join(ghbg, bgndvi, 
                       by = c("FIPS" = "FIPS"),
                       check = "", conflict =  coalesce)
ghbg <- safe_left_join(ghbg, bgcanopy, 
                       by = c("FIPS" = "FIPS"),
                       check = "", conflict =  coalesce)

ghbg <- safe_left_join(ghbg, svibg, 
                       by = c("FIPS" = "GEOID"),
                       check = "", conflict =  coalesce)

ghbg <- safe_left_join(ghbg, bgroads, 
                       by = c("FIPS" = "FIPS"),
                       check = "", conflict =  coalesce)
ghbg <- ghbg %>%
  mutate(SPL_THEMES_cat = ntile(SPL_THEMES, 2)) %>%
  mutate(SPL_THEMES_cat = if_else(SPL_THEMES_cat == 1, 'Low', 'High')) %>%
  mutate(SPL_THEMES_cat = factor(SPL_THEMES_cat) %>% fct_relevel(c("Low", "High")))



ghbg <- ghbg %>%
  mutate(hsgrad_sum = dplyr::select(., 24:32) %>% rowSums(na.rm = TRUE)) %>%
  mutate(hsgrad_pct = hsgrad_sum/Total1*100) %>%
  mutate(unemployed = Civilian_Labor_Force__Unemplo1/Total__Employment_1*100) %>%
  mutate(minority_sum = Estimate__Total__Black_or_Afric+Hispanic_or_Latino) %>%
  mutate(minority_pct = minority_sum/Total_Population*100) %>%
  mutate(age = Estimate__Median_age_____Total) %>%
  mutate(roadlength = SUM_Shape_Length)
# FOR ALL JC
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ hsgrad_pct+unemployed+Median_HH_income+F__Renter_Occupied_Household+PopDen+age+minority_pct+roadlength, data=ghbg)
car::vif(bglevel_model)
# hs_grad _pct is so left skewed, no power is helping this situation
# removing and re-running
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ log(unemployed+0.0001)+Median_HH_income+F__Renter_Occupied_Household+PopDen+age+minority_pct+roadlength, data=ghbg)
# 72 0's: should we even log transform? 
# let's try sqrt
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ sqrt(unemployed)+Median_HH_+F__Renter_Occupied_Household+PopDen+age+minority_pct+roadlength, data=ghbg)
# the univariate distribution wasn't great for sqrt, really
# to stick with log + 0.001
# now for median hh income
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ log(unemployed+0.0001)+log(Median_HH_income+0.0001)+F__Renter_Occupied_Household+PopDen+age+minority_pct+roadlength, data=ghbg)
# unable to take the root power of F_Renter
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ log(unemployed+0.0001)+log(Median_HH_income+0.0001)+F__Renter_Occupied_Household+log(PopDen)+age+minority_pct+log(roadlength), data=ghbg)
# we're getting minority as sign again and again
# but we're dealing with potentially collinear minority_pct which is also badly skewed
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ log(unemployed+0.0001)+log(Median_HH_income+0.0001)+F__Renter_Occupied_Household+log(PopDen)+age+log(roadlength), data=ghbg)
# if we remove it, we get age significant
# let's put it back in and take out renters (remember, renters was also a skewed set)
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ log(unemployed+0.0001)+log(Median_HH_income+0.0001)+log(PopDen)+age+minority_pct+log(roadlength), data=ghbg)
# minority just becomes more significant
# still, our R squared is pretty bad. I wouldn't count on this.
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ hsgrad_pct+age+FREQUENCY, data=ghbg)
bglevel_model <- lm(bgcanopy ~ SPL_THEMES, data=ghbg)
bglevel_model <- gam(Participant_5m_Mean_NDVI ~ s(SPL_THEMES), data=ghbg)

bglevel_model <- lm(Participant_5m_Mean_NDVI ~ Median_HH_income, data=ghbg)


summary(bglevel_model)
gam.check(bglevel_model)
#Below two are similar looking
ggplot(ghbg, aes(SPL_THEMES_cat, Participant_5m_Mean_NDVI)) + geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))
ggplot(participant_greenness, aes(SPL_THEMES_cat, Participant_200m_Mean_ndvi)) + geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))


boxcox(bglevel_model)
ggplot(bglevel)  + 
  geom_point(aes(x = minority_pct, y = Participant_5m_Mean_NDVI)) +
  geom_density2d(aes(x = minority_pct, y = Participant_5m_Mean_NDVI))



#NDVI outcome, all covariates
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ hsgrad_pct+log(unemployed+0.0001)+Median_HH_income+
                      F__Renter_Occupied_Household+PopDen+age+minority_pct+log(roadlength+0.0001), data=ghbg)
#NDVI outcome, select covariates
# hs grad is somewhat collinear, so let's get rid of that
bglevel_model <- lm(Participant_5m_Mean_NDVI ~ Median_HH_income+
                      F__Renter_Occupied_Household+PopDen+age+minority_pct, data=ghbg)
write.csv(tidy(bglevel_model), "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/table4_incomeoutcome_selectcov.csv")
car::vif(bglevel_model)
#Income as outcome, all covariates minus hsgrad_pct
bglevel_model <- lm(Median_HH_income ~ log(unemployed+0.0001)+Participant_5m_Mean_NDVI+
                      F__Renter_Occupied_Household+PopDen+age+minority_pct+log(roadlength+0.0001), data=ghbg)
# Income as outcome, selected covariates
bglevel_model <- lm(Median_HH_income ~ Participant_5m_Mean_NDVI+
                      F__Renter_Occupied_Household+PopDen+age+minority_pct, data=ghbg)
summary(bglevel_model)

