library(pscl)
library(mgcv)

hypertensive <- participant_greenness %>% filter(htn==1)
highbp <- participant_greenness %>% filter(sbpavg>140)

medicated <- participant_greenness %>% filter(ace == 1  | bb == 1  | ccb == 1 | diur == 1 |psdiur == 1)

participant_greenness <- participant_greenness %>%
  mutate(htn2 = as.factor(if_else(htn == 1, 'Yes', 'No'))) %>%
  mutate(htn3 = as.factor(if_else(htn == 1, 'Yes', 
                                  if_else(htn == 2, 'No', NULL))))

model <- lm(sbpavg ~Participant_150m_Mean_ndvi+age+race, data = htndata_filtered)
summary(model)
ggplot(participant_greenness, aes(x=sbpavg, y=age)) + geom_point() + geom_smooth(method = lm)
ggplot(participant_greenness, aes(x=as.factor(htn), y=Participant_100m_Mean_ndvi)) + geom_boxplot()

p<-ggplot(participant_greenness, aes(x=as.factor(htn), y=Participant_100m_Mean_ndvi)) + 
  geom_dotplot(binaxis='y', stackdir='center')


model <- glm(htn2 ~Participant_100m_Mean_NDVI_L19+age+nowsmk+
              gend+race+obese.desc, data = participant_greenness, family = binomial())
# McFadden's pseudo-R2
pR2(model)
participant_greenness$Participant_50m_Mean_l

model <- glm(htn2 ~Participant_50m_Mean_LAI+age+nowsmk+
               gend+race+obese.desc, data = participant_greenness, family = binomial())


model <- lm(sbpavg ~Participant_200m_Mean_ndvi+age+race+bmi+gend.desc+edu.desc, data = pg_for_lm2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race+bmi, data = file2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race, data = file3)

model <- lm(sbpavg ~Participant_200m_Mean_ndvi+age+race+bmi+ace+diur+psdiur, data = participant_greenness)

model <- glm(htn2 ~Participant_100m_Mean_ndvi+age+race+bmi+gend.desc+edu.desc, data = participant_greenness, family=binomial())
model <- glm(htn2 ~Participant_100m_Mean_ndvi+age+race+bmi, data = file2, family=binomial())
model <- glm(htn2 ~Participant_100m_Mean_ndvi+age+race, data = file3, family=binomial())
model <- lm(sbpavg ~Participant_200m_Mean_ndvi+age+race+bmi+diur+psdiur, data = participant_greenness)


summary(model)
model2 <- lm(sbpavg ~ ace+age+bmi, data=pg_for_lm2)
summary(model2)
#ggplot(data=participant_greenness, aes(x=ace, y=mean(Participant_100m_Mean_ndvi)) + geom_bar(stat="identity")

       
model2 <- lm(sbpavg ~ diur.desc, data=participants_greenness)
summary(model2)

model2 <- lm(sbpavg ~ Participant_200m_Mean_ndvi, data=participant_greenness)
summary(model2)

pg_for_lm2 <- participant_greenness %>% select(ace, u_ma_n)
pg_for_lm2 <- na.omit(pg_for_lm2)
pg_for_lm2$predlm = predict(model2)

ggplot(pg_for_lm2, aes(x = ace, y = u_ma_n) ) +
  geom_point() +
  geom_line(aes(y = predlm), size = 1)


# New htn data has 625 observations: NAs 
htn2 <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/htndata2.csv")
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2_filtered)
model1 <- mgcv::gam(sbpavg ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(log(sbpavg) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)
model <- gam(log(sbpavg) ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(MAP ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)
model <- gam(MAP ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(log(MAP) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)


model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
model <- gam(sbpavg ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
model <- lm(log(sbpavg) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
model <- gam(log(sbpavg) ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)



model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+as.factor(dm)+as.factor(etoh1)+as.factor(lip), data = htn2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+as.factor(dmorHbA1c)+as.factor(etoh1)+as.factor(lip), data = htn2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+HbA1c+as.factor(etoh1)+as.factor(lip), data = htn2)

model <- gam(sbpavg ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+as.factor(dm)+as.factor(etoh1)+as.factor(lip), data = htn2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+as.factor(dmorHbA1c)+as.factor(etoh1)+as.factor(lip), data = htn2)
model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+HbA1c+as.factor(etoh1)+as.factor(lip), data = htn2)

model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
               bmi+nowsmk.desc+as.factor(dm)+as.factor(etoh1)+as.factor(lip)+as.factor(anyDiurACECCB), data = htn2)

htn2_filtered <- htn2 %>% filter(anyDiurACECCB == 0)

model <- lm(sbpavg ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope)+
              bmi+nowsmk.desc+as.factor(dm)+as.factor(etoh1)+as.factor(lip), data = htn2)

model <- gam(sbpavg ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
model <- lm(log(sbpavg) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
model <- gam(log(sbpavg) ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc+edu.desc+(edu.desc*inc.desc)+as.factor(cope), data = htn2)
# hba1c


model1 <- mgcv::gam(sbpavg ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(log(sbpavg) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)
model <- gam(log(sbpavg) ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(MAP ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)
model <- gam(MAP ~s(Participant_100m_Mean_ndvi)+s(age)+race.desc+gend.desc, data = htn2)
model <- lm(log(MAP) ~Participant_100m_Mean_ndvi+age+race.desc+gend.desc, data = htn2)

participant_greenness$Partici


summary(model)



model <- lm(log(map) ~Participant_150m_Mean_ndvi+age+race.desc, data = participant_greenness)
model <- lm(ace ~Participant_150m_Mean_ndvi+age+race.desc, data = participant_greenness)


library(mgcv)
model3 <- gam(sbpavg ~ s(Participant_200m_Mean_ndvi), data=participants_greenness)
summary(model3)
plot(model3)

model4 <- glm(sbpavg ~ Participant_200m_Mean_ndvi, data=participants_greenness)
summary(model4)
plot(model3)

fordoseresponse2 <- NULL
fordoseresponse2$Change.in.SBP <- coef(model3)
fordoseresponse2 <- data.frame(fordoseresponse2)
fordoseresponse2 <- cbind(Change.in.NDVI = rownames(fordoseresponse2), fordoseresponse2)
rownames(fordoseresponse2) <- 1:nrow(fordoseresponse2)
fordoseresponse2 <- fordoseresponse2[-1,]
fordoseresponse2$Change.in.NDVI <- seq(0.1,0.9,0.1)
ggplot(fordoseresponse2, aes(x=Change.in.NDVI, y=Change.in.SBP)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "Difference in NDVI", y = "Change in expected systolic blood pressure",
       title = "Dose Response between NDVI (200m) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")


participants_greenness <- participants_greenness %>%
  mutate(NDVIcat = ntile(Participant_200m_Mean_ndvi, 3)) %>%
  mutate(NDVIcat = if_else(NDVIcat == 1, 'Low', 
                           if_else(NDVIcat == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat = factor(NDVIcat) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness_low200ndvi <- filter(participants_greenness, NDVIcat == "Low")
participant_greenness_med200ndvi <- filter(participants_greenness, NDVIcat == "Medium")
participant_greenness_high200ndvi <- filter(participants_greenness, NDVIcat == "High")
participants_greenness <- participants_greenness %>%
  mutate(NDVIcat_lawn = ntile(morevisible_ndvi_MEAN, 3)) %>%
  mutate(NDVIcat_lawn = if_else(NDVIcat_lawn == 1, 'Low', 
                           if_else(NDVIcat_lawn == 2, 'Medium', 'High'))) %>%
  mutate(NDVIcat_lawn = factor(NDVIcat_lawn) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness_lowparcelndvi <- filter(participants_greenness, NDVIcat_lawn == "Low")
participant_greenness_medparcelndvi <- filter(participants_greenness, NDVIcat_lawn == "Medium")
participant_greenness_highparcelndvi <- filter(participants_greenness, NDVIcat_lawn == "High")

participants_greenness <- participants_greenness %>%
  mutate(canopycat = ntile(Participant_200m_Mean_canopy2, 3)) %>%
  mutate(canopycat = if_else(canopycat == 1, 'Low', 
                                if_else(canopycat == 2, 'Medium', 'High'))) %>%
  mutate(canopycat = factor(canopycat) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness_low200canopy <- filter(participants_greenness, canopycat == "Low")
participant_greenness_med200canopy <- filter(participants_greenness, canopycat == "Medium")
participant_greenness_high200canopy <- filter(participants_greenness, canopycat == "High")

participant_greenness <- participant_greenness %>%
  mutate(canopycat_lawn = ntile(morevisible_canopy2_MEAN, 3)) %>%
  mutate(canopycat_lawn = if_else(canopycat_lawn == 1, 'Low', 
                             if_else(canopycat_lawn == 2, 'Medium', 'High'))) %>%
  mutate(canopycat_lawn = factor(canopycat_lawn) %>% fct_relevel(c("High", "Medium", "Low")))
participant_greenness_lowparcelcanopy <- filter(participant_greenness, canopycat_lawn == "Low")
participant_greenness_medparcelcanopy <- filter(participant_greenness, canopycat_lawn == "Medium")
participant_greenness_highparcelcanopy <- filter(participant_greenness, canopycat_lawn == "High")

# 200 ndvi
model1 <- glm(sbpavg ~ Participant_200m_Mean_ndvi+race.desc+age+gend.desc, data=participant_greenness)
summary(model1)
fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/Hypertension paper/fordoseresponse200ndvi.csv")
ggplot(fordoseresponse, aes(x=Change.in.NDVI, y=Change.in.SBP)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "NDVI", y = "Change in expected systolic blood pressure",
       title = "Dose Response between NDVI (200m) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")
# parcel ndvi
model1 <- glm(sbpavg ~ morevisible_ndvi_MEAN+race.desc+age+gend.desc, data=participant_greenness_highparcelndvi)
summary(model1)
fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/Hypertension paper/fordoseresponsefyardndvi.csv")
ggplot(fordoseresponse, aes(x=Change.in.NDVI, y=Change.in.SBP)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "NDVI", y = "Change in expected systolic blood pressure",
       title = "Dose Response between NDVI (Front Yard) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")
# 200 canopy
model1 <- glm(sbpavg ~ Participant_200m_Mean_canopy2+race.desc+age+gend.desc, data=participant_greenness_high200canopy)
summary(model1)
fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/Hypertension paper/fordoseresponse200canopy.csv")
ggplot(fordoseresponse, aes(x=Change.in.NDVI, y=Change.in.SBP)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "Canopy", y = "Change in expected systolic blood pressure",
       title = "Dose Response between Canopy (200m) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")
# fyard canopy
model1 <- glm(sbpavg ~ morevisible_canopy2_MEAN+race.desc+age+gend.desc, data=participant_greenness_highparcelcanopy)
summary(model1)
fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/Hypertension paper/fordoseresponsefyardcanopy.csv")
ggplot(fordoseresponse, aes(x=Change.in.NDVI, y=Change.in.SBP)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "Canopy", y = "Change in expected systolic blood pressure",
       title = "Dose Response between Canopy (Front Yard) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")

ggplot(participant_greenness, aes(x=Participant_200m_Mean_ndvi)) + 
  geom_point() + theme_light() + geom_smooth() +
  labs(x = "Difference in Canopy", y = "Change in expected systolic blood pressure",
       title = "Dose Response between Canopy (Front Yard) and Systolic BP",
       caption = "data from Yeager/Uppal (2021)")


# Compute a histogram of `chol$AGE`
ggplot(data=participant_greenness, aes(participant_greenness$Participant_200m_Mean_ndvi)) + 
  geom_histogram()
