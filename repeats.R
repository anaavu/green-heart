library(caret)
library(leaps)
library(tidyverse)
library(MASS)
library(GGally)
library(bootStepAIC)
library(plotly)
library(mlbench)


# Grab the data
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_AU_20201221.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_allgreenness_20210204.csv")
# greenness <- greenness %>% dplyr::select(Participant_200m_Mean_ndvi)

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
  mutate(NEAR_DIST_MajorRoads = NEAR_DIST_MajorRoads/3281)

participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))

# taking all variables from SES table 
# checking the NA count for each
# and removing rentown.desc and bc.desc (type of building) 
# these two variables have n=300 missing values
pg_for_lm <- participant_greenness %>% select(Parcel_ndvi_Mean,
                                          gend.desc,race.desc,ethn.desc,
                                           inc.desc,
                                           edu.desc,medu.desc,hznum.desc,exr.desc,
                                           rentown.desc,bc.desc,
                                           litt.desc,
                                           sdwlk.desc,
                                           saf1.desc,saf2.desc,saf3.desc,age,bmi,
                                           NEAR_DIST_MajorRoads, LASTCNT_MajorRoadsTraffic,
                                           PopDen)
## removing rentown=333 NA
drop <- c("rentown.desc")
pg_for_lm = pg_for_lm[,!(names(pg_for_lm) %in% drop)]
d <- sapply(pg_for_lm, function(x) sum(is.na(x)))
pg_for_lm <- na.omit(pg_for_lm) #574 observations

ggscatmat(pg_for_lm, columns = 1:ncol(pg_for_lm))

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

library(leaps)
library(tidyverse)
library(broom)
library(MASS)
library(GGally)
library(bootStepAIC)
library(plotly)
library(mlbench)

# Grab the data
participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_AU_20201221.csv")
greenness <- read.csv("C:/Users/a0uppa01/Documents/GIS/participants_allgreenness_20210214.csv")
traffic <- read.csv("C:/Users/a0uppa01/Documents/GIS/HEAL_addTrafficGISData_AU_20210224.csv")
participants <- participants %>% dplyr::select(inc, stid)

participants <- participants %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL))))))


participant_greenness  <- left_join(participants, greenness, by=c("stid"="StudyID"))

# taking all variables from SES table 
# checking the NA count for each
# and removing rentown.desc and bc.desc (type of building) 
# these two variables have n=300 missing values
sapply(pg_for_lm, function(x) sum(is.na(x)))
drop <- c("Nobuilding_NDVI_L19_Sum ","lessvisible_NDVI_L19_MEAN", 
          "morevisible_NDVI_L19_MEAN", "Nobuilding_NDVI_L19_Mean", "Parcel_NDVI_L19_Sum",
          "Parcel_NDVI_L19_Mean","lessvisible_LAI_MEAN", "morevisible_LAI_MEAN",
          "morevisible_leafarea_MEAN", "lessvisible_leafarea_MEAN", "lessvisible_ndvi_MEAN",
          "morevisible_biomass_MEAN", "lessvisible_biomass_MEAN", "HEALLessVisbParcelNDVI061520b_1",
          "HEALLessVisbParcelNDVI061520b", "Nobuilding_NDVI_L19_Sum", "Field", 
          "FocalStatistics_OutRaster_Full7inNDVI052620500m")
pg_for_lm = participant_greenness[,!(names(participant_greenness) %in% drop)]
pg_for_lm <- na.omit(pg_for_lm) #643 observations

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



