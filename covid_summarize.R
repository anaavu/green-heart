library(ggplot2)
library(dplyr)
mydata <- read.csv("/Users/Anaavu/Desktop/Anagha/Louisville/GIS/cases_with_sheds_stats_AU_20201030.csv")



summarized_table <- mydata %>%
  group_by(ShedNUM, WEEKNUM) %>%
  summarize(Cases = sum(FREQUENCY),
            total_pop = mean(MEAN_SUM_Total_),
            white_pop = mean(MEAN_SUM_White_),
            black_pop = mean(MEAN_SUM_Estima),
            hispanic_pop = mean(MEAN_SUM_Hispan),
            pct_white_pop = mean(MEAN_Percent_Wh),
            pct_black_pop = mean(MEAN_Percent_Bl),
            pct_hispanic_pop = mean(MEAN_Percent_Hi))

mydata <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID/ProcessedData/metrodata/metrodata.csv")


mydata$FinalZipcode <- rowMeans(mydata[,72:73], na.rm=TRUE)
mydata <- mydata %>%
  mutate(Sex = as.factor(if_else(SEX == 1, 'Male', 
                  if_else(SEX == 2, 'Female', 
                    if_else(SEX==3 | SEX==6, 'Other', NULL))))) %>%
  mutate(Race = as.factor(if_else(RACE == 1 | RACE == 4, 'Native American/Pacific Islander', 
                        if_else(RACE == 2, 'Asian', 
                            if_else(RACE==3, 'Black',
                                if_else(RACE==5, 'White', 
                                    if_else(RACE == 6 | RACE == 7, 'Other',NULL))))))) %>%
  mutate(Ethnicity = as.factor(if_else(ETHN == 1, 'Yes', 
                                        if_else(ETHN == 2, 'No', NULL)))) %>%
  mutate(Workoutsidehome = as.factor(if_else(WRKHM == 1, 'Yes', 
                                    if_else(WRKHM == 0, 'No', NULL)))) %>%
  mutate(Pregnant = as.factor(if_else(PREG == 1, 'Yes', 
                                        if_else(PREG == 2, 'No', NULL))))
  
na_count <- data.frame(sapply(mydata, 
                              function(y) sum(is.na(y))))
na_count$name<-rownames(na_count)
mydata1 <- mydata %>% select(X,ROUNDNAME,AGE_DOB, Sex, Race,Ethnicity,
                             Workoutsidehome,Pregnant,STRESS)
library(gtsummary)
theme_gtsummary_compact()
mydata1 %>% tbl_summary(by = ROUNDNAME,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2,
                       label = list(AGE_DOB ~ "Age", Ethnicity ~ "Hispanic/Latino?"),
                       missing = "ifany", #can switch to ifany
                       missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6") ~ "**Round**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()

  

mydata1 %>% tbl_summary(
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        digits = all_continuous() ~ 2,
                        label = list(),
                        missing = "no", #can switch to ifany
                        missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()
