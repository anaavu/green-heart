library(gtsummary)
library(dplyr)


participants <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHData_RY_041720.csv")
whichbg <- read.csv("C:/Users/a0uppa01/Documents/GIS/GHParticipants_whichbg.csv")
whichbg <- whichbg %>% select(StudyID, FIPS)
participants <- left_join(participants, whichbg, by=c("stid"="StudyID"))
participants <- participants %>%
  mutate(gend.desc = as.factor(if_else(gend == 1, 'Female', 'Male'))) %>%
  mutate(race.desc = as.factor(if_else(race == 3, 'Black', 
                                       if_else(race == 5, 'White', 'Other')))) %>%
  mutate(obese.desc = as.factor(if_else(bmi >= 30, 'Yes', 
                                        if_else(bmi < 30, 'No', NULL)))) %>%
  mutate(lip.desc = as.factor(if_else(lip == 1, 'Yes', 
                                      if_else(lip == 2, 'No', NULL)))) %>%
  mutate(dm.desc = as.factor(if_else(dm == 1, 'Yes', 
                                     if_else(dm == 2, 'No', NULL)))) %>%
  mutate(inc.desc = as.factor(if_else(inc == 1, '< $20,000', 
                                      if_else(inc == 2, '20,000-$45,000', 
                                              if_else(inc == 3, '$45,000-$65,000',
                                                      if_else(inc==4 | inc==5 | inc==6, '>$65,000', NULL)))))) %>%
  mutate(edu.desc = as.factor(if_else(edu>=1 & edu<=3, '<=High School Diploma',
                                      if_else(edu==4 | edu==5, 'Some college',
                                              if_else(edu>=6 & edu<=8, '>=4-year degree', NULL))))) %>%
  mutate(hznum.desc = as.factor(if_else(hznum==1 | hznum==2, 'Small',
                                        if_else(medu>=3 & medu<=5, 'Medium',
                                                if_else(medu>=6, 'Large', NULL))))) %>%
  mutate(rentown.desc = as.factor(if_else(rentown == 1, 'Rent', 'Own')))
  

participants <- participants %>% dplyr::select(gend.desc,race.desc,
                                        #obese.desc,dm.desc,
                                        inc.desc,edu.desc,hznum.desc,
                                        age,rentown.desc,FIPS)

participants %>% tbl_summary(by = FIPS,
                          statistic = list(all_continuous() ~ "{mean} ({sd})",
                                           all_categorical() ~ "{n} / {N} ({p}%)"),
                          digits = all_continuous() ~ 2,
                          label = list(race.desc ~ "Race", gend.desc ~ "Gender", 
                                       #obese.desc ~ "Obese",
                                       #dm.desc ~ "Diabetes", 
                                       hznum.desc ~ "Household size",
                                       age ~ "Age",
                                       inc.desc ~ "Income", edu.desc ~ "Education"),
                          missing = "no", #can switch to ifany
                          missing_text = "(Missing)") %>% 
  modify_header(label ~ "**Variable**") %>%
  # modify_footnote(everything() ~ "Abbreviations") %>%
  #modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**NDVI**") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  bold_labels() %>% add_overall() %>% bold_p()
