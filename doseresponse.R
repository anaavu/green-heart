model_test <- lm(Participant_200m_Mean_ndvi ~inc.desc,
                 data = participant_greenness)
coef(model_test)
confint(model_test)

model_test <- lm(Nobuilding_ndvi_Mean ~inc.desc,
                 data = participant_greenness)
coef(model_test)
confint(model_test)

fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES paper/FinalTables/fordoseresponse_200m.csv")
fordoseresponse <- fordoseresponse %>%
  mutate(Income.Level = factor(Income.Level) %>% fct_relevel(c("<$20,000", 
                                                               "$20,000-$45,000", 
                                                               "$45,000-$60,000",
                                                               ">$60,000")))

ggplot(fordoseresponse, aes(x=Income.Level, y=Difference.in.NDVI, group=1)) + 
  geom_point() + theme_light() + geom_smooth() + geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1) +
  labs(x = "Income Level", y = "Difference in NDVI",
       title = "Dose Response between Income Level \nand NDVI (200m)",
       caption = "data from Yeager/Uppal (2021)") + 
  theme(axis.text.x = element_text(angle = 10, vjust = 0.9))


fordoseresponse <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES paper/FinalTables/fordoseresponse_allyard.csv")
fordoseresponse <- fordoseresponse %>%
  mutate(Income.Level = factor(Income.Level) %>% fct_relevel(c("<$20,000", 
                                                               "$20,000-$45,000", 
                                                               "$45,000-$60,000",
                                                               ">$60,000")))

ggplot(fordoseresponse, aes(x=Income.Level, y=Difference.in.NDVI, group=1)) + 
  geom_point() + theme_light() + geom_smooth() + geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1) +
  labs(x = "Income Level", y = "Difference in NDVI",
       title = "Dose Response between Income Level \nand NDVI (All Yard)",
       caption = "data from Yeager/Uppal (2021)") + 
  theme(axis.text.x = element_text(angle = 10, vjust = 0.9))
