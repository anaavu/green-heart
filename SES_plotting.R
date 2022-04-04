
library(ggplot2)
library(plotly)
# Grouped
ggplot(data = partic, aes(fill=edu.desc, 
                              y=Participant_200m_Mean_ndvi, x=inc.desc)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "seagreen"))

ses <- ggplot(data = participants, aes(x=factor(edu), y=factor(medu)))

options(scipen=9999999)
ggplot(data = pg_for_lm, aes(y=Parcel_ndvi_Mean, 
                             x=Parcel_ndvi_Sum)) + 
  geom_point() + theme_bw() + xlim(0,20000)
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "seagreen"))
  
  
pg_for_lm %$% cc(ht, lbw, graph = F)


plot_ly(pg_for_lm$age, pg_for_lm$NEAR_DIST_MajorRoads, 
        pg_for_lm$Participant_300m_Mean_ndvi,type = "surface")
plot_ly(data = pg_for_lm, x=pg_for_lm$inc.desc, y=pg_for_lm$Participant_300m_Mean_ndvi, 
        z=pg_for_lm$rentown_parcel)


ggplot(pg_for_lm, aes(x=CUR_TOTAL, y=Participant_300m_Mean_ndvi) ) +
  geom_point() +
  stat_smooth() + theme_bw() + theme(text = element_text(size = 17), 
                                     legend.text = element_text(size = 12),
                                    ) + 
  scale_fill_manual(name = "Rent-own")


