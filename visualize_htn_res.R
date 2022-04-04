library(corrplot)
library(ztable)
library(devtools)

options(ztable.type = "html")

tableres <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/GreenHeart/DataOutputsandDeliverables/SES Paper/FinalTables/unadj_allparticipants_table2.csv")

tableres1 <- tableres[2:22,2:14]
tableres1[tableres1 == "-"] <- NA  
tableres2 <- sapply( tableres1, as.numeric )
tableres1$All0 <- as.numeric(tableres1$All0) %>% unlist()
tableres1$All1 <- as.numeric(tableres1$All1) %>% unlist()
tableres2 <- tableres1[, 1:2]

#corrplot(tableres2, is.corr = FALSE)

ztable1 <- ztable(tableres2)
print(ztable1)
ztable1 %>% makeHeatmap()
