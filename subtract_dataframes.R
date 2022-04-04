
library(dplyr)
# Grab the data
all <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/Outputs/GH_Parcels1.csv")
gh <- read.csv("I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/Outputs/GH_Participant_Parcels1.csv")
# make data types identical
all <- all %>% mutate_all(as.character)
gh <- gh %>% mutate_all(as.character)
# in one but not in the other
isleft <- all[ !(all$PARCELID %in% gh$PARCELID), ]
# write out
write.csv(isleft, "I:/Backups/GIS/Local_Computer_Stuff/GIS/GH/Outputs/Non_GH_Parcels1.csv")
