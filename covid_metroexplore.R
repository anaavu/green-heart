# Libraries
library(dplyr)
# Read in data
covid_city_tests_orig <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID/RawData/COVID_Testing_20210408.csv")
covid_city_tests_orig_unique <- distinct(covid_city_tests_orig, Patient.Local.ID, 
                                         Specimen.Coll.Dt, Birth.Dt, .keep_all = TRUE)
covid_city_tests_pos_geo <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID/ProcessedData/CityData/PositiveGeocodes_CSV_JK.csv")
covid_city_tests_neg_geo <- read.csv("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID/ProcessedData/CityData/NegativeGeocodes_CSV_MC.csv")

#function to pick first string out of a string split-to use to split timestamp into date and time
date <- function(my.string){
  unlist(strsplit(my.string, " "))[1]
}

# Collection date records have the timestamp missing (converted to 00:00) 
# from the geocoded dataset. Removing all timestamps to make consistent
covid_city_tests_orig_unique$Specimen.Coll.Date <- sapply(covid_city_tests_orig_unique$Specimen.Coll.Dt, date)
covid_city_tests_pos_geo$USER_Specimen_Coll_Date <- sapply(covid_city_tests_pos_geo$USER_Specimen_Coll_Dt, date)


# Join positives geocoded to original test info
covid_city_tests <- left_join(covid_city_tests_orig_unique, covid_city_tests_pos_geo, 
                              c("Patient.Local.ID" = "USER_Patient_ID", 
                                "Specimen.Coll.Date" = "USER_Specimen_Coll_Date"))

write.csv(covid_city_tests, "C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID/ProcessedData/CityData/TestDatawithGeo_AU_20210408.csv")

sum(!is.na(covid_city_tests$USER_Specimen_Coll_Dt)) 
#Positive geocoded is 38101, while joined rows are 39947?
#Some records are being counted twice
#35757 -lost 2.5K

#Let's check which data were lost in the joining
covid_city_tests_joined <- inner_join(covid_city_tests_orig_unique, covid_city_tests_pos_geo, 
                                     c("Patient.Local.ID" = "USER_Patient_ID", 
                                       "Specimen.Coll.Date" = "USER_Specimen_Coll_Date"))
unjoined <- anti_join(covid_city_tests_pos_geo, covid_city_tests_joined, 
                      by = c("USER_Patient_ID" = "Patient.Local.ID", 
                             "USER_Specimen_Coll_Date" = "Specimen.Coll.Date"))
newunjoined<- subset(covid_city_tests_pos_geo, !(USER_Patient_ID %in% covid_city_tests_joined$Patient.Local.ID))


sort(table(covid_city_tests_orig$Patient.Local.ID), decreasing=T)
sort(table(covid_city_tests_orig_unique$Patient.Local.ID), decreasing=T)
guy_with_most_tests <- filter(covid_city_tests_orig_unique, Patient.Local.ID=="PSN12345855KY01")
table(guy_with_most_tests$Patient.Local.ID)
guy_with_2most_tests <- filter(covid_city_tests_orig_unique, Patient.Local.ID=="PSN20114316KY01")
 
households_patients <- covid_city_tests_orig_unique %>% group_by(Address.One, Patient.Local.ID) %>% count()
households <- covid_city_tests_orig_unique %>% group_by(Address.One) %>% count()
mean(households$n) #2.7
mean(households_patients$n) # 1.7
households_p2 <- households_patients %>% group_by(Address.One) %>% count()
mean(households_p2$n) #1.6

patients <- covid_city_tests_orig_unique %>% group_by(Patient.Local.ID) %>% count()
mean(patients$n) #2.2

na_count <- data.frame(sapply(covid_city_tests_orig_unique, 
                  function(y) sum(is.na(y))))
na_count$name<-rownames(na_count)

