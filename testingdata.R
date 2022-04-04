#####################################################
### Author: Anagha Uppal
### Date: 12-18-2020
### Purpose: To create COVID testing data summary,
### first, we preprocess data with new columns,
### then pull into Arc to spatial join sewersheds
### because join in R was problematic
### then we pull back in and create summary tables
### Two tables: 1 - data grouped by sewershed
### 2 - data grouped by shed AND sampling type
### Finally export
### Metadata in testingdata/output_metadata.txt
#####################################################

library(haven)
library(dplyr)
library(rlang)
library(sf)
setwd("C:/Users/a0uppa01/Documents/GIS/testing_data")
sasdata_1208 <- read_sas("af_r1r2r3.sas7bdat")
sasdata_1208_2 <- sasdata_1208 %>% 
  dplyr::select(LATITUDE, LONGITUDE, PCR_RESULT, ANTIBODY_RESULT, ROUNDNAME) %>%
  mutate(ROUNDNAME = as.factor(ROUNDNAME)) %>%
  mutate(vol_sampling = if_else(ROUNDNAME=="r1vol" | ROUNDNAME=="r2vol" | 
                                  ROUNDNAME=="r3vol", 1, 0)) %>%
  mutate(rand_sampling = as.factor(if_else(ROUNDNAME=="r1wes" | ROUNDNAME=="r2wes" | 
                                             ROUNDNAME=="r3wes", 1, 0))) %>%
  mutate(PCR_POSITIVE = as.factor(if_else(PCR_RESULT=="2", 1, 0, missing=0))) %>%
  mutate(PCR_NEGATIVE = as.factor(if_else(PCR_RESULT=="1", 1, 0, missing=0))) %>%
  mutate(ANTIBODY_POSITIVE = as.factor(if_else(ANTIBODY_RESULT=="2", 1, 0))) %>%
  mutate(ANTIBODY_NEGATIVE = as.factor(if_else(ANTIBODY_RESULT=="1", 1, 0)))

write.csv(sasdata_1230, "sasdata.csv")
write.csv(sasdata_1230_2, "sasdata1.csv")

setwd("C:/Users/a0uppa01/Documents/GIS/testing_data/af_r1r2r3_012021")
sasdata_1230 <- read_sas("af_r1r2r3.sas7bdat")
# Re-introduce dataset from sas type to csv type
sasdata_1230_2 <- sasdata_1230 %>% 
  dplyr::select(LATITUDE, LONGITUDE, PCR_RESULT, ANTIBODY_RESULT, ROUNDNAME) %>%
  mutate(ROUNDNAME = as.factor(ROUNDNAME)) %>%
  mutate(vol_sampling = if_else(ROUNDNAME=="r1vol" | ROUNDNAME=="r2vol" | 
                                        ROUNDNAME=="r3vol", 1, 0)) %>%
  mutate(rand_sampling = as.factor(if_else(ROUNDNAME=="r1wes" | ROUNDNAME=="r2wes" | 
                                            ROUNDNAME=="r3wes", 1, 0))) %>%
  mutate(PCR_POSITIVE = as.factor(if_else(PCR_RESULT=="2", 1, 0, missing=0))) %>%
  mutate(PCR_NEGATIVE = as.factor(if_else(PCR_RESULT=="1", 1, 0, missing=0))) %>%
  mutate(ANTIBODY_POSITIVE = as.factor(if_else(ANTIBODY_RESULT=="2", 1, 0))) %>%
  mutate(ANTIBODY_NEGATIVE = as.factor(if_else(ANTIBODY_RESULT=="1", 1, 0))) %>%
  mutate(LATITUDE = if (is.na(LATITUDE)) {NULL}) %>%
  mutate(ANTIBODY_NEGATIVE = as.factor(if_else(ANTIBODY_RESULT=="1", 1, 0)))
  
write.csv(sasdata_1230, "sasdata.csv")
write.csv(sasdata_1230_2, "sasdata1.csv")

# sasdata1 is pulled into arcgis,  projected,  joined to sewersheds 
# (sewersheds is target fc) one to many
participants_sewersheds2 <- st_read("participants_sewersheds2.shp")

#General info
sum(is.na(sasdata_1208$LONGITUDE)) ## 97
nrow(subset(sasdata_1208, LONGITUDE==0)) ## 8
nrow(subset(sasdata_1208, APLOCATION=="")) ## 27
nrow(subset(sasdata_1208, Home=="")) ## 27
noaddress_1208 <- subset(sasdata_1208, APLOCATION=="")
sum(participants_sewersheds$ShedNUM==0) ## 145
sum(is.na(participants_sewersheds1$LONGITUDE)) ## 97

#General info
sum(is.na(sasdata_1230$LONGITUDE)) ## 41
nrow(subset(sasdata_1230, LONGITUDE==0)) ## 0
nrow(subset(sasdata_1230, APLOCATION=="")) ## 6
noaddress_1230 <- subset(sasdata_1230, APLOCATION=="")
sum(participants_sewersheds2$ShedNUM==0) ## 150
sum(is.na(participants_sewersheds2$LONGITUDE)) ## 0

write.csv(noaddress_1208, "noaddress_1208.csv")
write.csv(noaddress_1230, "noaddress_1230.csv")

results1 = anti_join(sasdata_1208, sasdata_1230, by = c("RECORD_ID" = "RECORD_ID"))

# Results sitting in PCR_RESULT
# Vol/convenience rounds in ROUNDNAME
# Locations in X, Y columns
# sEWERSHED id IN sHEDnum

onetable <- participants_sewersheds2 %>%
  group_by(ShedNUM) %>%
  dplyr::summarise(
    ShedName = first(Address),
    n = n(),
    PCR_POSITIVE = sum(PCR_POSITI),
    PCR_NEGATIVE = sum(PCR_NEGATI),
    ANTIBODY_POSITIVE = sum(ANTIBODY_P),
    ANTIBODY_NEGATIVE = sum(ANTIBODY_N)
  ) %>%
  mutate(PCR_POSPLUSNEG = PCR_POSITIVE + PCR_NEGATIVE) %>%
  mutate(ANTIBODY_POSPLUSNEG = ANTIBODY_POSITIVE + ANTIBODY_NEGATIVE) %>%
  mutate(PCR_PCT_N = PCR_POSITIVE/n*100) %>%
  mutate(ANTIBODY_PCT_N = ANTIBODY_POSITIVE/n*100) %>%
  mutate(PCR_PCT_POS_NEG = PCR_POSITIVE/PCR_POSPLUSNEG*100) %>%
  mutate(ANTIBODY_PCT_POS_NEG = ANTIBODY_POSITIVE/ANTIBODY_POSPLUSNEG*100) %>%
  st_drop_geometry() %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

newtable <- participants_sewersheds2 %>% 
  mutate(sampling_type = if_else(ROUNDNAME=="r1vol" | ROUNDNAME=="r2vol" | 
                                ROUNDNAME=="r3vol", "volunteer", "random")) %>%
  group_by(ShedNUM, sampling_type) %>%
  dplyr::summarise(
    ShedName = first(Address),
    n = n(),
    PCR_POSITIVE = sum(PCR_POSITI),
    PCR_NEGATIVE = sum(PCR_NEGATI),
    ANTIBODY_POSITIVE = sum(ANTIBODY_P),
    ANTIBODY_NEGATIVE = sum(ANTIBODY_N)
  ) %>%
  mutate(PCR_POSPLUSNEG = PCR_POSITIVE + PCR_NEGATIVE) %>%
  mutate(ANTIBODY_POSPLUSNEG = ANTIBODY_POSITIVE + ANTIBODY_NEGATIVE) %>%
  mutate(PCR_PCT_N = PCR_POSITIVE/n*100) %>%
  mutate(ANTIBODY_PCT_N = ANTIBODY_POSITIVE/n*100) %>%
  mutate(PCR_PCT_POS_NEG = PCR_POSITIVE/PCR_POSPLUSNEG*100) %>%
  mutate(ANTIBODY_PCT_POS_NEG = ANTIBODY_POSITIVE/ANTIBODY_POSPLUSNEG*100) %>%
  st_drop_geometry() %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

write.csv(onetable, "testingsummary_byshednum_AU_20210127.csv")
write.csv(newtable, "testingsummary_byshednum_bysampling_AU_20210127.csv")

bigsheds <- participants_sewersheds2 %>%
  filter(ShedNUM==1 | ShedNUM==2 | ShedNUM==0) %>%
  st_drop_geometry()
smallsheds <- participants_sewersheds2 %>%
  filter(!(ShedNUM==1 | ShedNUM==2 | ShedNUM==0)) %>%
  st_drop_geometry()

results2 = anti_join(bigsheds, smallsheds, by = c("TARGET_FID" = "TARGET_FID"))
results2 <- results2 %>% filter(ShedNUM==1 | ShedNUM==2 | ShedNUM==0)


onetable2 <- results2 %>%
  dplyr::summarise(
    ShedName = first(Address),
    n = n(),
    PCR_POSITIVE = sum(PCR_POSITI),
    PCR_NEGATIVE = sum(PCR_NEGATI),
    ANTIBODY_POSITIVE = sum(ANTIBODY_P),
    ANTIBODY_NEGATIVE = sum(ANTIBODY_N)
  ) %>%
  mutate(PCR_POSPLUSNEG = PCR_POSITIVE + PCR_NEGATIVE) %>%
  mutate(ANTIBODY_POSPLUSNEG = ANTIBODY_POSITIVE + ANTIBODY_NEGATIVE) %>%
  mutate(PCR_PCT_N = PCR_POSITIVE/n*100) %>%
  mutate(ANTIBODY_PCT_N = ANTIBODY_POSITIVE/n*100) %>%
  mutate(PCR_PCT_POS_NEG = PCR_POSITIVE/PCR_POSPLUSNEG*100) %>%
  mutate(ANTIBODY_PCT_POS_NEG = ANTIBODY_POSITIVE/ANTIBODY_POSPLUSNEG*100) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

newtable2 <- results2 %>% 
  mutate(sampling_type = if_else(ROUNDNAME=="r1vol" | ROUNDNAME=="r2vol" | 
                                   ROUNDNAME=="r3vol", "volunteer", "random")) %>%
  group_by(sampling_type) %>%
  dplyr::summarise(
    ShedName = first(Address),
    n = n(),
    PCR_POSITIVE = sum(PCR_POSITI),
    PCR_NEGATIVE = sum(PCR_NEGATI),
    ANTIBODY_POSITIVE = sum(ANTIBODY_P),
    ANTIBODY_NEGATIVE = sum(ANTIBODY_N)
  ) %>%
  mutate(PCR_POSPLUSNEG = PCR_POSITIVE + PCR_NEGATIVE) %>%
  mutate(ANTIBODY_POSPLUSNEG = ANTIBODY_POSITIVE + ANTIBODY_NEGATIVE) %>%
  mutate(PCR_PCT_N = PCR_POSITIVE/n*100) %>%
  mutate(ANTIBODY_PCT_N = ANTIBODY_POSITIVE/n*100) %>%
  mutate(PCR_PCT_POS_NEG = PCR_POSITIVE/PCR_POSPLUSNEG*100) %>%
  mutate(ANTIBODY_PCT_POS_NEG = ANTIBODY_POSITIVE/ANTIBODY_POSPLUSNEG*100) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

write.csv(onetable2, "testingsummary2_byshednum_AU_20210127.csv")
write.csv(newtable2, "testingsummary2_byshednum_bysampling_AU_20210127.csv")