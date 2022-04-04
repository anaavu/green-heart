library(edf)
library(edfReader)
library(dplyr)

fileloc <- "C:/Users/a0uppa01/Downloads/09-22-32.edf"

gc() #this cleans out your memory to be able to take in big files
memory.limit(size=800) #opens up your RAM to use 800MB. Increase more if get errors again
hdr <- edfReader::readEdfHeader(fileloc)
myfile2 <- edfReader::readEdfSignals(hdr, 
                                     signals = "All", from = 0, till = Inf,
                                     physical = TRUE, fragments = FALSE, 
                                     recordStarts = FALSE,
                                     mergeASignals = TRUE, simplify = TRUE)

HRV <- myfile2$HRV
HRV_signal <- HRV$signal
HRV_signal_df <- as.data.frame(HRV_signal)
View(HRV_signal_df)
print(summary(HRV_signal_df))