#git config --global user.email ""
#git config --global user.name ""
#usethis::edit_r_environ()

username <- Sys.info()["user"]

# Setup in Correct Directory
WorkingDirectory <- file.path("C:", "Users", username, "Documents", "InstrumentQC")
setwd(WorkingDirectory)
#source("renv/activate.R")

library(stringr)
library(purrr)
library(dplyr)
library(openCyto)
library(flowWorkspace)
library(flowCore)
library(lubridate)
library(Luciernaga)
library(tidyr)

# Find out current date
Today <- Sys.Date()
Today <- as.Date(Today)

Instrument <- "Discover S8"
SetupFolder <- '/Users/Nate/Documents/ManualQC/Discover S8'
ArchiveFolder <- file.path(WorkingDirectory, "data", Instrument, "Archive")
SetupCSV <- list.files(SetupFolder, pattern = "csv", full.names = T)[1]
max_cols <- max(count.fields(SetupCSV, sep=","))
col_names <- paste0("V", 1:max_cols)
SetupCSV <- read.csv(SetupCSV, check.names = F, header = F, col.names = col_names, fill=T)
Setup2 <- as.data.frame(t(SetupCSV))
colnames(Setup2) <- Setup2[1,]
Setup2 <- Setup2[-1,]
DateTime <- mdy_hms(Setup2$`Setup Data time:`[1])

# Imaging detectors
SetupImg <- as.data.frame(t(Setup2[1:4,21:27]))
colnames(SetupImg) <- SetupImg[1,]
SetupImg <- SetupImg[-1,]
SetupImg$DateTime <- DateTime
ArchiveCSV <- list.files(ArchiveFolder, pattern = "Imaging", full.names = T)
if (!length(ArchiveCSV)==0){
  ArchiveData <- read.csv(ArchiveCSV, check.names = F)
  UpdatedData <- bind_rows(ArchiveCSV, SetupImg)
} else {
  UpdatedData <- SetupImg
}
UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
ArchivePath <- file.path(ArchiveFolder, paste0("ImagingData", Instrument, ".csv"))
write.csv(UpdatedData, ArchivePath)

# Fluorescent detectors
SetupDetectors <- SetupCSV[41:126,2:19]
colnames(SetupDetectors) <- SetupCSV[40,2:19]
DetectorData <- SetupDetectors[,c(1,5,6,10,11)]
DetectorData$DateTime<- DateTime
HistoricData <- DetectorData
DetectorData <- DetectorData %>% pivot_wider(id_cols = DateTime, 
                                             names_from = Name, 
                                             values_from = c(' MFI-A',' rCV','Gain (dB)',IsOnTarget))
## Laser Delays
LaserDelays <- Setup2[1:5,13:19]
LaserDelays$DateTime <- DateTime 
LaserDelays <- LaserDelays %>% pivot_wider(id_cols = "DateTime",
                              names_from = Laser, 
                           values_from = colnames(LaserDelays)[2:7])
LaserDelays <- LaserDelays[,-1]
NewData <- bind_cols(DetectorData, LaserDelays)

ArchiveCSV <- list.files(ArchiveFolder, pattern = "Detector", full.names = T)
if (!length(ArchiveCSV)==0){
  ArchiveData <- read.csv(ArchiveCSV, check.names = F)
  UpdatedData <- bind_rows(ArchiveCSV, NewData)
} else {
  UpdatedData <- NewData
}
UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
ArchivePath <- file.path(ArchiveFolder, paste0("DetectorData", Instrument, ".csv"))
write.csv(UpdatedData, ArchivePath)

# Historic Data
HistoricData$Instrument <- Instrument
HistoricData$Date <- as.Date(HistoricData$DateTime)
HistoricData <- HistoricData %>% relocate('DateTime', Date, Instrument)
HistoricData <- HistoricData[,-1]
colnames(HistoricData) <- c("Date", "Instrument", "Name", "Gain", "GainValue", "MFIValue", "rCVValue")
HistoricData <- HistoricData %>% relocate(GainValue, .before = Gain)
HistoricData$rCV <- NA
HistoricData <- HistoricData %>% relocate(c(rCVValue, rCV), .before = MFIValue)


