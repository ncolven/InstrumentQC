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

ImgParse <- function (SetupCSV){
  max_cols <- max(count.fields(SetupCSV, sep=","))
  col_names <- paste0("V", 1:max_cols)
  SetupCSV <- read.csv(SetupCSV, check.names = F, header = F, col.names = col_names, fill=T)
  StatusRow <- which(SetupCSV[,1]=="Status:")
  SetupStatus <- SetupCSV[StatusRow,2]
  if (str_detect(SetupStatus,"Cancelled")){
    return(NULL)
  }
  Setup2 <- as.data.frame(t(SetupCSV))
  colnames(Setup2) <- Setup2[1,]
  Setup2 <- Setup2[-1,]
  DateTime <- mdy_hms(Setup2$`Setup Data time:`[1])
  
  SetupImg <- as.data.frame(t(Setup2[1:4,21:27]))
  colnames(SetupImg) <- SetupImg[1,]
  SetupImg <- SetupImg[-1,]
  SetupImg$DateTime <- DateTime
  SetupImg <- SetupImg %>% relocate(DateTime) 
  SetupImg <- SetupImg %>% pivot_wider(id_cols = 'DateTime', 
                                       names_from = Channel, 
                                       values_from = colnames(SetupImg)[3:5])
  rownames(SetupImg) <- NULL
  return(SetupImg)
}
DiscoverParse <- function (SetupCSV){
  # Fluorescent detectors
  max_cols <- max(count.fields(SetupCSV, sep=","))
  col_names <- paste0("V", 1:max_cols)
  SetupCSV <- read.csv(SetupCSV, check.names = F, header = F, col.names = col_names, fill=T)
  StatusRow <- which(SetupCSV[,1]=="Status:")
  SetupStatus <- SetupCSV[StatusRow,2]
  if (str_detect(SetupStatus,"Cancelled")){
    return(NULL)
  }
  Setup2 <- as.data.frame(t(SetupCSV))
  colnames(Setup2) <- Setup2[1,]
  Setup2 <- Setup2[-1,]
  DateTime <- mdy_hms(Setup2$`Setup Data time:`[1])
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
  return(NewData)
}
Instrument <- "Discover S8"
SetupFolder <- '/Users/Nate/Documents/ManualQC/Discover S8'
WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
ArchiveFolder <- file.path(WorkingDirectory, "data", Instrument, "Archive")
SetupCSV <- list.files(SetupFolder, pattern = "csv", full.names = T)

QCs <- list.files(ArchiveFolder, pattern = "Detector", full.names = T)
if (length(QCs)==0){
  LastQCItem <- as.Date("20260901")
}else{
QCs <- read.csv(QCs[1], check.names = F)
LastQCItem <- QCs$DateTime[1] %>% ymd_hms(.) %>% as.Date(.)
}
PotentialQCDays <- seq.Date(from = LastQCItem, to = Today, by = "day")
QCRemoveIndex <- which(PotentialQCDays == LastQCItem)
PotentialQCDays <- PotentialQCDays[-QCRemoveIndex]

TheQCFiles <- list.files(SetupFolder, pattern = "csv", full.names = T, recursive = T)
QCMatches <- TheQCFiles[str_detect(basename(TheQCFiles), str_c(PotentialQCDays, collapse = "|"))]

if (length(QCMatches) > 0){
  file.copy(QCMatches, WorkingFolder)
  QC_Files <- list.files(WorkingFolder, pattern = "csv", full.names = T)
  
  # Imaging Parameters
  ParsedImg <- bind_rows(map(.x = QC_Files, .f=ImgParse))
  ParsedImg[] <- lapply(ParsedImg, as.character)
  ArchiveCSV <- list.files(ArchiveFolder, pattern = "Imaging", full.names = T)
  if (!length(ArchiveCSV)==0){
    ArchiveData <- read.csv(ArchiveCSV, check.names = F)
    ArchiveData[] <- lapply(ParsedImg, as.character) 
    UpdatedData <- bind_rows(ArchiveData, ParsedImg)
  } else {
    UpdatedData <- ParsedImg
  }
  UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
  ArchivePath <- file.path(ArchiveFolder, paste0("ImagingData", Instrument, ".csv"))
  write.csv(UpdatedData, ArchivePath, row.names = F)
  
  # Fluorescent Parameters
  ParsedFluor <- bind_rows(map(.x=QC_Files, .f=DiscoverParse))
  ParsedFluor[] <- lapply(ParsedFluor, as.character)
  ArchiveCSV <- list.files(ArchiveFolder, pattern = "Detector", full.names = T)
  if (!length(ArchiveCSV)==0){
    ArchiveData <- read.csv(ArchiveCSV, check.names = F)
    ArchiveData[] <- lapply(ArchiveData, as.character)
    UpdatedData <- bind_rows(ArchiveData, ParsedFluor)
  } else {
    UpdatedData <- ParsedFluor
  }
  UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
  ArchivePath <- file.path(ArchiveFolder, paste0("DetectorData", Instrument, ".csv"))
  write.csv(UpdatedData, ArchivePath, row.names = F)
  file.remove(QC_Files)
}

# Historic Data
HistoricData$Instrument <- Instrument
HistoricData$Date <- as.Date(HistoricData$DateTime)
HistoricData <- HistoricData %>% relocate('DateTime', Date, Instrument)
HistoricData <- HistoricData[,-1]
colnames(HistoricData) <- c("Date", "Instrument", "Name", "Gain", "GainValue", "MFIValue", "rCVValue")
HistoricData <- HistoricData %>% relocate(GainValue, .before = Gain)
HistoricData$rCV <- NA
HistoricData <- HistoricData %>% relocate(c(rCVValue, rCV), .before = MFIValue)


