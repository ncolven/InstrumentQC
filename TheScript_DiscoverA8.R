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
library(FHFCQC)

# Find out current date
Today <- Sys.Date()
Today <- as.Date(Today)

Instrument <- "Discover A8"
SetupFolder <- '/Users/Nate/Documents/ManualQC/Discover A8'
WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
ArchiveFolder <- file.path(WorkingDirectory, "data", Instrument, "Archive")
SetupCSV <- list.files(SetupFolder, pattern = "csv", full.names = T)

QCs <- list.files(ArchiveFolder, pattern = "Detector", full.names = T)
if (length(QCs)==0){
  LastQCItem <- as.Date("2026/09/01")
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
  ImgQC_Files <- QC_Files[!str_detect(QC_Files, "HighSpeed")]
  ParsedImg <- bind_rows(map(.x = ImgQC_Files, .f=ImgParse))
  ParsedImg[] <- lapply(ParsedImg, as.character)
  ArchiveCSV <- list.files(ArchiveFolder, pattern = "Imaging", full.names = T)
  if (!length(ArchiveCSV)==0){
    ArchiveData <- read.csv(ArchiveCSV, check.names = F)
    ArchiveData[] <- lapply(ParsedImg, as.character) 
    UpdatedData <- bind_rows(ArchiveData, ParsedImg)
  } else {
    UpdatedData <- ParsedImg
  }
  UpdatedData <- UpdatedData %>% distinct() %>% arrange(desc(DateTime))
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
  UpdatedData <- UpdatedData %>% distinct() %>% arrange(desc(DateTime))
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


