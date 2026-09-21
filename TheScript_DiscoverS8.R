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

SetupFolder <- '/Users/Nate/Documents/ManualQC/Discover S8'
SetupCSV <- list.files(SetupFolder, pattern = "csv", full.names = T)[1]
max_cols <- max(count.fields(SetupCSV, sep=","))
col_names <- paste0("V", 1:max_cols)
SetupCSV <- read.csv(SetupCSV, check.names = F, header = F, col.names = col_names, fill=T)
Setup2 <- as.data.frame(t(SetupCSV))
colnames(Setup2) <- Setup2[1,]
Setup2 <- Setup2[-1,]
DateTime <- mdy_hms(Setup2$`Setup Data time:`[1])
LaserDelays <- Setup2[1:5,13:19]
SetupImg <- as.data.frame(t(Setup2[1:4,21:27]))
colnames(SetupImg) <- SetupImg[1,]
SetupImg <- SetupImg[-1,]
SetupDetectors <- SetupCSV[41:126,2:19]
colnames(SetupDetectors) <- SetupCSV[40,2:19]
DetectorData <- SetupDetectors[,c(1,5,6,10,11)]
DetectorData$Date<- as.Date(DateTime)
#colnames(DetectorData) <- c("Name", "IsOnTarget", "GainValue", "MFIValue", "rCVValue", "Date")
NewData <- DetectorData %>% pivot_wider(id_cols = Date, names_from = Name, values_from = c(' MFI-A',' rCV','Gain (dB)',IsOnTarget))
