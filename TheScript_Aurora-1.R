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
library(flowWorkspace)
library(flowCore)
library(openCyto)
library(lubridate)
library(dplyr)
library(Luciernaga)

QbSureParse <- function (x, MainFolder, Template) {
  Folder <- file.path(MainFolder, x)
  FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
  if (!length(FCS_Files) == 0) {
    The_CS <- load_cytoset_from_fcs(files = FCS_Files, 
                                    transformation = FALSE, truncate_max_range = FALSE)
    Gating <- data.table::fread(Template)
    MyGatingSet <- GatingSet(The_CS)
    MyGatingTemplate <- gatingTemplate(Gating)
    gt_gating(MyGatingTemplate, MyGatingSet)
    BeforeAfter <- flowWorkspace::lapply(MyGatingSet,Luciernaga:::QC_GainMonitoring, sample.name = "TUBENAME", 
                                         stats = "median", subsets = "2ndPeak")
    BeforeAfter <- bind_rows(BeforeAfter)
    BeforeAfter <- BeforeAfter %>% mutate(DateTime = DATE + 
                                            TIME) %>% relocate(DateTime, .before = DATE)
    BeforeAfter <- BeforeAfter %>% arrange(desc(DateTime))
    ArchiveFolder <- file.path(Folder, "Archive")
    ArchiveCSV <- list.files(ArchiveFolder, pattern = "Bead", 
                             full.names = TRUE)
    if (!length(ArchiveCSV) == 0) {
      if (!length(ArchiveCSV) > 1) {
        ArchiveData <- read.csv(ArchiveCSV, check.names = FALSE)
        ArchiveData$DateTime <- lubridate::ymd_hms(ArchiveData$DateTime)
        ArchiveData$DATE <- lubridate::ymd(ArchiveData$DATE)
        ArchiveData$TIME <- lubridate::hms(ArchiveData$TIME)
        if (!ncol(BeforeAfter) == ncol(ArchiveData)) {
          stop("Mismatched Number of Columns")
        }
        NewData <- BeforeAfter %>% anti_join(ArchiveData, 
                                             by = c("DATE", "TIME"))
        UpdatedData <- rbind(NewData, ArchiveData)
        file.remove(ArchiveCSV)
      }
      else {
        stop("Two BeadData csv files in the archive folder!")
      }
    }
    else {
      UpdatedData <- BeforeAfter
    }
    UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
    file.remove(FCS_Files)
    name <- paste0("BeadData", x, ".csv")
    StorageLocation <- file.path(ArchiveFolder, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No fcs files to update with in ", x)
  }
}

# Find out current date
Today <- Sys.Date()
Today <- as.Date(Today)

# Check for Flag Files

AnyFlags <- list.files(WorkingDirectory, pattern="Flag.csv", full.names=TRUE)

if (length(AnyFlags) == 0){

# Git Pull
RepositoryPath <- WorkingDirectory
RepositoryPath <- file.path(RepositoryPath, ".git")
TheRepo <- git2r::repository(RepositoryPath, discover = FALSE)
#TheRepo <- git2r::repository(RepositoryPath)
git2r::pull(TheRepo)

# Locating Archive Folder
Instrument <- "Aurora-1"
MainFolder <- file.path(WorkingDirectory, "data")
WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
StorageFolder <- file.path(WorkingFolder, "Archive")

# Gains
Gains <- list.files(StorageFolder, pattern="Archived", full.names=TRUE)
Gains <- read.csv(Gains[1], check.names = FALSE)
LastGainItem <- Gains |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastGainItem <- lubridate::ymd_hms(LastGainItem)
#LastGainItem <- lubridate::mdy_hm(LastGainItem)
LastGainItem <- as.Date(LastGainItem)
PotentialGainDays <- seq.Date(from = LastGainItem, to = Today, by = "day")
GainRemoveIndex <- which(PotentialGainDays == LastGainItem)
PotentialGainDays <- PotentialGainDays[-GainRemoveIndex]

# MFIs
MFIs <- list.files(StorageFolder, pattern="Bead", full.names=TRUE)
MFIs <- read.csv(MFIs[1], check.names=FALSE)
LastMFIItem <- MFIs |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastMFIItem <- lubridate::ymd_hms(LastMFIItem)
LastMFIItem <- as.Date(LastMFIItem)
PotentialMFIDays <- seq.Date(from = LastMFIItem, to = Today, by = "day")
MFIRemoveIndex <- which(PotentialMFIDays == LastMFIItem)
PotentialMFIDays <- PotentialMFIDays[-MFIRemoveIndex]
  
# Usage
Apps <- list.files(StorageFolder, pattern="Application", full.names=TRUE)
Apps <- read.csv(Apps[1], check.names=FALSE)
LastAppsItem <- Apps |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastAppsItem <- lubridate::ymd_hms(LastAppsItem)
LastAppsItem <- as.Date(LastAppsItem)
PotentialAppsDays <- seq.Date(from = LastAppsItem, to = Today, by = "day")
AppsRemoveIndex <- which(PotentialAppsDays == LastAppsItem)
PotentialAppsDays <- PotentialAppsDays[-AppsRemoveIndex]

if (!length(PotentialGainDays) == 0){
# Gain Starting Locations

SetupFolder <- file.path("C:", "CytekbioExport", "Setup")
TheSetupFiles <- list.files(SetupFolder, pattern="DailyQC", full.names=TRUE)

Dates <- as.character(PotentialGainDays)
Dates <- gsub("-", "", Dates)

GainMatches <- TheSetupFiles[str_detect(TheSetupFiles, str_c(Dates, collapse = "|"))]

if (!length(GainMatches) == 0){
  file.copy(GainMatches, WorkingFolder)
  walk(.x=Instrument, .f=Luciernaga:::DailyQCParse, MainFolder=MainFolder)
}
} else {message("QC data has already been transferred")
  GainMatches <- NULL
  }
  
if (!length(PotentialMFIDays) == 0){
# MFI Starting Locations

FCSFolder <- file.path("D:", "CytekData","Experiments", "Admin", "Aurora QC","Raw")
MonthFolder <- format(Today, "%B %Y")
MonthFolder <- file.path(FCSFolder, MonthFolder)
TheFCSFiles <- list.files(MonthFolder, pattern="fcs", full.names=TRUE, recursive=TRUE)

days <- format(PotentialMFIDays, "%d")

MFIMatches <- TheFCSFiles[str_detect(basename(TheFCSFiles), str_c(days, collapse = "|"))]

if (!length(MFIMatches) == 0){
file.copy(MFIMatches, WorkingFolder)
Template <- file.path(WorkingDirectory, "Aurora-1.csv")
walk(.x=Instrument, .f=QbSureParse, MainFolder=MainFolder, Template=Template)
}
} else {message("QC data has already been transferred")
  MFIMatches <- NULL
  }

if (!length(PotentialAppsDays) == 0){
  SetupFolder <- file.path("C:", "CytekbioExport")
    TheSetupFiles <- list.files(SetupFolder, pattern="Application", full.names=TRUE)
    MonthStyle <- format(Today, "%Y-%m")
    MonthStyle <- sub("([0-9]{4})-([0-9]{2})", "\\2-\\1", MonthStyle)
    MonthStyle <- gsub("-", " ", MonthStyle)
    MonthStyle <- paste0(MonthStyle, ".txt")
  
    AppMatches <- TheSetupFiles[str_detect(TheSetupFiles, str_c(MonthStyle, collapse = "|"))]
    
    if (!length(AppMatches) == 0){
      if (any(length(GainMatches)|length(MFIMatches) > 0)){
      file.copy(AppMatches, WorkingFolder)
      walk(.x=Instrument, .f=Luciernaga:::AppQCParse, MainFolder=MainFolder)
      }
      }
} else {message("QC data has already been transferred")
    AppMatches <- NULL
    }

if (any(length(PotentialGainDays)|length(PotentialMFIDays)|length(PotentialAppsDays) > 0)){
  
  if (any(length(GainMatches)|length(MFIMatches) > 0)){
    # Stage to Git
    git2r::add(TheRepo, "*")
    
    TheCommitMessage <- paste0("Update for ", Instrument, " on ", Today)
    git2r::commit(TheRepo, message = TheCommitMessage)
    cred <- git2r::cred_token(token = "GITHUB_PAT")
    git2r::push(TheRepo, credentials = cred)
    message("Done ", Today)
  } else {message("No files to process ", Today)}
} else {message("No files to process 2", Today)}
} else {message("Automation Skipped ", Today)}


