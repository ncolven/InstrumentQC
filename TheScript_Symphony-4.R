#git config --global user.email ""
#git config --global user.name ""
#usethis::edit_r_environ()

username <- Sys.info()["user"]

# Setup in Correct Directory
Linux <- file.path("/Users", "Nate", "Documents", "InstrumentQC", "InstrumentQC")
Windows <- file.path("C:", "Users", username, "Documents", "InstrumentQC")

OperatingSystem <- Sys.info()["sysname"]
# if(OperatingSystem == "Linux"){OS <- Linux
# } else if (OperatingSystem == "Windows"){OS <- Windows}
OS <- Linux

WorkingDirectory <- OS
setwd(WorkingDirectory)
source("renv/activate.R")

library(stringr)
library(purrr)
library(dplyr)
library(openCyto)
library(flowWorkspace)
library(flowCore)
library(lubridate)
library(Luciernaga)

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
  #git2r::pull(TheRepo)
  
  # Locating Archive Folder
  Instrument <- "Symphony-4"
  MainFolder <- file.path(WorkingDirectory, "data")
  WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
  StorageFolder <- file.path(WorkingFolder, "Archive")
  
  MFIs <- list.files(StorageFolder, pattern="Holistic", full.names=TRUE)
  MFIs <- read.csv(MFIs[1], check.names=FALSE)
  LastMFIsItem <- MFIs |> dplyr::slice(1) |> dplyr::pull(DateTime)
  LastMFIsItem <- lubridate::ymd_hms(LastMFIsItem)
  LastMFIsItem <- as.Date(LastMFIsItem)
  PotentialMFIsDays <- seq.Date(from = LastMFIsItem, to = Today, by = "day")
  MFIsRemoveIndex <- which(PotentialMFIsDays == LastMFIsItem)
  PotentialMFIsDays <- PotentialMFIsDays[-MFIsRemoveIndex]
  
  if (!length(PotentialMFIDays) == 0){
    # MFI Starting Locations
    #SetupFolder <- #file.path(SetupFolder, "DailyQC")
    TheFCSFiles <- list.files(SetupFolder, pattern="fcs", full.names=TRUE, recursive=TRUE)
    
    days <- format(PotentialMFIsDays, "%m%d")
    
    MFIMatches <- TheFCSFiles[str_detect(basename(TheFCSFiles), str_c(days, collapse = "|"))]
    
    if (!length(MFIMatches) == 0){
      file.copy(MFIMatches, WorkingFolder)
      Template <- file.path(WorkingDirectory, "Symphony-4.csv")
      # Rainbow bead Parse
      {
        Folder <- file.path(MainFolder, Instrument)
        FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
        if (!length(FCS_Files) == 0) {
          The_CS <- load_cytoset_from_fcs(files = FCS_Files, 
                                          transformation = FALSE, truncate_max_range = FALSE)
          DateFormat <- keyword(The_CS[[1]])$`$DATE`
          if (DateFormat == "01-Jan-0001") {
            sample.name1 <- "$FIL"
          }
          else {
            sample.name1 <- "$DATE"
          }
          Gating <- data.table::fread(Template)
          MyGatingSet <- GatingSet(The_CS)
          MyGatingTemplate <- gatingTemplate(Gating)
          gt_gating(MyGatingTemplate, MyGatingSet)
          Parsed <- bind_rows(map(.x = MyGatingSet, .f = Luciernaga::QC_GainMonitoring, 
                                  subsets = "Beads", sample.name = sample.name1, 
                                  stats = "median"))
          if (DateFormat == "01-Jan-0001") {
            Parsed <- mutate(Parsed, DATE = gsub("DailyQCDataSample_", 
                                                 "", SAMPLE))
            Parsed <- mutate(Parsed, TIME = sub("^[^_]*_", 
                                                "", DATE))
            Parsed <- mutate(Parsed, DATE = sub("_.*", "", 
                                                DATE))
            Parsed$DATE <- ymd(Parsed$DATE)
            Parsed <- mutate(Parsed, TIME = str_c(str_sub(TIME, 
                                                          1, 2), ":", str_sub(TIME, 3, 4), ":", str_sub(TIME, 
                                                                                                        5, 6)), TIME = lubridate::hms(TIME))
          }
          Parsed <- relocate(mutate(Parsed, DateTime = DATE + TIME), 
                             DateTime, .before = DATE)
          Parsed <- arrange(Parsed, desc(DateTime))
          ArchiveFolder <- file.path(Folder, "Archive")
          ArchiveCSV <- list.files(ArchiveFolder, pattern = "Holistic", 
                                   full.names = TRUE)
          if (!length(ArchiveCSV) == 0) {
            if (!length(ArchiveCSV) > 1) {
              ArchiveData <- read.csv(ArchiveCSV, check.names = FALSE)
              ArchiveData$DateTime <- lubridate::ymd_hms(ArchiveData$DateTime)
              ArchiveData$DATE <- lubridate::ymd(ArchiveData$DATE)
              ArchiveData$TIME <- lubridate::hms(ArchiveData$TIME)
              if (!ncol(Parsed) == ncol(ArchiveData)) {
                message("Mismatched Number of Columns")
              }
              NewData <- Parsed %>% anti_join(ArchiveData, 
                                              by = c("DATE", "TIME"))
              UpdatedData <- bind_rows(NewData, ArchiveData)
              file.remove(ArchiveCSV)
            }
            else {
              stop("Two Holistic csv files in the archive folder!")
            }
          }
          else {
            UpdatedData <- Parsed
          }
          UpdatedData <- UpdatedData %>% arrange(desc(DateTime))
          file.remove(FCS_Files)
          name <- paste0("HolisticData", Instrument, ".csv")
          StorageLocation <- file.path(ArchiveFolder, name)
          write.csv(UpdatedData, StorageLocation, row.names = FALSE)
        } else {
          message("No fcs files to update with in ", x)
        }
      }
      }
    } else {message("QC data has already been transferred")
    MFIMatches <- NULL
  }
  
  
  if (length(PotentialMFIsDays) > 0){
    
    if (length(MFIMatches) > 0){
      # Stage to Git
      git2r::add(TheRepo, "*")
      
      TheCommitMessage <- paste0("Update for ", Instrument, " on ", Today)
      git2r::commit(TheRepo, message = TheCommitMessage)
      #cred <- git2r::cred_token(token = "GITHUB_PAT")
      cred <- git2r::cred_ssh_key(publickey = ssh_path("id_ed25519.pub"), privatekey = ssh_path("id_ed25519"))
      git2r::push(TheRepo, credentials = cred)
      message("Done ", Today)
    } else {message("No files to process ", Today)}
  } else {message("No files to process 2", Today)}
} else {message("Automation Skipped ", Today)}


