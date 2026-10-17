#git config --global user.email ""
#git config --global user.name ""
#usethis::edit_r_environ()

# Setup in Correct Directory
username <- Sys.info()["user"]
WorkingDirectory <- file.path("C:","Users","My Aurora", "Documents", "InstrumentQC")
setwd(WorkingDirectory)

library(stringr)
library(purrr)
library(flowWorkspace)
library(flowCore)
library(openCyto)
library(lubridate)
library(dplyr)

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

FH_DailyQCPrep_CS <-function (x) {
  ReadInfo <- readLines(x)
  index <- grep("^Laser Settings", ReadInfo)
  Final <- length(ReadInfo)
  InitialIndex <- grep("^DailyQC", ReadInfo)
  Initial <- ReadInfo[InitialIndex]
  String <- gsub("DailyQCReport_", "", Initial)
  Parts <- strsplit(String, "_")[[1]]
  if (length(Parts) == 5) {
    Instrument <- data.frame(Instrument = Parts[1])
    Nozzle <- data.frame(Nozzle = Parts[2], Serial = Parts[3])
    Date <- data.frame(Date = Parts[4])
    Date$Date <- lubridate::ymd(Date$Date)
    Time <- data.frame(Time = Parts[5])
  } else if (length(Parts) == 2) {
    Instrument <- "Unknown"
    Date <- data.frame(Date = Parts[1])
    Date$Date <- lubridate::ymd(Date$Date)
    Time <- data.frame(Time = Parts[2])
  }  else {
    stop("File Format for ", x, " not recognized")
  }
  Time$Time <- sub("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", 
                   Time$Time)
  Time$Time <- lubridate::hms(Time$Time)
  Intro <- cbind(Date, Time, Instrument, Nozzle)
  Intro <- select(relocate(mutate(Intro, DateTime = Date + 
                                    Time), DateTime, .before = 1), -Date, -Time)
  StartDetector <- grep("^Laser,Detector", ReadInfo)
  DetectorSegment <- ReadInfo[StartDetector:(index - 3)]
  DetectorLength <- length(DetectorSegment)
  header <- strsplit(DetectorSegment[1], ",")[[1]]
  data <- DetectorSegment[3:DetectorLength]
  data <- strsplit(data, ",")
  TheData <- do.call(rbind, lapply(data, function(x) as.data.frame(t(x), 
                                                                   stringsAsFactors = FALSE)))
  colnames(TheData) <- header
  TheData$Detector <- gsub(" .*", "", TheData$Detector)
  colnames(TheData) <- gsub(" ", "", colnames(TheData))
  TheData$Gain <- as.integer(TheData$Gain)
  TheData$`%rCV` <- as.numeric(TheData$`%rCV`)
  if (any(colnames(TheData) == "DeltaGain")) {
    TheData$DeltaGain <- as.integer(TheData$DeltaGain)
  }
  if (any(colnames(TheData) == "GainChange")) {
    TheData <- rename(TheData, DeltaGain = GainChange)
    TheData$DeltaGain <- as.integer(TheData$DeltaGain)
  }
  Updated <- mutate(mutate(mutate(mutate(mutate(TheData, Baseline = Gain - 
                                                  DeltaGain), Comparison = Baseline * 2), GainFlag = Gain > 
                                    Comparison), RCVFlag = `%rCV` > 6), RCVFlag = case_when(Detector == 
                                                                                              "SSC" | Detector == "SSC-B" & `%rCV` < 8 ~ FALSE, TRUE ~ 
                                                                                              RCVFlag))
  TheGain <- select(Updated, Detector, Gain)
  TheGain$Detector <- paste0(TheGain$Detector, "-Gain")
  TheGain <- pivot_wider(TheGain, names_from = Detector, values_from = Gain)
  GainFlagged <- select(Updated, Detector, GainFlag)
  GainFlagged$Detector <- paste0("Flag-", GainFlagged$Detector, 
                                 "-Gain")
  GainFlagged <- pivot_wider(GainFlagged, names_from = Detector, 
                             values_from = GainFlag)
  TheRCV <- select(Updated, Detector, `%rCV`)
  TheRCV$Detector <- paste0(TheRCV$Detector, "-% rCV")
  TheRCV <- pivot_wider(TheRCV, names_from = Detector, values_from = `%rCV`)
  RCVFlagged <- select(Updated, Detector, RCVFlag)
  RCVFlagged$Detector <- paste0("Flag-", RCVFlagged$Detector, 
                                "-% rCV")
  RCVFlagged <- pivot_wider(RCVFlagged, names_from = Detector, 
                            values_from = RCVFlag)
  Assembly <- cbind(Intro, TheGain, GainFlagged, TheRCV, RCVFlagged)
  LaserSegment <- ReadInfo[index:Final]
  FSCIndex <- grep("^FSC", LaserSegment)
  FSCAreaScaling <- LaserSegment[FSCIndex]
  FSCAreaScaling <- strsplit(FSCAreaScaling, ",")[[1]]
  FSCArea <- data.frame(FSCAreaScalingFactor = FSCAreaScaling[[2]])
  FSCArea$FSCAreaScalingFactor <- as.numeric(FSCArea$FSCAreaScalingFactor)
  FSCArea <- mutate(FSCArea, across(everything(), ~FALSE, .names = "Flag-{.col}"))
  FlowRateIndex <- grep("^Flow", LaserSegment)
  if (length(FlowRateIndex) > 0) {
    FlowRate <- LaserSegment[FlowRateIndex]
    FlowRate <- strsplit(FlowRate, ",")[[1]]
    FlowRate <- data.frame(FlowRate = FlowRate[[2]])
    FlowRate$FlowRate <- as.numeric(FlowRate$FlowRate)
    FlowRate <- mutate(FlowRate, across(everything(), ~FALSE, 
                                        .names = "Flag-{.col}"))
    FSCArea <- cbind(FSCArea, FlowRate)
  }
  TemperatureIndex <- grep("^Temperature", LaserSegment)
  if (length(TemperatureIndex) > 0) {
    Temperature <- LaserSegment[TemperatureIndex]
    Temperature <- strsplit(Temperature, ",")[[1]]
    Temperature <- data.frame(Temperature = Temperature[[2]])
    Temperature$Temperature <- as.numeric(Temperature$Temperature)
    Temperature <- mutate(Temperature, across(everything(), 
                                              ~FALSE, .names = "Flag-{.col}"))
  } else {
    Temperature <- data.frame(Temperature = 0, `Flag-Temperature` = FALSE, 
                              check.names = FALSE)
  }
  if (length(FSCIndex) == 2) {
    LaserFrame <- LaserSegment[2:(FSCIndex[1] - 1)]
  } else {
    LaserFrame <- LaserSegment[2:(FSCIndex - 1)]
  }
  LaserFrame <- strsplit(LaserFrame, ",")
  LaserFrame <- do.call(rbind, lapply(LaserFrame, function(x) as.data.frame(t(x), 
                                                                            stringsAsFactors = FALSE)))
  colnames(LaserFrame) <- LaserFrame[1, ]
  LaserFrame <- LaserFrame[-1, ]
  colnames(LaserFrame) <- gsub(" L", "L", colnames(LaserFrame))
  colnames(LaserFrame) <- gsub(" A", "A", colnames(LaserFrame))
  if (any(str_detect(colnames(LaserFrame), "Laser Power"))) {
    LaserPower <- pivot_wider(select(LaserFrame, -`Area Scaling Factor`, 
                                     -`Laser Delay`), names_from = Laser, values_from = `Laser Power`, 
                              names_glue = "{Laser}-Laser Power")
    LaserPower <- mutate(LaserPower, across(everything(), 
                                            as.numeric))
    LaserPower <- mutate(LaserPower, across(everything(), 
                                            ~FALSE, .names = "Flag-{.col}"))
    LaserFrame <- select(LaserFrame, -`Laser Power`)
    FSCArea <- cbind(LaserPower, FSCArea)
  }
  LaserDelay <- pivot_wider(select(LaserFrame, -`Area Scaling Factor`), 
                            names_from = Laser, values_from = `Laser Delay`, names_glue = "{Laser}-Laser Delay")
  LaserDelay <- mutate(LaserDelay, across(everything(), as.numeric))
  LaserDelay <- mutate(LaserDelay, across(everything(), ~FALSE, 
                                          .names = "Flag-{.col}"))
  LaserArea <- pivot_wider(select(LaserFrame, -`Laser Delay`), 
                           names_from = Laser, values_from = `Area Scaling Factor`, 
                           names_glue = "{Laser}-Area Scaling Factor")
  LaserArea <- mutate(LaserArea, across(everything(), as.numeric))
  LaserArea <- mutate(LaserArea, across(everything(), ~FALSE, 
                                        .names = "Flag-{.col}"))
  NewData <- cbind(Assembly, LaserDelay, LaserArea, FSCArea, 
                   Temperature)
  NewData <- select(NewData, -Instrument)
  return(NewData)
}

FHDailyQCParse <-function (MainFolder, x) 
{
  Folder <- file.path(MainFolder, x)
  DailyQCFiles <- list.files(Folder, pattern = "DailyQC", full.names = TRUE)
  if (!length(DailyQCFiles) == 0) {
    if (length(DailyQCFiles) >= 1) {
      Parsed <- bind_rows(map(.x = DailyQCFiles, .f = FH_DailyQCPrep_CS))
      Parsed <- mutate(Parsed, across(starts_with("Flag"), 
                                      ~as.logical(.)))
    }
    else {
      stop("Two csv files in the folder found!")
    }
    ShinyData <- Luciernaga:::ShinyQCSummary(x = Parsed, Instrument = x)
    HistoricalPath <- file.path(MainFolder, "HistoricalData.csv")
    History <- list.files(MainFolder, pattern = "HistoricalData.csv", 
                          full.names = TRUE)
    if (length(History == 1)) {
      HistoricalData <- read.csv(HistoricalPath, check.names = FALSE)
      HistoricalData$Date <- lubridate::ymd(HistoricalData$Date)
      if (ncol(ShinyData) == ncol(HistoricalData)) {
        TheShiniestData <- bind_rows(ShinyData, HistoricalData)
        write.csv(TheShiniestData, HistoricalPath, row.names = FALSE)
      }
      else {
        stop("Shiny Historical Data Conflicting Column Numbers")
      }
    } else {
      write.csv(ShinyData, HistoricalPath, row.names = FALSE)
    }
    TheArchive <- file.path(Folder, "Archive")
    ArchivedDataFile <- list.files(TheArchive, pattern = "Archived", 
                                   full.names = TRUE)
    if (!length(ArchivedDataFile) == 0) {
      if (length(ArchivedDataFile) == 1) {
        ArchivedData <- read.csv(ArchivedDataFile[1], 
                                 check.names = FALSE)
      }else {
        message("Two csv files in the folder found!")
      }
      ArchivedData$DateTime <- lubridate::ymd_hms(ArchivedData$DateTime)
      ArchivedData <- mutate(ArchivedData, across(starts_with("Flag"), 
                                                  ~as.logical(.)))
      if (!ncol(ArchivedData) == ncol(Parsed)) {
        Recent <- setdiff(colnames(ArchivedData), colnames(Parsed))
        Previous <- setdiff(colnames(Parsed), colnames(ArchivedData))
        if (length(Previous) == 0) {
          UpToHere <- nrow(Parsed)
          WorkAround <- bind_rows(Parsed, ArchivedData)
          WorkAround1 <- WorkAround[1:UpToHere, ]
          NewData <- generics::setdiff(WorkAround1, ArchivedData)
          UpdatedData <- rbind(NewData, ArchivedData)
        } else {
          stop("Mismatched Columns, newer data fewer columns than old data")
        }
      } else {
        NewData <- generics::setdiff(Parsed, ArchivedData)
        UpdatedData <- rbind(NewData, ArchivedData)
      }
      file.remove(ArchivedDataFile)
    } else {
      UpdatedData <- Parsed
    }
    file.remove(DailyQCFiles)
    UpdatedData <- arrange(UpdatedData, desc(DateTime))
    name <- paste0("ArchivedData", x, ".csv")
    StorageLocation <- file.path(TheArchive, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No DailyQCFiles files to update with in ", x)
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
TheRepo <- git2r::repository(RepositoryPath)
git2r::pull(TheRepo)

# Locating Archive Folder
Instrument <- "CS"
MainFolder <- file.path(WorkingDirectory, "data")
WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
StorageFolder <- file.path(WorkingFolder, "Archive")

# Gains
Gains <- list.files(StorageFolder, pattern="Archived", full.names=TRUE)
Gains <- read.csv(Gains[1], check.names = FALSE)
LastGainItem <- Gains |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastGainItem <- lubridate::ymd_hms(LastGainItem)
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

if (!length(PotentialGainDays) == 0){
# Gain Starting Locations

SetupFolder <- file.path("C:", "CytekbioExport_CS", "Setup")
TheSetupFiles <- list.files(SetupFolder, pattern="DailyQC", full.names=TRUE)

Dates <- as.character(PotentialGainDays)
Dates <- gsub("-", "", Dates)

GainMatches <- TheSetupFiles[str_detect(TheSetupFiles, str_c(Dates, collapse = "|"))]

if (!length(GainMatches) == 0){
file.copy(GainMatches, WorkingFolder)
walk(.x=Instrument, .f=FHDailyQCParse, MainFolder=MainFolder)
}
} else {message("QC data has already been transferred")
  GainMatches <- NULL
  }

if (!length(PotentialMFIDays) == 0){
# MFI Starting Locations

FCSFolder <- file.path("C:", "CytekbioExport_CS", "FcsFiles", "Admin")
MonthStyle <- format(Today, "%Y-%m")
MonthFolder <- paste0("QC ", MonthStyle)
MonthFolder <- file.path(FCSFolder, MonthFolder)
TheFCSFiles <- list.files(MonthFolder, pattern="fcs", full.names=TRUE, recursive=TRUE)

days <- format(PotentialMFIDays, "%d")

MFIMatches <- TheFCSFiles[str_detect(basename(TheFCSFiles), str_c(days, collapse = "|"))]

if (!length(MFIMatches) == 0){
file.copy(MFIMatches, WorkingFolder)
walk(.x=Instrument, .f=Luciernaga:::QCBeadParse, MainFolder=MainFolder)
}
} else {message("QC data has already been transferred")
  MFIMatches <- NULL
  }

if (any(length(PotentialGainDays)|length(PotentialMFIDays) > 0)){
  
  if (any(length(GainMatches)|length(MFIMatches) > 0)){
    # Stage to Git
    git2r::add(TheRepo, "*")
    
    TheCommitMessage <- paste0("Update for ", Instrument, " on ", Today)
    git2r::commit(TheRepo, message = TheCommitMessage)
    cred <- git2r::cred_token(token = "GITHUB_PAT")
    git2r::push(TheRepo, credentials = cred)
    message("Done ", Today)
  } else {message("No files to process ", Today)}
} else {message("No files to process ", Today)}
} else {message("Automation Skipped ", Today)}