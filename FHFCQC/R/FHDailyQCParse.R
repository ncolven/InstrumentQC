#' For parsing Aurora QC data
#'
#' @param x Instrument name
#' @param MainFolder Path to InstrumentQC data folder
#' @return Writes csv file with SpectroFlo bead data to instrument Archive folder
#' @export

FHDailyQCParse <-function (MainFolder, x)
{
  Folder <- file.path(MainFolder, x)
  DailyQCFiles <- list.files(Folder, pattern = "DailyQC", full.names = TRUE)
  if (!length(DailyQCFiles) == 0) {
    if (length(DailyQCFiles) >= 1) {
      Parsed <- dplyr::bind_rows(map(.x = DailyQCFiles, .f = FH_DailyQCPrep_CS))
      Parsed <- dplyr::mutate(Parsed, dplyr::across(starts_with("Flag"),
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
        TheShiniestData <- dplyr::bind_rows(ShinyData, HistoricalData)
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
      ArchivedData <- dplyr::mutate(ArchivedData, dplyr::across(starts_with("Flag"),
                                                  ~as.logical(.)))
      if (!ncol(ArchivedData) == ncol(Parsed)) {
        Recent <- dplyr::setdiff(colnames(ArchivedData), colnames(Parsed))
        Previous <- dplyr::setdiff(colnames(Parsed), colnames(ArchivedData))
        if (length(Previous) == 0) {
          UpToHere <- nrow(Parsed)
          WorkAround <- dplyr::bind_rows(Parsed, ArchivedData)
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
    UpdatedData <- dplyr::arrange(UpdatedData, desc(DateTime))
    name <- paste0("ArchivedData", x, ".csv")
    StorageLocation <- file.path(TheArchive, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No DailyQCFiles files to update with in ", x)
  }
}
