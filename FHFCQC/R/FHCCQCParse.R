#' For parsing convention BD instrument UR QC files
#'
#' @param x Instrument name
#' @param MainFolder The data folder within InstrumentQC
#' @param Template The specific template for the given instrument
#' @param subsets The specific gate used for given beads
#' @param sample.name Keyword used, usually to be ignored
#' @return Writes a .csv file with updated QC data from supplied FCS files to the instrument's archive folder
#' @export
FHCCQCParse <- function (x, MainFolder, Template = NULL, subsets = NULL,
                         sample.name = "$DATE") {
  Folder <- file.path(MainFolder, x)
  FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
  if (!length(FCS_Files) == 0) {
    The_CS <- flowWorkspace::load_cytoset_from_fcs(files = FCS_Files,
                                    transformation = FALSE, truncate_max_range = FALSE)
    DateFormat <- keyword(The_CS[[1]])$`$DATE`
    if (sample.name == "$DATE" && DateFormat == "01-Jan-0001") {
      sample.name1 <- "$FIL"
    }
    else {
      sample.name1 <- sample.name
    }
    if (is.null(Template)) {
      Parsed <- dplyr::bind_rows(purrr::map(.x = The_CS, .f = QC_GainMonitoring,
                              sample.name = sample.name, stats = "median"))
    }
    else {
      Gating <- data.table::fread(Template)
      MyGatingSet <- flowWorkspace::GatingSet(The_CS)
      MyGatingTemplate <- openCyto::gatingTemplate(Gating)
      if (is.null(MyGatingSet)) {
        return(MyGatingSet)
      }
      openCyto::gt_gating(MyGatingTemplate, MyGatingSet)
      Parsed <- dplyr::bind_rows(purrr::map(.x = MyGatingSet, .f = Luciernaga::QC_GainMonitoring,
                              subsets = subsets, sample.name = sample.name1,
                              stats = "median"))
      if (DateFormat == "01-Jan-0001") {
        Parsed <- dplyr::mutate(Parsed, DATE = gsub("DailyQCDataSample_",
                                             "", SAMPLE))
        Parsed <- dplyr::mutate(Parsed, TIME = sub("^[^_]*_",
                                            "", DATE))
        Parsed <- dplyr::mutate(Parsed, DATE = sub("_.*", "",
                                            DATE))
        Parsed$DATE <- lubridate::ymd(Parsed$DATE)
        Parsed <- dplyr::mutate(Parsed, TIME = str_c(str_sub(TIME,
                                                      1, 2), ":", str_sub(TIME, 3, 4), ":", str_sub(TIME,
                                                                                                    5, 6)), TIME = lubridate::hms(TIME))
      }
    }
    Parsed <- dplyr::relocate(mutate(Parsed, DateTime = DATE + TIME),
                       DateTime, .before = DATE)
    Parsed <- dplyr::arrange(Parsed, desc(DateTime))
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
        NewData <- Parsed %>% dplyr::anti_join(ArchiveData,
                                        by = c("DATE", "TIME"))
        UpdatedData <- dplyr::bind_rows(NewData, ArchiveData)
        file.remove(ArchiveCSV)
      }
      else {
        stop("Two Holistic csv files in the archive folder!")
      }
    }
    else {
      UpdatedData <- Parsed
    }
    UpdatedData <- UpdatedData %>% dplyr::arrange(desc(DateTime))
    # file.remove(FCS_Files)
    name <- paste0("HolisticData", x, ".csv")
    StorageLocation <- file.path(ArchiveFolder, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No fcs files to update with in ", x)
  }
}
