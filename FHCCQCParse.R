FHCCQCParse <- function (x, MainFolder, Template = NULL, subsets = NULL,
                         sample.name = "$DATE") {
  Folder <- file.path(MainFolder, x)
  FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
  if (!length(FCS_Files) == 0) {
    The_CS <- load_cytoset_from_fcs(files = FCS_Files, 
                                    transformation = FALSE, truncate_max_range = FALSE)
    DateFormat <- keyword(The_CS[[1]])$`$DATE`
    if (sample.name == "$DATE" && DateFormat == "01-Jan-0001") {
      sample.name1 <- "$FIL"
    }
    else {
      sample.name1 <- sample.name
    }
    if (is.null(Template)) {
      Parsed <- bind_rows(map(.x = The_CS, .f = QC_GainMonitoring, 
                              sample.name = sample.name, stats = "median"))
    }
    else {
      Gating <- data.table::fread(Template)
      MyGatingSet <- GatingSet(The_CS)
      MyGatingTemplate <- gatingTemplate(Gating)
      #MyGatingSet <- Luciernaga:::GateCheck(gs = MyGatingSet, gatingtemplate = MyGatingTemplate, 
      #   subsets = subsets)
      if (is.null(MyGatingSet)) {
        return(MyGatingSet)
      }
      gt_gating(MyGatingTemplate, MyGatingSet)
      Parsed <- bind_rows(map(.x = MyGatingSet, .f = Luciernaga::QC_GainMonitoring, 
                              subsets = subsets, sample.name = sample.name1, 
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
    # file.remove(FCS_Files)
    name <- paste0("HolisticData", x, ".csv")
    StorageLocation <- file.path(ArchiveFolder, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No fcs files to update with in ", x)
  }
}