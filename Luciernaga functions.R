Luciernaga:::DailyQCParse
function (MainFolder, x) 
{
  Folder <- file.path(MainFolder, x)
  DailyQCFiles <- list.files(Folder, pattern = "DailyQC", full.names = TRUE)
  if (!length(DailyQCFiles) == 0) {
    if (length(DailyQCFiles) >= 1) {
      Parsed <- bind_rows(map(.x = DailyQCFiles, .f = Luciernaga:::QC_FilePrep_DailyQC))
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
    }
    else {
      write.csv(ShinyData, HistoricalPath, row.names = FALSE)
    }
    TheArchive <- file.path(Folder, "Archive")
    ArchivedDataFile <- list.files(TheArchive, pattern = "Archived", 
                                   full.names = TRUE)
    if (!length(ArchivedDataFile) == 0) {
      if (length(ArchivedDataFile) == 1) {
        ArchivedData <- read.csv(ArchivedDataFile[1], 
                                 check.names = FALSE)
      }
      else {
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
        }
        else {
          stop("Mismatched Columns, newer data fewer columns than old data")
        }
      }
      else {
        NewData <- generics::setdiff(Parsed, ArchivedData)
        UpdatedData <- rbind(NewData, ArchivedData)
      }
      file.remove(ArchivedDataFile)
    }
    else {
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
<bytecode: 0x9b8daac48>
  <environment: namespace:Luciernaga>
############################
############################
############################
############################
Luciernaga:::QCBeadParse
function (x, MainFolder) 
{
  Folder <- file.path(MainFolder, x)
  FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
  if (!length(FCS_Files) == 0) {
    QCBeads <- FCS_Files[grep("Before|After", FCS_Files)]
    BeforeAfter_CS <- load_cytoset_from_fcs(files = QCBeads, 
                                            transformation = FALSE, truncate_max_range = FALSE)
    BeforeAfter <- map(.x = BeforeAfter_CS, .f = Luciernaga:::QC_GainMonitoring, 
                       sample.name = "TUBENAME", stats = "median") %>% bind_rows()
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
<bytecode: 0xaa13e6cf0>
  <environment: namespace:Luciernaga>
############################
############################
############################
############################
Luciernaga:::QC_GainMonitoring
function (x, sample.name, stats, subsets = NULL, inverse.transform = FALSE) 
{
  if (class(x) == "GatingHierarchy") {
    SayTheName <- sampleNames(x)
    cs <- gs_pop_get_data(x, subsets, inverse.transform = inverse.transform)
    if (nrow(cs[[1]]) != 0) {
      x <- cs[[1]]
    }
    else {
      message("No cells retained in ", SayTheName, ", passing original .fcs file")
      cs <- gs_pop_get_data(x, "root", inverse.transform = inverse.transform)
      if (nrow(cs[[1]]) != 0) {
        x <- cs[[1]]
      }
      else {
        message("No cells present in ", SayTheName)
      }
    }
  }
  Guts <- QC_Retrieval(x = x, sample.name = sample.name)
  Data <- data.frame(exprs(x), check.names = FALSE)
  These <- colnames(Data)
  These <- These[These != "Time"]
  TheRCVs <- bind_cols(map(.x = These, .f = InternalRCV, data = Data))
  TheRCVs <- round(TheRCVs * 100, 2)
  colnames(TheRCVs) <- paste0(colnames(TheRCVs), "-% rCV")
  Data <- AveragedSignature(Data, stats)
  Data <- select(Data, -Time)
  Bound <- cbind(Guts, TheRCVs, Data)
  Bound[["SAMPLE"]] <- NameCleanUp(Bound[["SAMPLE"]], removestrings = ".fcs")
  Bound[["SAMPLE"]] <- NameCleanUp(Bound[["SAMPLE"]], removestrings = ".fcs")
  if (str_detect(Bound[["SAMPLE"]], "efore")) {
    Bound <- relocate(mutate(Bound, Timepoint = "Before"), 
                      Timepoint, .after = TIME)
  } else if (str_detect(Bound[["SAMPLE"]], "fter")) {
    Bound <- relocate(mutate(Bound, Timepoint = "After"), 
                      Timepoint, .after = TIME)
  } else {
    Bound <- relocate(mutate(Bound, Timepoint = "Unknown"), 
                      Timepoint, .after = TIME)
  }
  return(Bound)
}
<bytecode: 0x9b4db1930>
  <environment: namespace:Luciernaga>
############################
############################
############################
############################
Luciernaga::QC_Retrieval
function (x, sample.name) 
{
  KeywordsList <- keyword(x)
  KeywordsDF <- data.frame(KeywordsList, check.names = FALSE)
  TheColumnNames <- colnames(KeywordsDF)
  SAMPLE <- keyword(x)[[sample.name]]
  DATE <- keyword(x)$`$DATE`
  DATE <- dmy(DATE)
  TIME <- keyword(x)$`$BTIM`
  TIME <- hms(TIME)
  CYT <- keyword(x)$`$CYT`
  if (is.null(CYT)) {
    CYTN <- "Unknown"
  }
  CYTSN <- keyword(x)$`$CYTSN`
  if (is.null(CYTSN)) {
    CYTSN <- keyword(x)$CYTNUM
  }
  if (is.null(CYTSN)) {
    CYTSN <- "Unknown"
  }
  OP <- keyword(x)$`$OP`
  if (is.null(OP)) {
    OP <- "Unknown"
  }
  PN_Names1 <- TheColumnNames[grepl("^\\$P[0-9]{1}N$", TheColumnNames)]
  PN_Names2 <- TheColumnNames[grepl("^\\$P[0-9]{2}N$", TheColumnNames)]
  PN_Names3 <- TheColumnNames[grepl("^\\$P[0-9]{3}N$", TheColumnNames)]
  PV_Gains1 <- TheColumnNames[grepl("^\\$P[0-9]{1}V$", TheColumnNames)]
  PV_Gains2 <- TheColumnNames[grepl("^\\$P[0-9]{2}V$", TheColumnNames)]
  PV_Gains3 <- TheColumnNames[grepl("^\\$P[0-9]{3}V$", TheColumnNames)]
  if (length(PN_Names3) > 0) {
    PN_Names <- c(PN_Names1, PN_Names2, PN_Names3)
  }else {
    PN_Names <- c(PN_Names1, PN_Names2)
  }
  PN_Names <- PN_Names[-1]
  if (length(PV_Gains3) > 0) {
    PV_Gains <- c(PV_Gains1, PV_Gains2, PV_Gains3)
  }else {
    PV_Gains <- c(PV_Gains1, PV_Gains2)
  }
  ParameterRows <- map2(.x = PN_Names, .y = PV_Gains, .f = Luciernaga:::RetrievalMerge, 
                        TheData = KeywordsDF) %>% bind_cols()
  Laser_Name <- TheColumnNames[grepl("^\\LASER[0-9]{1}NAME$", 
                                     TheColumnNames)]
  Laser_Delay <- TheColumnNames[grepl("^\\LASER[0-9]{1}DELAY$", 
                                      TheColumnNames)]
  Laser_ASF <- TheColumnNames[grepl("^\\LASER[0-9]{1}ASF$", 
                                    TheColumnNames)]
  LaserDelayRows <- map2(.x = Laser_Name, .y = Laser_Delay, 
                         .f = Luciernaga:::RetrievalMerge, TheData = KeywordsDF) %>% 
    bind_cols()
  colnames(LaserDelayRows) <- paste0(colnames(LaserDelayRows), 
                                     "_LaserDelay")
  LaserASFRows <- map2(.x = Laser_Name, .y = Laser_ASF, .f = Luciernaga:::RetrievalMerge, 
                       TheData = KeywordsDF) %>% bind_cols()
  colnames(LaserASFRows) <- paste0(colnames(LaserASFRows), 
                                   "_AreaScalingFactor")
  ParameterRows <- ParameterRows %>% mutate(across(everything(), 
                                                   as.numeric))
  colnames(ParameterRows) <- paste0(colnames(ParameterRows), 
                                    "_Gain")
  LaserDelayRows <- LaserDelayRows %>% mutate(across(everything(), 
                                                     as.numeric))
  if (ncol(LaserDelayRows) == 0) {
    LaserDelayRows <- NULL
  }
  LaserASFRows <- LaserASFRows %>% mutate(across(everything(), 
                                                 as.numeric))
  if (ncol(LaserASFRows) == 0) {
    LaserASFRows <- NULL
  }
  if (is.null(SAMPLE)) {
    stop("sample.name keyword not recognized")
  }
  RecoveredQC <- cbind(SAMPLE, DATE, TIME, CYT, CYTSN, OP, 
                       ParameterRows)
  if (!is.null(LaserDelayRows)) {
    RecoveredQC <- cbind(RecoveredQC, LaserDelayRows)
  }
  if (!is.null(LaserASFRows)) {
    RecoveredQC <- cbind(RecoveredQC, LaserASFRows)
  }
  return(RecoveredQC)
}
<bytecode: 0x9b4f637e0>
  <environment: namespace:Luciernaga>
############################
############################
############################
############################  
Luciernaga:::HolisticQCParse
function (x, MainFolder, Template = NULL, subsets = NULL, FuckIt = FALSE, 
          sample.name = "$DATE") 
{
  Folder <- file.path(MainFolder, x)
  FCS_Files <- list.files(Folder, pattern = "fcs", full.names = TRUE)
  if (!length(FCS_Files) == 0) {
    if (FuckIt == TRUE) {
      Screen <- CytosetScreen(files = FCS_Files)
      MainList <- which.max(sapply(Screen, length))
      Screen <- Screen[MainList][[1]]
      The_CS <- load_cytoset_from_fcs(files = Screen, transformation = FALSE, 
                                      truncate_max_range = FALSE)
    }
    else {
      The_CS <- load_cytoset_from_fcs(files = FCS_Files, 
                                      transformation = FALSE, truncate_max_range = FALSE)
    }
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
      MyGatingSet <- Luciernaga:::GateCheck(gs = MyGatingSet, gatingtemplate = MyGatingTemplate, 
                               subsets = subsets)
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
    file.remove(FCS_Files)
    name <- paste0("HolisticData", x, ".csv")
    StorageLocation <- file.path(ArchiveFolder, name)
    write.csv(UpdatedData, StorageLocation, row.names = FALSE)
  }
  else {
    message("No fcs files to update with in ", x)
  }
}
<bytecode: 0x7faeca3f2e20>
  <environment: namespace:Luciernaga>
  
  
  Luciernaga:::GateCheck
function (gs, gatingtemplate, subsets = NULL) 
{
  Nodes <- gatingtemplate@nodes
  These <- Nodes
  if (!is.null(subsets)) {
    TheMatch <- paste0("/", subsets)
    Matching <- min(which(These %in% TheMatch))
    These <- These[1:Matching]
    if (length(These) == 1) {
      CheckThis <- These
    }
    else {
      CheckThis <- paste(These, collapse = "|")
    }
  }
  else {
    These <- These[-1]
    CheckThis <- paste(These, collapse = "|")
  }
  ThisFluor <- gatingtemplate@edgeData@data[[CheckThis]]$gtMethod@dims
  TheIndex <- any(colnames(gs) %in% ThisFluor)
  Present <- gs[TheIndex]
  if (length(Present) == 0) {
    gs <- NULL
  }
  else {
    gs <- Present
  }
  return(gs)
}

Luciernaga:::QC_Plots
function (x, FailedFlag, MeasurementType = NULL, Metadata = NULL, 
          plotType = "individual", returntype, path, filename, thecolumns = 1, 
          therows = 3, width = 7, height = 9, strict = FALSE, YAxisLabel = NULL, 
          RepairVisits = NULL) 
  
  #UVPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=UVGains,
    #                      plotType = "individual", returntype = "plots", YAxisLabel = "Gain",
    #                      RepairVisits=RepairAria)
{
  if (any(str_detect(colnames(x), "DateTime"))) {
    TheDateTime <- x %>% relocate(DateTime, .before = 1)
    TheDateTime[["DateTime"]] <- lubridate::ymd_hms(x[["DateTime"]])
  } else {
    TheDateTime <- x %>% mutate(DateTime = ymd(DATE) + hms(TIME)) %>% 
      relocate(DateTime, .before = 1)
  }
  if (!is.null(Metadata)) {
    TheDateTime <- TheDateTime %>% select(DateTime, contains(Metadata))
  } else {
    TheDateTime <- TheDateTime %>% select(DateTime)
  }
  if (!is.null(RepairVisits)) {
    if (!is.data.frame(RepairVisits)) {
      Visit <- read.csv(RepairVisits, check.names = FALSE)
    } else {
      Visit <- RepairVisits
    }
    Earliest <- TheDateTime %>% arrange(DateTime) %>% slice(1) %>% 
      pull(DateTime)
    Earliest <- Earliest - days(1)
    Earliest
    Visit$date <- lubridate::mdy(Visit$date)
    TimeWindow <- Visit %>% filter(date > Earliest)
    TheEngineerVisits <- TimeWindow %>% pull(date)
  } else {
    TheEngineerVisits <- NULL
  }
  if (!is.null(MeasurementType)) {
    x <- x %>% select(contains(MeasurementType))
  }
  x <- x %>% mutate(across(starts_with("Flag"), ~as.logical(.)))
  x <- x %>% select(where(~!is.character(.)))
  if (strict == TRUE) {
    Regular <- x %>% select(all_of(MeasurementType))
    EquivalentFlags <- paste0("Flag-", colnames(Regular))
    if (!plotType == "comparison" && FailedFlag == TRUE) {
      Flagged <- x %>% select(all_of(EquivalentFlags))
      ReorderedData <- cbind(Regular, Flagged)
    } else {
      ReorderedData <- Regular
    }
  } else {
    Regular <- x %>% select(-starts_with("Flag"))
    Flagged <- x %>% select(starts_with("Flag"))
    ReorderedData <- cbind(Regular, Flagged)
  }
  FlaggedStart <- length(Regular) + 1
  FlaggedEnd <- length(ReorderedData)
  RegularStop <- length(Regular)
  DFNames <- colnames(ReorderedData[seq_len(RegularStop)])
  TheData <- cbind(TheDateTime, ReorderedData)
  if (is.null(TheEngineerVisits)) {
    Plots <- map(.x = DFNames, .f = Luciernaga:::LevyJennings, FailedFlag = FailedFlag, 
                 xValue = "DateTime", TheData = TheData, Metadata = Metadata, 
                 plotType = plotType, YAxisLabel = YAxisLabel, EngineerVisits = NULL)
  } else if (length(TheEngineerVisits) > 0) {
    Plots <- map(.x = DFNames, .f = Luciernaga:::LevyJennings, FailedFlag = FailedFlag, 
                 xValue = "DateTime", TheData = TheData, Metadata = Metadata, 
                 plotType = plotType, YAxisLabel = YAxisLabel, EngineerVisits = TheEngineerVisits)
  } else {
    Plots <- map(.x = DFNames, .f = Luciernaga:::LevyJennings, FailedFlag = FailedFlag, 
                 xValue = "DateTime", TheData = TheData, Metadata = Metadata, 
                 plotType = plotType, YAxisLabel = YAxisLabel, EngineerVisits = NULL)
  }
  if (returntype == "pdf") {
    if (is.null(path)) {
      path <- getwd()
    }
    AssembledPlots <- Utility_Patchwork(x = Plots, filename = filename, 
                                        outfolder = path, returntype = "pdf", thecolumns = thecolumns, 
                                        therows = therows, width = width, height = height)
  }
  if (returntype == "patchwork") {
    AssembledPlots <- Utility_Patchwork(x = Plots, filename = filename, 
                                        outfolder = path, returntype = "patchwork", thecolumns = thecolumns, 
                                        therows = therows, width = width, height = height)
  }
  if (returntype == "plots") {
    AssembledPlots <- Plots
  }
  return(AssembledPlots)
}
<bytecode: 0x7faa232671f8>
  <environment: namespace:Luciernaga>

  
  Luciernaga:::LevyJennings
function (x, FailedFlag, xValue, TheData, Metadata, plotType, 
          YAxisLabel, EngineerVisits = NULL) 
{
  yValue <- x
  if (FailedFlag == TRUE) {
    FlagValue <- paste0("Flag-", yValue)
    FlagColumn <- TheData %>% select(starts_with(FlagValue)) %>% 
      colnames(.)
    if (length(FlagColumn) > 1) {
      NewFlagColumn <- str_detect(FlagColumn, paste0("^", 
                                                     FlagValue, "$"))
      FlagColumn <- FlagColumn[which(NewFlagColumn)]
    }
    else (FlagColumn <- FlagColumn)
  }
  if (str_detect(yValue, "^UV\\d{1,2}-[A-Za-z]+(_Gain)?$")) {
    mycolor <- "purple"
  }
  else if (str_detect(yValue, "^UV\\d{1,3}[-_]")) {
    mycolor <- "purple"
  }
  else if (str_detect(yValue, "^V\\d{1,3}[-_]")) {
    mycolor <- "violet"
  }
  else if (str_detect(yValue, "^V\\d{1,2}-[A-Za-z]+(_Gain)?$")) {
    mycolor <- "violet"
  }
  else if (str_detect(yValue, "^B\\d{1,3}[-_]")) {
    mycolor <- "blue"
  }
  else if (str_detect(yValue, "^B\\d{1,2}-[A-Za-z]+(_Gain)?$")) {
    mycolor <- "blue"
  }
  else if (str_detect(yValue, "^Y\\d{1,3}[-_]")) {
    mycolor <- "darkgreen"
  }
  else if (str_detect(yValue, "^YG\\d{1,3}[-_]")) {
    mycolor <- "darkgreen"
  }
  else if (str_detect(yValue, "^YG\\d{1,2}-[A-Za-z]+(_Gain)?$")) {
    mycolor <- "darkgreen"
  }
  else if (str_detect(yValue, "^R\\d{1,3}[-_]")) {
    mycolor <- "darkred"
  }
  else if (str_detect(yValue, "^R\\d{1,2}-[A-Za-z]+(_Gain)?$")) {
    mycolor <- "darkred"
  }
  else if (str_detect(yValue, "^Change_UV\\d{1,2}(-[A-Za-z% ]+)?$")) {
    mycolor <- "purple"
  }
  else if (str_detect(yValue, "^Change_V\\d{1,2}(-[A-Za-z% ]+)?$")) {
    mycolor <- "violet"
  }
  else if (str_detect(yValue, "^Change_B\\d{1,2}(-[A-Za-z% ]+)?$")) {
    mycolor <- "blue"
  }
  else if (str_detect(yValue, "^Change_YG\\d{1,2}(-[A-Za-z% ]+)?$")) {
    mycolor <- "darkgreen"
  }
  else if (str_detect(yValue, "^Change_R\\d{1,2}(-[A-Za-z% ]+)?$")) {
    mycolor <- "darkred"
  }
  else {
    mycolor <- "black"
  }
  if (plotType == "individual") {
    if (FailedFlag == TRUE) {
      if (any(grepl("FALSE", TheData[[FlagColumn]]))) {
        shape_qc <- c(`FALSE` = 21, `TRUE` = 22)
        fill_qc <- c(`FALSE` = mycolor, `TRUE` = "red")
        size_qc <- c(`FALSE` = 1, `TRUE` = 3)
      }
      else {
        shape_qc <- c(False = 21, True = 22)
        fill_qc <- c(False = mycolor, True = "red")
        size_qc <- c(False = 1, True = 3)
      }
      Plot <- ggplot(TheData, aes(x = .data[[xValue]], 
                                  y = .data[[yValue]])) + geom_line(color = mycolor, 
                                                                    linewidth = 1) + geom_point(aes(shape = .data[[FlagColumn]], 
                                                                                                    size = .data[[FlagColumn]], fill = .data[[FlagColumn]])) + 
        scale_shape_manual(values = shape_qc) + scale_fill_manual(values = fill_qc) + 
        scale_size_manual(values = size_qc) + labs(title = yValue, 
                                                   x = NULL, y = YAxisLabel) + theme_bw() + theme(legend.position = "none")
    }
    else {
      Plot <- ggplot(TheData, aes(x = .data[[xValue]], 
                                  y = .data[[yValue]], color = mycolor)) + geom_line(color = mycolor) + 
        geom_point(color = mycolor) + labs(title = yValue, 
                                           x = NULL, y = YAxisLabel) + theme_bw() + theme(legend.position = "none")
    }
  }
  if (plotType == "comparison") {
    if (mycolor != "black") {
      VariantColor <- c("black", mycolor)
    }
    else {
      VariantColor <- c("gray", mycolor)
    }
    Plot <- ggplot(TheData, aes(x = .data[[xValue]], y = .data[[yValue]], 
                                group = .data[[Metadata]], color = .data[[Metadata]])) + 
      geom_line(aes(color = .data[[Metadata]])) + geom_point(aes(color = .data[[Metadata]])) + 
      scale_color_manual(values = VariantColor) + labs(title = yValue, 
                                                       x = NULL, y = YAxisLabel) + theme(legend.position = "none") + 
      theme_bw()
  }
  if (is.null(EngineerVisits)) {
    Plot1 <- Plot
  }
  else {
    Plot1 <- Plot + geom_vline(xintercept = as.POSIXct(EngineerVisits), 
                               color = "red", linetype = "dashed")
  }
  return(Plot1)
}  
  
Luciernaga:::CurrentData
function (x, MainFolder, type) 
{
  ArchiveLocation <- file.path(MainFolder, x, "Archive")
  if (type == "MFI") {
    BeadData <- list.files(ArchiveLocation, pattern = "Bead", 
                           full.names = TRUE)
    Data <- read.csv(BeadData, check.names = FALSE)
    Data$DateTime <- lubridate::ymd_hms(Data$DateTime)
    Data$DATE <- lubridate::ymd(Data$DATE)
    Data$TIME <- lubridate::hms(Data$TIME)
  }
  if (type == "Gain") {
    ArchiveData <- list.files(ArchiveLocation, pattern = "Archived", 
                              full.names = TRUE)
    Data <- read.csv(ArchiveData, check.names = FALSE)
    if (any(str_detect(Data$DateTime, ":.*:"))) {
      Data$DateTime <- lubridate::ymd_hms(Data$DateTime)
    }
    else {
      Data$DateTime <- lubridate::mdy_hm(Data$DateTime)
    }
  }
  if (type == "Both") {
    BothData <- list.files(ArchiveLocation, pattern = "Holistic", 
                           full.names = TRUE)
    Data <- read.csv(BothData, check.names = FALSE)
    Data$DateTime <- lubridate::ymd_hms(Data$DateTime)
    Data$DATE <- lubridate::ymd(Data$DATE)
    Data$TIME <- lubridate::hms(Data$TIME)
  }
  Data <- Data %>% arrange(desc(DateTime))
  return(Data)
}
<bytecode: 0x7faa2182abc8>
  <environment: namespace:Luciernaga>  
  
  
  Luciernaga:::VisualQCSummary
function (x, detectorType = "-A") 
{
  WindowOfInterest <- Sys.time() - weeks(1)
  if (nrow(x) > 1) {
    Data <- filter(x, DateTime > WindowOfInterest)
    if (nrow(Data) == 0) {
      Data <- slice(x, 1)
    }
  }
  else {
    Data <- x
  }
  if (any(stringr::str_detect(colnames(Data), "Flag"))) {
    Flags <- select(Data, starts_with("Flag"))
    colnames(Flags) <- gsub("Flag-", "", colnames(Flags))
    Gains <- select(Flags, contains("Gain"))
    TheGains <- colnames(Gains)
    rCV <- select(Flags, contains("rCV"))
    TherCV <- colnames(rCV)
    colnames(Gains) <- gsub("-Gain", "", fixed = TRUE, colnames(Gains))
    colnames(Gains) <- gsub("_Gain", "", fixed = TRUE, colnames(Gains))
    colnames(rCV) <- gsub("-% rCV", "", fixed = TRUE, colnames(rCV))
  }
  else {
    Gains <- select(Data, contains("Gain"))
    TheGains <- colnames(Gains)
    rCV <- select(Data, contains("rCV"))
    TherCV <- colnames(rCV)
    colnames(Gains) <- gsub("-Gain", "", fixed = TRUE, colnames(Gains))
    colnames(Gains) <- gsub("_Gain", "", fixed = TRUE, colnames(Gains))
    Gains[] <- "NA"
    colnames(rCV) <- gsub("-% rCV", "", fixed = TRUE, colnames(rCV))
    rCV[] <- "NA"
  }
  TheGainData <- select(Data, all_of(c("DateTime", TheGains)))
  colnames(TheGainData) <- gsub("_Gain", "", fixed = TRUE, 
                                colnames(TheGainData))
  colnames(TheGainData) <- gsub("-Gain", "", fixed = TRUE, 
                                colnames(TheGainData))
  TheGainData <- pivot_longer(TheGainData, !DateTime, names_to = "Detector", 
                              values_to = "Gain")
  Gains <- relocate(mutate(Gains, DateTime = Data$DateTime), 
                    DateTime, .before = 1)
  Gains <- pivot_longer(Gains, !DateTime, names_to = "Detector", 
                        values_to = "Gain_Logical")
  TherCVData <- select(Data, all_of(c("DateTime", TherCV)))
  colnames(TherCVData) <- gsub("-% rCV", "", fixed = TRUE, 
                               colnames(TherCVData))
  TherCVData <- pivot_longer(TherCVData, !DateTime, names_to = "Detector", 
                             values_to = "rCV")
  rCV <- relocate(mutate(rCV, DateTime = Data$DateTime), DateTime, 
                  .before = 1)
  rCV <- pivot_longer(rCV, !DateTime, names_to = "Detector", 
                      values_to = "rCV_Logical")
  Tidy <- left_join(left_join(left_join(TheGainData, Gains, 
                                        by = c("Detector", "DateTime")), TherCVData, by = c("Detector", 
                                                                                            "DateTime")), rCV, by = c("Detector", "DateTime"))
  TheDetectors <- unique(pull(Tidy, Detector))
  Summary <- bind_rows(map(.x = TheDetectors, .f = Luciernaga:::QCSummaryCheck, 
                           data = Tidy))
  return(Summary)
}
<bytecode: 0x7faa211ab330>
  <environment: namespace:Luciernaga>
###############################################    
############################################### 
###############################################  
###############################################  
  Luciernaga:::QCSummaryCheck
function (x, data) 
{
  Subset <- dplyr::filter(data, Detector %in% x)
  GainValue <- pull(slice(Subset, 1), Gain)
  if (any(Subset$Gain_Logical == TRUE)) {
    Followup <- pull(slice(Subset, 1), Gain_Logical)
    if (Followup == TRUE) {
      GainStatus <- "Red"
    }
    else {
      GainStatus <- "Yellow"
    }
  }
  else if (any(Subset$Gain_Logical == FALSE)) {
    GainStatus <- "Green"
  }
  else {
    GainStatus <- "Gray"
  }
  rCVValue <- round(pull(slice(Subset, 1), rCV), 2)
  if (any(Subset$rCV_Logical == TRUE)) {
    Followup <- pull(slice(Subset, 1), rCV_Logical)
    if (Followup == TRUE) {
      rCVStatus <- "Red"
    }
    else {
      rCVStatus <- "Yellow"
    }
  }
  else if (any(Subset$rCV_Logical == FALSE)) {
    rCVStatus <- "Green"
  }
  else {
    rCVStatus <- "Gray"
  }
  Summary <- data.frame(Detector = x, GainValue = GainValue, 
                        Gain = GainStatus, rCVValue = rCVValue, rCV = rCVStatus)
  return(Summary)
}
<bytecode: 0x7faa0bb77b38>
  <environment: namespace:Luciernaga>  
  
  
  
  Luciernaga:::SmallTable
function (data) 
{
  table <- data %>% gt() %>% data_color(columns = c(Gain, rCV), 
                                        fn = function(x) {
                                          dplyr::case_when(x == "Green" ~ "#0B6623", x == "Orange" ~ 
                                                             "#FF6E00", x == "Yellow" ~ "#BA8E23", x == "Red" ~ 
                                                             "#C80815", x == "Gray" ~ "#D3D3D3", TRUE ~ NA_character_)
                                        })
  Substituted <- sub_values(sub_values(sub_values(sub_values(sub_values(table, 
                                                                        values = c("Green"), replacement = "Pass"), values = c("Orange"), 
                                                             replacement = "Warning"), values = c("Yellow"), replacement = "Caution"), 
                                       values = c("Red"), replacement = "Fail"), values = c("Gray"), 
                            replacement = "")
  Bolded <- cols_align(opt_table_font(Substituted, font = "Montserrat"), 
                       align = "center")
  Final <- cols_label(cols_label(tab_spanner(tab_spanner(Bolded, 
                                                         label = "Gain ", columns = c(GainValue, Gain)), label = "%RCV ", 
                                             columns = c(rCVValue, rCV)), GainValue = "Value", Gain = "Status"), 
                      rCVValue = "Value", rCV = "Status")
  return(Final)
}
<bytecode: 0x7faa21175448>
  <environment: namespace:Luciernaga>  
#################
#################
#################
Utility_Patchwork
function (x, filename, outfolder, thecolumns = 2, therows = 3, 
          width = 7, height = 9, returntype = "pdf", NotListofList = TRUE, 
          patches = FALSE) 
{
  if (NotListofList == TRUE) {
    theList <- x
    theList <- Filter(Negate(is.null), theList)
    theListLength <- length(theList)
    theoreticalitems <- therows * thecolumns
    sublists <- split_list(theList, theoreticalitems)
  }
  else {
    sublists <- x
    if (patches == TRUE) {
      sublists <- flatten(sublists)
    }
  }
  if (returntype == "pdf") {
    MergedName <- paste(outfolder, filename, sep = "/")
    pdf(file = paste(MergedName, ".pdf", sep = "", collapse = NULL), 
        width = width, height = height)
    p <- map(sublists, .f = sublist_plots, thecolumns = thecolumns, 
             therows = therows)
    print(p)
    dev.off()
  }
  else if (returntype == "patchwork") {
    p <- map(sublists, .f = sublist_plots, thecolumns = thecolumns, 
             therows = therows)
    return(p)
  }
}
<bytecode: 0x7f7aa52362b0>
  <environment: namespace:Luciernaga>
  