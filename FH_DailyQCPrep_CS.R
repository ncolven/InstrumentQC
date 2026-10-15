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
