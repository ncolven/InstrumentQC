#' Parses Discover QC reports and returns data on lasers and fluorescent detectors
#' @param SetupCSV data frame from Discover QC report
#' @export

DiscoverParse <- function (SetupCSV){
  # Fluorescent detectors
  max_cols <- max(count.fields(SetupCSV, sep=","))
  col_names <- paste0("V", 1:max_cols)
  SetupCSV <- read.csv(SetupCSV, check.names = F, header = F, col.names = col_names, fill=T)
  StatusRow <- which(SetupCSV[,1]=="Status:")
  SetupStatus <- SetupCSV[StatusRow,2]
  if (str_detect(SetupStatus,"Cancelled")){
    return(NULL)
  }
  Setup2 <- as.data.frame(t(SetupCSV))
  colnames(Setup2) <- Setup2[1,]
  Setup2 <- Setup2[-1,]
  DateTime <- lubridate::mdy_hms(Setup2$`Setup Data time:`[1])
  SetupDetectors <- SetupCSV[41:126,2:19]
  colnames(SetupDetectors) <- SetupCSV[40,2:19]
  DetectorData <- SetupDetectors[,c(1,5,6,10,11)]
  DetectorData$DateTime<- DateTime
  HistoricData <- DetectorData
  DetectorData <- DetectorData %>% tidyr::pivot_wider(id_cols = DateTime,
                                               names_from = Name,
                                               values_from = c(' MFI-A',' rCV','Gain (dB)',IsOnTarget))
  ## Laser Delays
  LaserDelays <- Setup2[1:5,13:19]
  LaserDelays$DateTime <- DateTime
  LaserDelays <- LaserDelays %>% tidyr::pivot_wider(id_cols = "DateTime",
                                             names_from = Laser,
                                             values_from = colnames(LaserDelays)[2:7])
  LaserDelays <- LaserDelays[,-1]
  NewData <- dplyr::bind_cols(DetectorData, LaserDelays)
  return(NewData)
}
