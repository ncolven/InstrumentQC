#' Parses Discover QC reports and returns data on imaging detectors
#' @param SetupCSV data frame from Discover QC report
#' @export

ImgParse <- function (SetupCSV){
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

  SetupImg <- as.data.frame(t(Setup2[1:4,21:27]))
  colnames(SetupImg) <- SetupImg[1,]
  SetupImg <- SetupImg[-1,]
  SetupImg$DateTime <- DateTime
  SetupImg <- SetupImg %>% dplyr::relocate(DateTime)
  SetupImg <- SetupImg %>% tidyr::pivot_wider(id_cols = 'DateTime',
                                       names_from = Channel,
                                       values_from = colnames(SetupImg)[3:5])
  rownames(SetupImg) <- NULL
  return(SetupImg)
}
