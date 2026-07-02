BDSummary <- function (x, Instrument, detectorType = "-A"){
  MFICheck <- function(x){ 
    sapply(if (x <=7999){
    x <- "Red"
  }else if (x >=12001){
    x <- "Orange"
  }else{
    x <- "Green"
    })
  }
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
  TheColumns <- Data %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
  TheColumns <- setdiff(TheColumns, "TIME")
  
  # MFIs
  TheIntermediate <- TheColumns[!str_detect(TheColumns, "Gain")]
  TheIntermediate <- TheIntermediate[!str_detect(TheIntermediate, "rCV")]
  TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
  if (Instrument == "Fortessa"){
    UVNames <- TheColumnNames[16:22]  
    VioletNames <- TheColumnNames[23:30]
    BlueNames <- TheColumnNames[3:7]
    YellowGreenNames <- TheColumnNames[8:12]
    YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[31]
    RedNames <- TheColumnNames[13:15]
    
    UVMFI <- Data[,UVNames]
    VioletMFI <- Data[,VioletNames]
    BlueMFI <- Data[,BlueNames]
    YellowGreenMFI <- Data[,YellowGreenNames]
    RedMFI <- Data[,RedNames]
  }
  UVStatus <- lapply(UVMFI, MFICheck)
}