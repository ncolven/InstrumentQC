BDSummary <- function (x, Instrument, detectorType = "-A"){
  MFICheck <- function(x){
    data = x
    for (j in 1:ncol(x)){
      for (i in 1:nrow(x)){
        if (x[i,j] <=7999){
          data[i,j] <- "Red"
        }else if (x[i,j] >12000){
          data[i,j] <- "Orange"
        }else{
          data[i,j] <- "Green"
        }
      }
    }
    return(data)
  }
  RCVCheck <- function(x){
    data <- x
    for (i in 1:nrow(x)){
      for (j in 1:ncol(x)){
        if (x[i,j] >=15){
          data[i,j] <- "Red"
        } else {
          data[i,j] <- "Green"
        }
      }
    }
    return(data)
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
  {
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
  UVStatus <- MFICheck(UVMFI)
  VioletStatus <- MFICheck(VioletMFI)
  BlueStatus <- MFICheck(BlueMFI)
  YellowGreenStatus <- MFICheck(YellowGreenMFI)
  RedStatus <- MFICheck(RedMFI)
  
  InstrumentMFI <- relocate(mutate(bind_cols(c(UVMFI,VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)), 
                  DateTime = Data$DateTime), DateTime, .before = 1)
  MFIStatus <- relocate(mutate(bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)), 
                      DateTime = Data$DateTime), DateTime, .before = 1)
  }
  # rCVs
  {
    TheIntermediate <- TheColumns[str_detect(TheColumns, "rCV")]
    TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
    
    if (Instrument == "Fortessa"){
      UVNames <- TheColumnNames[16:22]  
      VioletNames <- TheColumnNames[23:30]
      BlueNames <- TheColumnNames[3:7]
      YellowGreenNames <- TheColumnNames[8:12]
      YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[31]
      RedNames <- TheColumnNames[13:15]
      
      UVRCV <- Data[,UVNames]
      VioletRCV <- Data[,VioletNames]
      BlueRCV <- Data[,BlueNames]
      YellowGreenRCV <- Data[,YellowGreenNames]
      RedRCV <- Data[,RedNames]
    }
    UVStatus <- RCVCheck(UVRCV)
    VioletStatus <- RCVCheck(VioletRCV)
    BlueStatus <- RCVCheck(BlueRCV)
    YellowGreenStatus <- RCVCheck(YellowGreenRCV)
    RedStatus <- RCVCheck(RedRCV)
    
    RCVStatus <- bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus))
  }
  InstrumentStatus <- bind_cols(c(MFIStatus, RCVStatus))
  #Use tidyr::pivot_longer to convert from wide to long data
  return(InstrumentStatus)
}