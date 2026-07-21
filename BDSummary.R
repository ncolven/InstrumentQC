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
    Data <- dplyr::filter(x, DateTime > WindowOfInterest)
    if (nrow(Data) == 0) {
      Data <- slice(x, 1)
    }
  }else {
    Data <- x
  }
  TheColumns <- Data %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
  TheColumns <- setdiff(TheColumns, "TIME")
  
  # MFIs
  {
  TheIntermediate <- TheColumns[!str_detect(TheColumns, "Gain")]
  TheIntermediate <- TheIntermediate[!str_detect(TheIntermediate, "rCV")]
  TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
  if (length(TheColumnNames) == 0){
    TheIntermediate <- TheIntermediate[str_detect(TheIntermediate, ".A")]
    TheColumnNames <- TheIntermediate[!str_detect(TheIntermediate, "_")]
  }
  
  if (Instrument == "Fortessa"){
    UVNames <- TheColumnNames[16:22]  
    VioletNames <- TheColumnNames[23:30]
    BlueNames <- TheColumnNames[3:7]
    YellowGreenNames <- TheColumnNames[8:12]
    YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[31]
    RedNames <- TheColumnNames[13:15]
  } else if(Instrument == "Symphony-1"){
    UVNames <- TheColumnNames[19:25]  
    VioletNames <- TheColumnNames[11:18]
    BlueNames <- TheColumnNames[3:7]
    YellowGreenNames <- TheColumnNames[26:30]
    RedNames <- TheColumnNames[8:10]
  } else if(Instrument == "Symphony-2"){
    UVNames <- c(TheColumnNames[9],TheColumnNames[18],TheColumnNames[25:28],TheColumnNames[10])
    VioletNames <- c(TheColumnNames[7],TheColumnNames[14],TheColumnNames[22],TheColumnNames[16:17],TheColumnNames[23:24], TheColumnNames[8])
    BlueNames <- c(TheColumnNames[3],TheColumnNames[19:20],TheColumnNames[4],TheColumnNames[21])
    YellowGreenNames <- c(TheColumnNames[11],TheColumnNames[15],TheColumnNames[29:30],TheColumnNames[12])
    RedNames <- c(TheColumnNames[5],TheColumnNames[13],TheColumnNames[6])
  } else if(Instrument == "Symphony-4"){
    UVNames <- TheColumnNames[20:27]  
    VioletNames <- TheColumnNames[12:19]
    BlueNames <- TheColumnNames[3:8]
    YellowGreenNames <- TheColumnNames[28:32]
    RedNames <- TheColumnNames[9:11]
  }
  
  VioletMFI <- Data[,VioletNames]
  BlueMFI <- Data[,BlueNames]
  YellowGreenMFI <- Data[,YellowGreenNames]
  RedMFI <- Data[,RedNames]
  
  VioletStatus <- MFICheck(VioletMFI)
  BlueStatus <- MFICheck(BlueMFI)
  YellowGreenStatus <- MFICheck(YellowGreenMFI)
  RedStatus <- MFICheck(RedMFI)
  if (!length(UVNames) == 0){
    UVMFI <- Data[,UVNames]
    UVStatus <- MFICheck(UVMFI)
    InstrumentMFI <- relocate(mutate(bind_cols(c(UVMFI,VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)), 
                                     DateTime = Data$DateTime), DateTime, .before = 1)
    MFIStatus <- relocate(mutate(bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)), 
                                 DateTime = Data$DateTime), DateTime, .before = 1)
  } else{
    InstrumentMFI <- relocate(mutate(bind_cols(c(VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)), 
                                     DateTime = Data$DateTime), DateTime, .before = 1)
    MFIStatus <- relocate(mutate(bind_cols(c(VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)), 
                                 DateTime = Data$DateTime), DateTime, .before = 1)
  }
  }
  # rCVs
  {
    TheIntermediate <- TheColumns[str_detect(TheColumns, "rCV")]
    TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
    if (length(TheColumnNames) == 0){
      TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, ".A")]
    }
    if (Instrument == "Fortessa"){
      UVNames <- TheColumnNames[16:22]  
      VioletNames <- TheColumnNames[23:30]
      BlueNames <- TheColumnNames[3:7]
      YellowGreenNames <- TheColumnNames[8:12]
      YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[31]
      RedNames <- TheColumnNames[13:15]
    } else if(Instrument == "Symphony-1"){
      UVNames <- TheColumnNames[19:25]  
      VioletNames <- TheColumnNames[11:18]
      BlueNames <- TheColumnNames[3:7]
      YellowGreenNames <- TheColumnNames[26:30]
      RedNames <- TheColumnNames[8:10]
    }else if(Instrument == "Symphony-2"){
      UVNames <- c(TheColumnNames[9],TheColumnNames[18],TheColumnNames[25:28],TheColumnNames[10])
      VioletNames <- c(TheColumnNames[7],TheColumnNames[14],TheColumnNames[22],TheColumnNames[16:17],TheColumnNames[23:24], TheColumnNames[8])
      BlueNames <- c(TheColumnNames[3],TheColumnNames[19:20],TheColumnNames[4],TheColumnNames[21])
      YellowGreenNames <- c(TheColumnNames[11],TheColumnNames[15],TheColumnNames[29:30],TheColumnNames[12])
      RedNames <- c(TheColumnNames[5],TheColumnNames[13],TheColumnNames[6])
    }else if(Instrument == "Symphony-4"){
      UVNames <- TheColumnNames[20:27]  
      VioletNames <- TheColumnNames[12:19]
      BlueNames <- TheColumnNames[3:8]
      YellowGreenNames <- TheColumnNames[28:32]
      RedNames <- TheColumnNames[9:11]
    }
    VioletRCV <- Data[,VioletNames]
    BlueRCV <- Data[,BlueNames]
    YellowGreenRCV <- Data[,YellowGreenNames]
    RedRCV <- Data[,RedNames]
    
    VioletStatus <- RCVCheck(VioletRCV)
    BlueStatus <- RCVCheck(BlueRCV)
    YellowGreenStatus <- RCVCheck(YellowGreenRCV)
    RedStatus <- RCVCheck(RedRCV)
    if (!length(UVNames) == 0){
      UVRCV <- Data[,UVNames]
      UVStatus <- RCVCheck(UVRCV)
      InstrumentRCV <- relocate(mutate(bind_cols(c(UVRCV,VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)), 
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      RCVStatus <- relocate(mutate(bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)), 
                                   DateTime = Data$DateTime), DateTime, .before = 1)
    } else{
      InstrumentRCV <- relocate(mutate(bind_cols(c(VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)), 
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      RCVStatus <- relocate(mutate(bind_cols(c(VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)), 
                                 DateTime = Data$DateTime), DateTime, .before = 1)
    }
  }
  InstrumentStatus <- bind_cols(InstrumentMFI, InstrumentRCV[,-1])
  return(InstrumentStatus)
}