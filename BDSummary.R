BDSummary <- function (x, Instrument){
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
  }else if(Instrument == "Symphony-2"){
    UVNames <- c(TheColumnNames[9],TheColumnNames[18],TheColumnNames[25:28],TheColumnNames[10])
    VioletNames <- c(TheColumnNames[7],TheColumnNames[14],TheColumnNames[22],TheColumnNames[16:17],TheColumnNames[23:24], TheColumnNames[8])
    BlueNames <- c(TheColumnNames[3],TheColumnNames[19:20],TheColumnNames[4],TheColumnNames[21])
    YellowGreenNames <- c(TheColumnNames[11],TheColumnNames[15],TheColumnNames[29:30],TheColumnNames[12])
    RedNames <- c(TheColumnNames[5],TheColumnNames[13],TheColumnNames[6])
  } else if(Instrument == "Symphony-3"){
    UVNames <- TheColumnNames[32:41]  
    VioletNames <- TheColumnNames[18:31]
    BlueNames <- TheColumnNames[3:11]
    YellowGreenNames <- TheColumnNames[42:50]
    RedNames <- TheColumnNames[12:17]
  }else if(Instrument == "Symphony-4"){
    UVNames <- TheColumnNames[20:27]  
    VioletNames <- TheColumnNames[12:19]
    BlueNames <- TheColumnNames[3:8]
    YellowGreenNames <- TheColumnNames[28:32]
    RedNames <- TheColumnNames[9:11]
  }else if(Instrument == "Symphony-5"){
    UVNames <- TheColumnNames[3:12]  
    VioletNames <- TheColumnNames[13:26]
    BlueNames <- TheColumnNames[27:35]
    YellowGreenNames <- TheColumnNames[36:45]
    RedNames <- TheColumnNames[46:51]
  }else if(Instrument == "Symphony A3"){
    UVNames <- TheColumnNames[3:8]  
    VioletNames <- TheColumnNames[9:14]
    BlueNames <- TheColumnNames[15:18]
    YellowGreenNames <- TheColumnNames[22:25]
    RedNames <- TheColumnNames[19:21]
  }else if(Instrument == "Celesta-1"){
    UVNames <- NULL
    VioletNames <- TheColumnNames[12:17]
    BlueNames <- TheColumnNames[3:4]
    YellowGreenNames <- TheColumnNames[8:11]
    RedNames <- TheColumnNames[5:7]
  } else if(Instrument == "Celesta-2"){
    UVNames <- NULL
    VioletNames <- TheColumnNames[12:17]
    BlueNames <- TheColumnNames[3:4]
    YellowGreenNames <- TheColumnNames[8:11]
    RedNames <- TheColumnNames[5:7]
  } else if(Instrument == "Celesta-3"){
    UVNames <- NULL
    VioletNames <- TheColumnNames[12:17]
    BlueNames <- TheColumnNames[3:4]
    YellowGreenNames <- TheColumnNames[8:11]
    RedNames <- TheColumnNames[5:7]
  }  else if(Instrument == "Symphony S6-1"){
    UVNames <- TheColumnNames[26:33]  
    VioletNames <- TheColumnNames[16:25]
    BlueNames <- TheColumnNames[3:8]
    YellowGreenNames <- TheColumnNames[9:13]
    RedNames <- TheColumnNames[14:16]
  }else if(Instrument == "Symphony S6-2"){
    UVNames <- TheColumnNames[32:41]  
    VioletNames <- TheColumnNames[18:31]
    BlueNames <- TheColumnNames[3:11]
    YellowGreenNames <- TheColumnNames[42:50]
    RedNames <- TheColumnNames[12:17]
  }else if(Instrument == "Symphony S6-3"){
    UVNames <- TheColumnNames[3:12]  
    VioletNames <- TheColumnNames[13:26]
    BlueNames <- TheColumnNames[27:35]
    YellowGreenNames <- TheColumnNames[36:44]
    RedNames <- TheColumnNames[45:50]
  }
  
  VioletMFI <- Data[,VioletNames]
  BlueMFI <- Data[,BlueNames]
  YellowGreenMFI <- Data[,YellowGreenNames]
  RedMFI <- Data[,RedNames]
  
  if (!length(UVNames) == 0){
    UVMFI <- Data[,UVNames]
    InstrumentMFI <- relocate(mutate(bind_cols(c(UVMFI,VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)), 
                                     DateTime = Data$DateTime), DateTime, .before = 1)
  } else{
    InstrumentMFI <- relocate(mutate(bind_cols(c(VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)), 
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
    } else if(Instrument == "Symphony-3"){
      UVNames <- TheColumnNames[32:41]  
      VioletNames <- TheColumnNames[18:31]
      BlueNames <- TheColumnNames[3:11]
      YellowGreenNames <- TheColumnNames[42:50]
      RedNames <- TheColumnNames[12:17]
    }else if(Instrument == "Symphony-4"){
      UVNames <- TheColumnNames[20:27]  
      VioletNames <- TheColumnNames[12:19]
      BlueNames <- TheColumnNames[3:8]
      YellowGreenNames <- TheColumnNames[28:32]
      RedNames <- TheColumnNames[9:11]
    }else if(Instrument == "Symphony-5"){
      UVNames <- TheColumnNames[3:12]  
      VioletNames <- TheColumnNames[13:26]
      BlueNames <- TheColumnNames[27:35]
      YellowGreenNames <- TheColumnNames[36:45]
      RedNames <- TheColumnNames[46:51]
    }else if(Instrument == "Symphony A3"){
      UVNames <- TheColumnNames[3:8]  
      VioletNames <- TheColumnNames[9:14]
      BlueNames <- TheColumnNames[15:18]
      YellowGreenNames <- TheColumnNames[22:25]
      RedNames <- TheColumnNames[19:21]
    }else if(Instrument == "Celesta-1"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[12:17]
      BlueNames <- TheColumnNames[3:4]
      YellowGreenNames <- TheColumnNames[8:11]
      RedNames <- TheColumnNames[5:7]
    } else if(Instrument == "Celesta-2"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[12:17]
      BlueNames <- TheColumnNames[3:4]
      YellowGreenNames <- TheColumnNames[8:11]
      RedNames <- TheColumnNames[5:7]
    } else if(Instrument == "Celesta-3"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[12:17]
      BlueNames <- TheColumnNames[3:4]
      YellowGreenNames <- TheColumnNames[8:11]
      RedNames <- TheColumnNames[5:7]
    }  else if(Instrument == "Symphony S6-1"){
      UVNames <- TheColumnNames[26:33]  
      VioletNames <- TheColumnNames[16:25]
      BlueNames <- TheColumnNames[3:8]
      YellowGreenNames <- TheColumnNames[9:13]
      RedNames <- TheColumnNames[14:16]
    }else if(Instrument == "Symphony S6-2"){
      UVNames <- TheColumnNames[32:41]  
      VioletNames <- TheColumnNames[18:31]
      BlueNames <- TheColumnNames[3:11]
      YellowGreenNames <- TheColumnNames[42:50]
      RedNames <- TheColumnNames[12:17]
    }else if(Instrument == "Symphony S6-3"){
      UVNames <- TheColumnNames[3:12]  
      VioletNames <- TheColumnNames[13:26]
      BlueNames <- TheColumnNames[27:35]
      YellowGreenNames <- TheColumnNames[36:44]
      RedNames <- TheColumnNames[45:50]
    }
    VioletRCV <- Data[,VioletNames]
    BlueRCV <- Data[,BlueNames]
    YellowGreenRCV <- Data[,YellowGreenNames]
    RedRCV <- Data[,RedNames]
    
    if (!length(UVNames) == 0){
      UVRCV <- Data[,UVNames]
      InstrumentRCV <- relocate(mutate(bind_cols(c(UVRCV,VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)), 
                                       DateTime = Data$DateTime), DateTime, .before = 1)
    } else{
      InstrumentRCV <- relocate(mutate(bind_cols(c(VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)), 
                                       DateTime = Data$DateTime), DateTime, .before = 1)
    }
  }
  InstrumentStatus <- bind_cols(InstrumentMFI, InstrumentRCV[,-1])
  return(InstrumentStatus)
}
