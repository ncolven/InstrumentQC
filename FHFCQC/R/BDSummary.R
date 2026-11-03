#' Converts archive csv file for conventional BD instruments into readable format for .qmd files
#'
#' @param x data frame from archive csv file
#' @param Instrument Instrument name
#' @export

BDSummary <- function (x, Instrument){
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
    if (nrow(Data) != 0) {
      Data <- slice(x, 1)
    }
  }else {
    Data <- x
  }
  TheColumns <- Data %>% dplyr::select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
  TheColumns <- setdiff(TheColumns, "TIME")

  # MFIs
  {
    TheIntermediate <- TheColumns[!str_detect(TheColumns, "Gain")]
    TheIntermediate <- TheIntermediate[!str_detect(TheIntermediate, "rCV")]
    TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A$")]
    if (length(TheColumnNames) == 0){
      TheIntermediate <- TheIntermediate[str_detect(TheIntermediate, ".A")]
      TheColumnNames <- TheIntermediate[!str_detect(TheIntermediate, "_")]
    }

    if (Instrument == "Fortessa"){
      UVNames <- TheColumnNames[15:21]
      VioletNames <- TheColumnNames[22:29]
      BlueNames <- TheColumnNames[3:7]
      YellowGreenNames <- TheColumnNames[8:11]
      YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[30]
      RedNames <- TheColumnNames[12:14]
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
    }  else if(Instrument == "Symphony-3"){
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
    } else if(Instrument == "Symphony-5"){
      UVNames <- TheColumnNames[3:12]
      VioletNames <- TheColumnNames[13:26]
      BlueNames <- TheColumnNames[27:35]
      YellowGreenNames <- TheColumnNames[36:44]
      RedNames <- TheColumnNames[45:50]
    } else if(Instrument == "Symphony A3"){
      UVNames <- TheColumnNames[3:8]
      VioletNames <- TheColumnNames[9:14]
      BlueNames <- TheColumnNames[15:18]
      YellowGreenNames <- TheColumnNames[22:25]
      RedNames <- TheColumnNames[19:21]
    }else if(Instrument == "Celesta-1"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[15:18]
      BlueNames <- TheColumnNames[3:5]
      YellowGreenNames <- TheColumnNames[9:14]
      RedNames <- TheColumnNames[6:8]
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

    VioletStatus <- MFICheck(VioletMFI)
    BlueStatus <- MFICheck(BlueMFI)
    YellowGreenStatus <- MFICheck(YellowGreenMFI)
    RedStatus <- MFICheck(RedMFI)
    if (!length(UVNames) == 0){
      UVMFI <- Data[,UVNames]
      UVStatus <- MFICheck(UVMFI)
      InstrumentMFI <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(UVMFI,VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)),
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      MFIStatus <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)),
                                   DateTime = Data$DateTime), DateTime, .before = 1)
    } else{
      InstrumentMFI <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(VioletMFI,BlueMFI,YellowGreenMFI,RedMFI)),
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      MFIStatus <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)),
                                   DateTime = Data$DateTime), DateTime, .before = 1)
    }
  }
  # rCVs
  {
    TheIntermediate <- TheColumns[str_detect(TheColumns, "rCV")]
    TheIntermediate <- TheIntermediate[!str_detect(TheIntermediate, "-H-")]
    TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
    if (length(TheColumnNames) == 0){
      TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, ".A")]
    }
    if (Instrument == "Fortessa"){
      UVNames <- TheColumnNames[15:21]
      VioletNames <- TheColumnNames[22:29]
      BlueNames <- TheColumnNames[3:7]
      YellowGreenNames <- TheColumnNames[8:11]
      YellowGreenNames[length(YellowGreenNames)+1] <- TheColumnNames[30]
      RedNames <- TheColumnNames[12:14]
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
      YellowGreenNames <- TheColumnNames[36:44]
      RedNames <- TheColumnNames[45:50]
    } else if(Instrument == "Symphony A3"){
      UVNames <- TheColumnNames[3:8]
      VioletNames <- TheColumnNames[9:14]
      BlueNames <- TheColumnNames[15:18]
      YellowGreenNames <- TheColumnNames[22:25]
      RedNames <- TheColumnNames[19:21]
    }else if(Instrument == "Celesta-1"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[15:18]
      BlueNames <- TheColumnNames[3:5]
      YellowGreenNames <- TheColumnNames[9:14]
      RedNames <- TheColumnNames[6:8]
    } else if(Instrument == "Celesta-2"){
      UVNames <- NULL
      VioletNames <- TheColumnNames[12:17]
      BlueNames <- TheColumnNames[3:4]
      YellowGreenNames <- TheColumnNames[8:11]
      RedNames <- TheColumnNames[5:7]
    }else if(Instrument == "Celesta-3"){
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

    VioletStatus <- RCVCheck(VioletRCV)
    BlueStatus <- RCVCheck(BlueRCV)
    YellowGreenStatus <- RCVCheck(YellowGreenRCV)
    RedStatus <- RCVCheck(RedRCV)
    if (!length(UVNames) == 0){
      UVRCV <- Data[,UVNames]
      UVStatus <- RCVCheck(UVRCV)
      InstrumentRCV <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(UVRCV,VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)),
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      RCVStatus <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(UVStatus,VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)),
                                   DateTime = Data$DateTime), DateTime, .before = 1)
    } else{
      InstrumentRCV <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(VioletRCV,BlueRCV,YellowGreenRCV,RedRCV)),
                                       DateTime = Data$DateTime), DateTime, .before = 1)
      RCVStatus <- dplyr::relocate(dplyr::mutate(dplyr::bind_cols(c(VioletStatus,BlueStatus,YellowGreenStatus,RedStatus)),
                                   DateTime = Data$DateTime), DateTime, .before = 1)
    }
  }
  InstrumentStatus <- dplyr::bind_cols(InstrumentMFI, InstrumentRCV[,-1])
  return(InstrumentStatus)
}
