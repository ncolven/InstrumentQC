DetailTable <- function(InstrumentStatus){
  IntermediateTable <- tidyr::pivot_longer(InstrumentStatus[,!str_detect(colnames(InstrumentStatus), "rCV")], !DateTime, names_to="Detector", values_to="MFI")
  RCVTable <- pivot_longer(mutate(DateTime = InstrumentStatus$DateTime, InstrumentStatus[,str_detect(colnames(InstrumentStatus), "rCV")]),
                           !DateTime, values_to = "RCV")
  IntermediateTable <- bind_cols(IntermediateTable, RCVTable[,3])
  DetailTable <- arrange(IntermediateTable, desc(IntermediateTable$DateTime))
  DetailTable <- DetailTable[str_detect(as_date(DetailTable$DateTime), 
                                        as.character(as_date(DetailTable$DateTime[1]))),]
  TableDate <- as.character(as_date(DetailTable$DateTime[1]))
  DetailTable <- DetailTable[,-1]
  
  return(DetailTable)
}