test <- pivot_wider(InstrumentStatus, names_from = Detector, values_from = c(MFI,MFIStatus, RCV, RCVStatus))
test <- test[!str_detect(colnames(test), "Status")]
slice_head(test)

IntermediateTable <- tidyr::pivot_longer(InstrumentStatus[,!str_detect(colnames(InstrumentStatus), "rCV")], !DateTime, names_to="Detector", values_to="MFI")
RCVTable <- pivot_longer(mutate(DateTime = InstrumentStatus$DateTime, InstrumentStatus[,str_detect(colnames(InstrumentStatus), "rCV")]),
                         !DateTime, values_to = "RCV")
IntermediateTable <- bind_cols(IntermediateTable, RCVTable[,3])
DetailTable <- arrange(IntermediateTable, desc(IntermediateTable$DateTime))
DetailTable <- DetailTable[str_detect(as_date(DetailTable$DateTime), 
                                      as.character(as_date(DetailTable$DateTime[1]))),]
TableDate <- as.character(as_date(DetailTable$DateTime[1]))
DetailTable <- DetailTable[,-1]
DailyTable <- DetailTable %>% gt() %>% tab_header(title = TableDate) %>%
                                      data_color(columns = MFI, 
                                      fn = function(x) {
                                        dplyr::case_when(between(x, 8000, 12000) ~ "#0B6623", x > 12000 ~ 
                                                           "#FF6E00", x < 8000 ~ 
                                                           "#C80815", TRUE ~ NA_character_)
                                      }) %>% data_color(columns = RCV, 
                                                        fn = function(x) {
                                                          dplyr::case_when(x < 15 ~ "#0B6623", x >= 15 ~ 
                                                                             "#C80815", TRUE ~ NA_character_)
                                                        })
DailyTable


InstrumentMFI <- tidyr::pivot_longer(InstrumentMFI, !DateTime, names_to = "Detector", values_to = "MFI")
MFIStatus <- tidyr::pivot_longer(MFIStatus, !DateTime, names_to = "Detector", values_to = "Status")
MFITable <- mutate(InstrumentMFI, Status = MFIStatus$Status)
InstrumentRCV <- tidyr::pivot_longer(InstrumentRCV, !DateTime, names_to = "Detector", values_to = "RCV")
RCVStatus <- tidyr::pivot_longer(RCVStatus, !DateTime, names_to = "Detector", values_to = "Status")
RCVTable <- mutate(InstrumentRCV, Status = RCVStatus$Status)
InstrumentStatus <- mutate(MFITable[,1:3], MFIStatus=MFITable$Status, 
                           RCV = RCVTable$RCV, RCVStatus = RCVStatus$Status)