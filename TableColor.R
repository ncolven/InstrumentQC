TableColor <- function(InstrumentStatus){
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
return(DailyTable)
}