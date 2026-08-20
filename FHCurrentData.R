FHCurrentData <- function(x, MainFolder){
  ArchiveCSV <- read.csv(list.files(file.path(MainFolder,x,"Archive"), pattern = "Holistic", full.names = T), check.names = F)
  InstrumentStatus <- BDSummary(ArchiveCSV, x)
  return(InstrumentStatus)
}