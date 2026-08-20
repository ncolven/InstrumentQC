FHStatus <- function(BDTable){
  Status <- "Pass"
  if (any(BDTable$MFI >12000)){
    Status <- "Warning"
  }
if (any(BDTable$MFI < 8000)||any(BDTable$RCV >= 15)){
  Status <- "Fail"
}
  return(Status)
}