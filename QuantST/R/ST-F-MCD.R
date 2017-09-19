#' Title "ST.F.MCD"
#' Calculates the MACD and MACD-Signal vectors, 
#' using the ???Close??? column, if available, and binds the result to the original
#' dataframe. Only functions for dataframes more than 27 rows.
#' @param dataframe 
#' @param nFast time period (days)
#' @param nSlow time period (days)
#' @param nSig time period (days)
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.F.MCD <- function(dataframe, nFast = 12, nSlow = 14, nSig = 9) {
  if ("Close" %in% colnames(dataframe)) {
    mVector = dataframe$Close
  }
  else if ("Last" %in% colnames(dataframe)) {
    mVector = dataframe$Last
  }
  else if ("Open" %in% colnames(dataframe)) {
    mVector = dataframe$Open
  }
  else if ("Low" %in% colnames(dataframe)) {
    mVector = dataframe$Low
  }
  else if ("High" %in% colnames(dataframe)) {
    mVector = dataframe$High
  }  
  else if ("Settlement" %in% colnames(dataframe)) {
    mVector = dataframe$Settlement
  }
  else if ("Previous Settlement" %in% colnames(dataframe)) {
    mVector = dataframe$"Previous Settlement"
  }
  else if ("Adjusted" %in% colnames(dataframe)) {
    mVector = dataframe$Adjusted
  }
  else {
    return(dataframe)
  }
  if (nrow(dataframe) < 27) {
    return(dataframe)  
  }  
  macframe <- MACD(mVector,nFast,nSlow,nSig)
  dataframe <- cbind(dataframe, macframe)
  names(dataframe)[names(dataframe) == "macd"] <- "MACD"
  names(dataframe)[names(dataframe) == "signal"] <- "MACD-S"
  dataframe$"MACD" <- round(dataframe$"MACD",3)
  dataframe$"MACD-S" <- round(dataframe$"MACD-S",3)
  dataframe[is.na(dataframe)] <- 0
  return(dataframe)
}