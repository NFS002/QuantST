#' Title "ST.F.ADX"
#' function to calculate the ADX(DIp DIn, ADX) of a price series.
#' and bind the result to the orginal data frame
#' @param dataframe time series on which the adx is calculated
#' @param n time period (days)
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.F.ADX <- function(dataframe, n = 14) {
  if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Close" %in% colnames(dataframe)) {
    ADX <- ADX(dataframe[,c("High","Low","Close")],n)
  }
  else if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Last" %in% colnames(dataframe)) {
    ADX <- ADX(dataframe[,c("High","Low","Last")],n)
  }
  else if (ncol(dataframe) > 2) {
    ADX <- ADX(dataframe,n)
  }  
  else {
    return(dataframe)
  }  
  dataframe <- cbind(dataframe, ADX) 
  dataframe$"DX" <- NULL
  dataframe[is.na(dataframe)] <- 0
  dataframe$"DIp" <- round(dataframe$"DIp",3)
  dataframe$"DIn" <- round(dataframe$"DIn",3)
  dataframe$"ADX" <- round(dataframe$"ADX",3)
  return(dataframe)
}