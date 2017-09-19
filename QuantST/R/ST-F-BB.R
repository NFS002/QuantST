#' Title "ST.F.BB"
#' Calculates bolinger bands for a price series dataframe is the original dataframe to bind to HLC is the ???HIGH??? ???LOW??? ???CLOSE??? price series of the data,
#' or whatever price columns are available, if only a univariate is available, that will be used
#' @param dataframe 
#' @param n is the time period for which the MA is calculated (in days)
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.F.BB <- function(dataframe, n=20) {
  if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Close" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,c("High","Low","Close")],n)
  }
  else if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Last" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,c("High","Low","Last")],n)
  }
  else if ("Close" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Close"],n)
  }
  else if ("Last" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Last"],n)
  }
  else if ("Open" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Open"],n)
  }
  else if ("Low" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Low"],n)
  }
  else if ("High" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"High"],n)
  }   
  else if ("Settlement" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Settlement"],n)
  }  
  else if ("Previous Settlement" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Previous Settlement"],n)
  }
  else if ("Adjusted" %in% colnames(dataframe)) {
    BBS <- BBands(dataframe[,"Adjusted"],n)
  }  
  else if (ncol(dataframe) > 1) {
    BBS <- BBands(dataframe,n)
  }  
  else {
    return(dataframe)
  }  
  dataframe <- cbind(dataframe, BBS)
  names(dataframe)[names(dataframe) == "dn"] <- "BBlow"
  names(dataframe)[names(dataframe) == "up"] <- "BBhigh"
  names(dataframe)[names(dataframe) == "mavg"] <- "BBav"
  dataframe[is.na(dataframe)] <- 0
  dataframe$"BBlow" <- round(dataframe$"BBlow",3)
  dataframe$"BBhigh" <- round(dataframe$"BBhigh",3)
  dataframe$"BBav" <- round(dataframe$"BBav",3)
  dataframe$"pctB" <- round(dataframe$"pctB",3)
  return(dataframe)
}