#' Title "ST.F.ST"
#' Calculates the stochastics the given dataframe
#' @param dataframe 
#' @param fastK time period in days, defaults to 14
#' @param fastD time period in days, defaults to 7
#' @param slowD time period in days, defaults to 3
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.F.ST <- function(dataframe,fastK = 14, fastD = 7, slowD = 3)  
{
  size <- nrow(dataframe)
  if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Close" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,c("High","Low","Close")],fastK,fastD,slowD)
  }
  else if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe) && "Last" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,c("High","Low","Last")],fastK,fastD,slowD)
  }
  else if ("Close" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Close"],fastK,fastD,slowD)
  }
  else if ("Last" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Last"],fastK,fastD,slowD)
  }
  else if ("Open" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Open"],fastK,fastD,slowD)
  }
  else if ("Low" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Low"],fastK,fastD,slowD)
  }
  else if ("High" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"High"],fastK,fastD,slowD)
  }   
  else if ("Settlement" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Settlement"],fastK,fastD,slowD)
  }  
  else if ("Previous Settlement" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Previous Settlement"],fastK,fastD,slowD)
  }
  else if ("Adjusted" %in% colnames(dataframe)) {
    stochframe <- stoch(dataframe[,"Adjusted"],fastK,fastD,slowD)
  }  
  else if (ncol(dataframe) > 1) {
    stochframe <- stoch(dataframe,fastK,fastD,slowD)
  }  
  else {
    return(dataframe)
  } 
  dataframe <- cbind(dataframe,stochframe)
  dataframe[is.na(dataframe)] <- 0
  dataframe$fastK <- round(dataframe$fastK,3)
  dataframe$fastD <- round(dataframe$fastD,3)
  dataframe$slowD <- round(dataframe$slowD,3)
  dataframe <- dataframe[(1:size),]
  return(dataframe)
}