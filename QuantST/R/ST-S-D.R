#' Title "ST.S.D"
#' Calculating the average daily profit and loss, as 
#' well as the running prfit and loss based on the time series dataframe, using 
#' the "SignalStrength" column as a general inidcator, the final step of the EA
#' @param dataframe 
#' @param risk The size of the Signalstrength at which a long position is to be opened,the 
#' size of the position itself, and the length of time this position is open
#' @return dataframe
#' @export
#'
#' @examples
ST.S.D <- function(dataframe, risk = 1) {
  size <- nrow(dataframe)
  numdeals <- 0
  tsig <- mean(dataframe$"Signalstrength" * 1.5)
  if ("Close" %in% colnames(dataframe)) {
    col <- dataframe$Close
  }
  else if ("Last" %in% colnames(dataframe)) {
    col <- dataframe$Last
  }
  else if ("Open" %in% colnames(dataframe)) {
    col <- dataframe$Open
  }
  else if ("Low" %in% colnames(dataframe)) {
    col <- dataframe$Low
  }
  else if ("High" %in% colnames(dataframe)) {
    col <- dataframe$High
  }   
  else if ("Settlement" %in% colnames(dataframe)) {
    col <- dataframe$Settlement
  }  
  else if ("Previous Settlement" %in% colnames(dataframe)) {
    col <- dataframe$"Previous Settlement"
  }
  else if ("Adjusted" %in% colnames(dataframe)) {
    col <- dataframe$Adjusted
  }  
  else {
    return(dataframe)
  } 
  for (i in (risk+1):size) {
    dataframe[i, "Signalstrength"] <- mean(dataframe[i:(i-risk),"Signalstrength"], na.rm = TRUE)
  }
  dataframe$"Dpl" <- 0
  dataframe$"Rpl" <- 0
  for (i in (2:size)) {  
    if(dataframe[i,"Signalstrength"] >= tsig) {
      dataframe[i,"Dpl"] <- (dataframe [i,"Signalstrength"] * (col[i]-col[i-1]) * risk)
      numdeals <- numdeals + 1
    }
  }
  dataframe[1,"Rpl"] <- dataframe[1,"Dpl"] 
  for (i in 2:size) {
    dataframe[i,"Rpl"] <- dataframe[i,"Dpl"] + dataframe[(i-1),"Rpl"]
  }
  dataframe$"Dpl" <- round(dataframe$"Dpl",3)
  dataframe$"Rpl" <- round(dataframe$"Rpl",3)
  srt <- mean(dataframe$"Dpl")
  dataframestats <- dataframe[,c("Dpl","Rpl")]
  print((data.table(dataframestats)))
  if ("High" %in% colnames(dataframe) && "Low" %in% colnames(dataframe)) {
    std <- sd(c(dataframe$"High",dataframe$"Low"))
    print(paste("Volatility: ", std*100))
    print(paste("Sharpe Ratio: ", srt/std))
  }
  print(paste("Exposure: ", (sum(dataframe$"Signalstrength"[dataframe$"Signalstrength" >= tsig]) * risk)))
  print(paste("Mean Dpl: ",srt))
  win <- 0
  lose <- 0
  for (i in 1: size) {
    if (dataframe[i, "Dpl"] > 0) {
      win <- win + 1
    }
    if (dataframe[i, "Dpl"] < 0) {
      lose <- lose + 1
    }
  }
  winningp <- ((win/(win + lose))*100) 
  print(paste(("Num of deals: "),numdeals))
  print(paste("Win: ",winningp, "%" ))
  return(dataframe)
}