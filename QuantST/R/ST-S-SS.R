#' Title 'ST.S.SS'
#' Generates a "Signalstrength" column based on stochastic, MACD, ADX, and BBand indicators. 
#' Requires columns (including crossover columns) for all these indicators.
#' @param dataframe 
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.S.SS <- function(dataframe) {
  size <- nrow(dataframe)
  dataframe$Signalstrength <- 0
  # +1 for low level crossovers
  for (i in 1:size) {
    if (dataframe[i,"fastD"] <= 0.25) {
      if (dataframe[i,"STcross"] == 1) {
        dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 1
      }
    }
  }
  # add 0.1 for crossovers that haven't happened for 5 days
  for (i in 6:size) {
    if (dataframe[i,"fastD"] <= 0.25) {
      if (dataframe[i,"STcross"] == 1) {
        if (all(dataframe[(i-1):(i-5),"STcross"] == 0)) {  
          dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
        }
      }
    }
  }
  # add 0.1 Smooth descent of the fastD line before a crossover
  for (i in 6:size) {
    if (all(diff(dataframe[(i-4):i,"fastD"]) > 0.05)) {
      if (dataframe[i,"fastD"] <= 0.25) {
        if (dataframe[i,"STcross"] == 1) {
          if (all(dataframe[(i-1):(i-5),"STcross"] == 0)) {  
            dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
          }
        }
      }
    }
  }
  # add 1 for MACD Crossovers
  for (i in 1:size) {
    if (dataframe[i,"MDcross"] == 1) {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 1
    }
  }
  for (i in 5:size) {
    if ((dataframe[i,"MACD"] - dataframe[(i-4),"MACD"]) < 0) {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
    }
    if ((dataframe[i,"MACD-S"] - dataframe[(i-4),"MACD-S"]) < 0) {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
    }
  }
  
  # 1 for DIp/DIn crossovers
  # Add 0.1 for when the ADX is above 20 at the time of the crossover, and another 0.1 if both the DIp and the ADX are
  #  and another 0.1 if the DIn is less than half the DIp at this point, and another 0.1 for less than a quarter
  for (i in 1:size) {
    if (dataframe[i,"DIcross"] == 1) {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 1  
      if (dataframe[i,"ADX"] > 20) { 
        dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
      }
      if (dataframe[i,"DIp"] > 20) {
        dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
      }
      if (dataframe[i,"DIp"] >= (2*dataframe[i,"DIn"])) {
        dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
      }
      if (dataframe[i,"DIp"] >= (4*dataframe[i,"DIn"])) {
        dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.1
      }      
    }
  }
  # 1 for BBcrossovers
  for (i in 1:size) {
    if (dataframe[i,"BBcross"] == 1) {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 1  
    }
  }
  
  # 0.2 for when the BBav is between 1 and 2 SD's
  for (i in 1:size) {
    if (dataframe[i,"pctB"] >= 0.5 && dataframe[i,"pctB"] <= 0.8)  {
      dataframe[i,"Signalstrength"] <- dataframe[i,"Signalstrength"] + 0.5
    }
  }
  # emalgamate the crossovers over the past 3 days
  i <- 5
  while (i < size) {
    dataframe[i,"Signalstrength"] <- sum(dataframe[i:(i-3),"Signalstrength"])
    i <- i + 5
  }
  dataframe$Signalstrength <- round(dataframe$Signalstrength, 3)
  return(dataframe)
}