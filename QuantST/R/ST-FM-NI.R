#' Title 'ST.FM.NI'
#' Normalizes the price lists of the time series to account to  standardized index
#' based on the percentage change successive values
#' @param dataframe 
#' @param invert if true, inverts all rows, so the most recent date is at the end of the series
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.FM.NI <- function(dataframe, invert = FALSE) {
  size <- nrow(dataframe)
  if (invert) {
    print("inverting dataframe rows")
    dataframe[1:size,] <- dataframe[size:1,]
  }

  print("normalzing units...")

  if ("Open" %in% colnames(dataframe) || "open" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "open"] <- "Open"
    dataframe$"Open" <- as.numeric(as.character(dataframe$"Open"))
    dataframe$"PCOpen" <- 0
    dataframe$"NIOpen" <- 100
    for (i in 2:size) {
      dataframe[i,"PCOpen"] <- ((dataframe[i,"Open"] / dataframe[i-1,"Open"])-1)*100
      dataframe[i,"NIOpen"] <- (1 + (dataframe[i, "PCOpen"]/100)) * dataframe[(i-1),"NIOpen"]
    }
    names(dataframe)[names(dataframe) == "NIOpen"] <- "Open"
    dataframe$PCOpen <- NULL
    dataframe$Open <- round(dataframe$Open,3)
  }

  if ("High" %in% colnames(dataframe) || "high" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "high"] <- "High"
    dataframe$"High" <- as.numeric(as.character(dataframe$"High"))
    dataframe$"PCHigh" <- 0
    dataframe$"NIHigh" <- 100
    for (i in 2:size) {
      dataframe[i,"PCHigh"] <- ((dataframe[i,"High"] / dataframe[i-1,"High"])-1)*100
      dataframe[i,"NIHigh"] <- (1 + (dataframe[i, "PCHigh"]/100)) * dataframe[(i-1),"NIHigh"]
    }
    names(dataframe)[names(dataframe) == "NIHigh"] <- "High"
    dataframe$PCHigh <- NULL
    dataframe$High <- round(dataframe$High,3)
  }

  if ("Low" %in% colnames(dataframe) || "low" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "low"] <- "Low"
    dataframe$"Low" <- as.numeric(as.character(dataframe$"Low"))
    dataframe$"PCLow" <- 0
    dataframe$"NILow" <- 100
    for (i in 2:size) {
      dataframe[i,"PCLow"] <- ((dataframe[i,"Low"] / dataframe[i-1,"Low"])-1)*100
      dataframe[i,"NILow"] <- (1 + (dataframe[i, "PCLow"]/100)) * dataframe[(i-1),"NILow"]
    }
    names(dataframe)[names(dataframe) == "NILow"] <- "Low"
    dataframe$PCLow <- NULL
    dataframe$Low <- round(dataframe$Low,3)
  }

  if ("Close" %in% colnames(dataframe) || "close" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "close"] <- "Close"
    dataframe$"Close" <- as.numeric(as.character(dataframe$"Close"))
    dataframe$"PCClose" <- 0
    dataframe$"NIClose" <- 100
    for (i in 2:size) {
      dataframe[i,"PCClose"] <- ((dataframe[i,"Close"] / dataframe[i-1,"Close"])-1)*100
      dataframe[i,"NIClose"] <- (1 + (dataframe[i, "PCClose"]/100)) * dataframe[(i-1),"NIClose"]
    }
    names(dataframe)[names(dataframe) == "NIClose"] <- "Close"
    dataframe$PCClose <- NULL
    dataframe$Close <- round(dataframe$Close,3)
  }

  if ("Last" %in% colnames(dataframe) || "last" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "last"] <- "Last"
    dataframe$"Last" <- as.numeric(as.character(dataframe$"Last"))
    dataframe$"PCLast" <- 0
    dataframe$"NILast" <- 100
    for (i in 2:size) {
      dataframe[i,"PCLast"] <- ((dataframe[i,"Last"] / dataframe[i-1,"Last"])-1)*100
      dataframe[i,"NILast"] <- (1 + (dataframe[i, "PCLast"]/100)) * dataframe[(i-1),"NILast"]
    }
    names(dataframe)[names(dataframe) == "NILast"] <- "Last"
    dataframe$PCLast <- NULL
    dataframe$Last <- round(dataframe$Last,3)
  }

  if ("Settlement" %in% colnames(dataframe) || "settlement" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "Settlement"] <- "settlement"
    dataframe$"Settlement" <- as.numeric(as.character(dataframe$"Settlement"))
    dataframe$"PCSettlement" <- 0
    dataframe$"NISettlement" <- 100
    for (i in 2:size) {
      dataframe[i,"PCSettlement"] <- ((dataframe[i,"Settlement"] / dataframe[i-1,"Settlement"])-1)*100
      dataframe[i,"NISettlement"] <- (1 + (dataframe[i, "PCSettlement"]/100)) * dataframe[(i-1),"NISettlement"]
    }
    names(dataframe)[names(dataframe) == "NISettlement"] <- "Settlement"
    dataframe$PCSettlement <- NULL
    dataframe$Settlement <- round(dataframe$Settlement,3)
  }

  if ("Previous Settlement" %in% colnames(dataframe) || "Previous settlement" %in% colnames(dataframe) || "previous settlement" %in% colnames(dataframe)) {
    names(dataframe)[names(dataframe) == "Previous settlement"] <- "PreviousSettlement"
    names(dataframe)[names(dataframe) == "previous settlement"] <- "PreviousSettlement"
    dataframe$"PreviousSettlement" <- as.numeric(as.character(dataframe$"PreviousSettlement"))
    dataframe$"PCPreviousSettlement" <- 0
    dataframe$"NIPreviousSettlement" <- 100
    for (i in 2:size) {
      dataframe[i,"PCPreviousSettlement"] <- ((dataframe[i,"PreviousSettlement"] / dataframe[i-1,"PreviousSettlement"])-1)*100
      dataframe[i,"NIPreviousSettlement"] <- (1 + (dataframe[i, "PCPreviousSettlement"]/100)) * dataframe[(i-1),"NIPreviousSettlement"]
    }
    names(dataframe)[names(dataframe) == "NIPreviousSettlement"] <- "PreviousSettlement"
    dataframe$PCPreviousSettlement <- NULL
    dataframe$PreviousSettlement <- round(dataframe$PreviousSettlement,3)
  }

  dataframe <- dataframe[(1:size),]
  return(dataframe)

}
