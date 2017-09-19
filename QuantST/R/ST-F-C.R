#' Title "ST.F.C"
#' calculates the point(s) at which the columns x & y crossover and binds 
#' this new column to the dataframe.
#' @param dataframe 
#' @param x vector or object coercible to xts
#' @param y vector or object coercible to xts
#' @param name the name of the new column
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.F.C <- function(dataframe, x,y, name = "Crossover") {
  dataframe$Crossover <- crossover(x,y)
  dataframe$cross <- 0
  size <- nrow(dataframe) 
  for (i in 1:size) {
    if (!is.na(dataframe[i,"Crossover"])) {
      if (dataframe[i,"Crossover"] == "UP") {
        dataframe[i,"cross"] <- 1
      }
      else {
        dataframe[i,"cross"] <- -1
      }
    }
    else {
      dataframe[i,"cross"] <- 0
    }
  }
  dataframe$Crossover <- NULL
  colnames(dataframe)[colnames(dataframe) == "cross"] <-  name
  dataframe <- dataframe[(1:size),]
  return(dataframe)
} 