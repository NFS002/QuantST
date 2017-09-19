#' Title "ST.FM.CP"
#' function to copy the whole of data.frame from xts/zoo to a matrix of integer
#' vectors. The function does not change values in the dataframe, or add new ones,
#' but copies it from one format to another, allowing mathermatical calculations on
#' its values.
#' @param dataframe 
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.FM.CP <- function(dataframe){
  width <- ncol(dataframe)
  size <- nrow(dataframe)
  copydata <- data.frame(matrix(ncol = width, nrow = size))
  for (i in 1: width) {
    copydata[,i] <- as.numeric(as.character(dataframe[,i]))
    names(copydata)[i] <- names(dataframe)[i]
  }
  return(copydata)
}