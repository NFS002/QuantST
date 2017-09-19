#' Title "ST.FM.R"
#' renames the columns of the dataframe.
#' @param dataframe 
#'
#' @return dataframe
#' @export
#'
#' @examples
ST.FM.R <- function(dataframe) {
  for (i in 1:ncol(dataframe)) {
    colnames(dataframe)[i] <- gsub(".*date.*","Date",colnames(dataframe)[i],ignore.case = TRUE)  
    colnames(dataframe)[i] <- gsub(".*open.*","Open",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*close.*","Close",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*low.*","Low",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*high.*","High",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*settlement.*","Settlement",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*volume.*","Volume",colnames(dataframe)[i],ignore.case = TRUE)
    colnames(dataframe)[i] <- gsub(".*adjusted.*","Adjusted",colnames(dataframe)[i],ignore.case = TRUE)
  }
  return(dataframe)  
}