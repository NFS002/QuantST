#' Title "ST.CH.01"
#' The first chaining function, which applied the whole EA to a given time series matrix
#' The dataframe must have at least one price column
#' @param dataframe the EA is applied to
#'
#' @return dataframe 
#' @export
#'
#' @examples
ST.CH.01 <-function(dataframe) {
  dataframe <- ST.FM.R(dataframe)
  dataframe <- ST.FM.CP(dataframe)
  dataframe <- ST.FM.NI(dataframe)
  dataframe <- ST.F.ST(dataframe)
  dataframe <- ST.F.C(dataframe,dataframe$fastD,dataframe$slowD, name = "STcross")
  dataframe <- ST.F.MCD(dataframe)
  dataframe <- ST.F.C(dataframe,dataframe$MACD,dataframe$"MACD-S", name = "MDcross")
  dataframe <- ST.F.ADX(dataframe)
  dataframe <- ST.F.C(dataframe,dataframe$DIp,dataframe$DIn, name = "DIcross")
  dataframe <- ST.F.BB(dataframe)
  dataframe <- ST.F.C(dataframe,dataframe$BBlow,dataframe$BBav, name = "BBcross")
  dataframe <- ST.S.SS(dataframe)
  dataframe <- ST.S.D(dataframe)
  return(dataframe)
}