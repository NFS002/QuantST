ST.runOnce <- function(symbol) {
  #make sure to specify correct directory
  cat("Attempting to access Ticker Symbols for:",symbol,"\n")
  v <- try(getSymbols(symbol))
  if (class(v) == "try-error") {
    cat("error retriving data for:",symbol,"\n")
    next
  }
  cat("Successfully acquired Ticker Symbols for: ",symbol,"\n")
  data = get(v)
  cat("Successfully acquired Time Series Data for:",symbol,"\n")
  print("Applying EA")
  #apply chaining function to data
  k <- try(ST.CH.01(data))
  if (class(k) == "try-error") {
    print(k)
    cat("Error applying EA to",symbol,"\n")
  }
  else {
  cat("Successfully applied EA to:",symbol,"\n")
  }
  print("...***<>***....")
  print("..****<>****...")
  print(".*****<>*****.")
  #evaluate averages?
  print("EXIT CODE 0")
  return(data)
}
