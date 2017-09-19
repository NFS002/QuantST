ST.Run <- function() {
  outputFile <- file("R/ST/output.txt")
  sink(outputFile)
  sink(outputFile,type = "message")
  #make sure to specify correct directory
  allStockSymbols = read.delim2("R/ST/YahooStockTickerSymbols.csv",sep = ",",dec = ".",colClasses = c(rep("character",5),"integer"))
  #remove all NA rows from 'Ticker' column
  allStockSymbols = allStockSymbols[!is.na(allStockSymbols$Ticker),]
  #remove all empty rows from 'Ticker' column
  allStockSymbols = allStockSymbols[!nchar(allStockSymbols$Ticker) == 0,]
  for (i in 1:nrow(allStockSymbols)) {
    symbol <- allStockSymbols$Ticker[i]
    cat("Attempting to access Ticker Symbols for:",symbol,"\n")
    v <- try(getSymbols(symbol))
    if (class(v) == "try-error") {
      cat("error retriving data for:",symbol,"\n")
      next
    }
    cat("Successfully acquired Ticker Symbols for: ",symbol,"\n")
    data = get(v)
    cat("Successfully acquired Time Series Data for:",symbol,"\n")
    cat("Applying comments to:",symbol,"\n")
    TickerSymbol <- cat("Ticker Symbol:",symbol)
    print(TickerSymbol)
    TickerName <- cat("Name: ",allStockSymbols$Name[i])
    print(TickerName)
    TickerExchange <- cat("Exchange: ",allStockSymbols$Exchange[i])
    print(TickerExchange)
    TickerCountry <- cat("Country: ",allStockSymbols$Country[i])
    print(TickerCountry)
    TickerCategoryName <- cat("Category Name: ",allStockSymbols$"Category Name"[i])
    print(TickerCategoryName)
    TickerCategoryNumber <- cat("Category Number: ",allStockSymbols$"Category Number"[i])
    print(TickerCategoryNumber)
    comment(data) <- c(TickerSymbol,TickerName,TickerExchange,TickerCountry,TickerCountry,TickerCategoryName,TickerCategoryNumber)
    print("APPLY NOW")
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
  }
  #evaluate averages?
  print("EXIT CODE 0")
  sink()
}

