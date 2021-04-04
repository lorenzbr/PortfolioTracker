#### Get financial data from Yahoo Finance via API
####


## several functions to get and update financial data
##
##
##

## to do:
## consider PDFs with more than one page
## access website to get conversion of ISIN to ticker automatically (prefer Xetra prices)
## when transaction data is retrieved. check whether this is an older transaction than before




get.financials.based.on.transaction <- function(filename.processed.transaction.data){

  #### function which accesses financial data from yahoo and starts with transaction date

  ## path names
  source(paste0("global.R"))

  ## get table with conversion of all relevant ISINs to ticker
  df.isin.ticker.converter <- get.ticker.from.isin()

  ## load transaction data
  df.transaction.data <- data.table::fread(paste0(path.data.processed.transactions.new,filename.processed.transaction.data))

  ## add ticker to transaction data
  df.transaction.data <- merge(df.transaction.data,df.isin.ticker.converter,by="isin",all.x = T)


  ## transaction date to date format
  df.transaction.data$transaction_date <- as.Date(df.transaction.data$transaction_date,"%d-%m-%Y")

  ## get current date
  today <- Sys.Date()


  ## for loop over all transactions in file
  for(i in 1:nrow(df.transaction.data)){

    tryCatch({

    ## select transaction date and ticker
    transaction.date <- df.transaction.data$transaction_date[i]
    ticker <- df.transaction.data$ticker[i]


    ## check whether a financial data for this ticker based on transaction date already exists
    # if no: continue
    # if yes: 1) if the focal one is younger, don't do anything 2) if the focal one is older, continue

    ## get all financials with same ticker
    df.financials.same.ticker <- data.frame(filename = list.files(path.data.raw.financials,pattern=ticker))

    ## initialize earliest.date (does not make sense, only needed to have an existent date)
    earliest.date <- as.Date("1900-01-01","%Y-%m-%d")

    ## identify earliest date for each of those files and compare to transaction.date
    if(nrow(df.financials.same.ticker) > 0){
      df.financials.same.ticker$first_date <- stringr::str_match(df.financials.same.ticker$filename, "from_(.*?)_to")[,2]
      df.financials.same.ticker$first_date <- as.Date(df.financials.same.ticker$first_date,"%Y-%m-%d")
      df.financials.same.ticker <- df.financials.same.ticker[df.financials.same.ticker$first_date == min(df.financials.same.ticker$first_date),]
      earliest.date <- unique(df.financials.same.ticker$first_date)

      ## if new transaction is older than earliest date updated "to" date
      if(transaction.date < earliest.date){today <- earliest.date - 1}

    } ## end of if statement


    ## file name for the data
    filename.data.raw.financials <- paste0("prices_ticker_",ticker,"_from_",transaction.date,"_to_",today,".csv")


    ## if transaction.date is older than earliest.date of existing transactions or no financials with same ticker exist, do this
    if(transaction.date < earliest.date | nrow(df.financials.same.ticker)==0){

      ## check whether such a file exists already, then no need to download again
      if(!(file.exists(paste0(path.data.raw.financials,filename.data.raw.financials)))){

        ## get financial data from yahoo
        df.ticker.prices <- get.financials.from.yahoo(ticker,transaction.date,today)

        ## start and end date
        from <- min(df.ticker.prices$date)
        to <- max(df.ticker.prices$date)

        ## file name for the data
        filename.data.raw.financials <- paste0("prices_ticker_",ticker,"_from_",from,"_to_",to,".csv")

        ## store as csv in raw financial data
        data.table::fwrite(df.ticker.prices,paste0(path.data.raw.financials,filename.data.raw.financials))

        if(today != Sys.Date()){
          print(paste("Older transaction: Financial data update for",ticker,"from",transaction.date,"to",today,"successfully downloaded."))
        } else {print(paste("Financial data for",ticker,"from",transaction.date,"to",today,"successfully downloaded."))}

      } else {

        print(paste("Financial data for",ticker,"from",transaction.date,"to",today,"already downloaded."," File",
                    filename.data.raw.financials,"exists already."))



      }  ## end of else if condition which checks whether file already exists


    } else {print("Financials based on older transaction already exist.")} ## end of if else statement: transaction.date older than any other or no transactions exist


    }, error=function(cond){
      message(paste0("No financials for ticker '",ticker,"'"))
      message("Original message:")
      message(cond)

    }

    ) ## end of try catch

  } ## end of for loop over all transactions in file

  ## move files to folders once all transaction files are used for financial data requests
  file.rename(from = paste0(path.data.processed.transactions.new,filename.processed.transaction.data),
              to = paste0(path.data.processed.transactions.used,filename.processed.transaction.data))


} ## end of function get transaction data



update.financial.data <- function(){

  #### update all financial data based on all tickers in folder and last date for each ticker

  ## path names
  source(paste0("global.R"))

  ## load file names for financial data
  filename.data.raw.financials.with.ticker <- list.files(path.data.raw.financials)

  ## only run code if filename.data.raw.financials.with.ticker is not empty
  if(!(rlang::is_empty(filename.data.raw.financials.with.ticker))){

  ## create data frame
  df.files.financial.data <- data.frame(filename = filename.data.raw.financials.with.ticker)

  ## identify last date
  df.files.financial.data$last_date <- stringr::str_match(df.files.financial.data$filename, "to_(.*?).csv")[,2]

  ## identify tickers
  df.files.financial.data$ticker <- stringr::str_match(df.files.financial.data$filename, "ticker_(.*?)_from")[,2]

  ## keep latest date for each ticker
  df.files.financial.data <- aggregate(last_date ~ ticker, data = df.files.financial.data, max)

  ## get date of today
  today <- Sys.Date()

  ## keep only tickers which are not up to date
  df.files.financial.data <- df.files.financial.data[df.files.financial.data$last_date < today,]

  ## if at least one ticker is not up to date
  if(nrow(df.files.financial.data) > 0){

    for(i in 1: nrow(df.files.financial.data)){

      skip_to_next <- FALSE

      tryCatch({

      from <- as.Date(df.files.financial.data$last_date[i]) + 1
      ticker <- df.files.financial.data$ticker[i]

      if(today > from){

        ## get financial data for them
        df.updated.financials <- get.financials.from.yahoo(ticker,from,today)

        ## start and end date
        from <- min(df.updated.financials$date)
        to <- max(df.updated.financials$date)

        ## file name for the data
        filename.data.raw.financials <- paste0("prices_ticker_",ticker,"_from_",from,"_to_",to,".csv")

        ## store as csv in raw financial data
        data.table::fwrite(df.updated.financials,paste0(path.data.raw.financials,filename.data.raw.financials))

        print(paste("Financial data for",ticker,"from",from,"to",to,"successfully downloaded."))

      } else {print("Start date needs to be earlier than end date.")}

      }, error = function(e) { skip_to_next <- TRUE})

      if(skip_to_next) { next }

    } ## end of for loop over all tickers which are not up to date


  } else {

    print("Everything up to date!")

  } ## end of else statement that checks whether updates are needed

  } else {print("No financials available for update.")} ## end of if else statement whether file names are non empty

} ## end of function update financial data



get.financials.from.yahoo <- function(ticker,from,to){

  #### get financial data from yahoo finance API and clean output data a bit

  ## get prices
  ticker.prices <- quantmod::getSymbols(ticker, from = from, to = to, auto.assign = FALSE)

  ## convert to data frame
  df.ticker.prices <- data.frame(ticker.prices)

  ## change column names
  names(df.ticker.prices) <- gsub(paste0(ticker,"\\."),"",names(df.ticker.prices))


  ## column names to lower case
  names(df.ticker.prices) <- tolower(names(df.ticker.prices))

  ## create data variable
  df.ticker.prices$date <- rownames(df.ticker.prices)

  ## new index for row names
  rownames(df.ticker.prices) <- 1:nrow(df.ticker.prices)

  ##  convert to date format
  df.ticker.prices$date <- as.Date(df.ticker.prices$date)

  ## remove NAs
  df.ticker.prices <- df.ticker.prices[!(is.na(df.ticker.prices$adjusted)),]

  return(df.ticker.prices)

}


