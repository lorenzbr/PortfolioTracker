#### functions to prepare data sets for financial analytics
####


## function to get price, quantity and price-quantity panel
##
##
##



## set global settings
source(paste0("global.R"))

## load functions for financials
source(paste0(path.src.data.financials,filename.src.get.data.financials),print=T,encoding="UTF-8")





get.quantity.panel <- function(){
  
  #### get full set of all quantity panels
  
  ## set global settings
  source(paste0("global.R"))
  
  ## load full history of transaction data
  df.transaction.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.transaction.history))
  
  ## convert to data frame
  df.transaction.history <- as.data.frame(df.transaction.history)
  
  ## convert transaction date into date type
  df.transaction.history$transaction_date <- as.Date(df.transaction.history$transaction_date,"%d-%m-%Y")
  
  ## ISIN to ticker converter
  df.isin.ticket.converter <- get.ticker.from.isin()
  
  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history,df.isin.ticket.converter,by="isin")
  
  ## all tickers
  tickers <- unique(df.transaction.history$ticker)
  
  ## delete all files in folder
  if(!(rlang::is_empty(list.files(path.data.processed.analytics.quantitypanel)))){
  file.remove(paste0(path.data.processed.analytics.quantitypanel,list.files(path.data.processed.analytics.quantitypanel)))}
  
  ## create quantity panels for all tickers
  output <- mapply(create.quantity.panel,tickers,MoreArgs = list(df.transaction.history))
  
} ## end of function get.quantity.panel




create.quantity.panel <- function(ticker,df.transaction.history){
  
  #### create quantity panel based on ticker and transaction history
  
  ## get transactions only for ticker
  df.transaction.history.ticker <- df.transaction.history[df.transaction.history$ticker == ticker,]
  
  ## keep only sale and purchase transaction types
  df.transaction.history.ticker <- df.transaction.history.ticker[grepl("^Sale$|^Purchase$",df.transaction.history.ticker$transaction_type),]
  
  if(nrow(df.transaction.history.ticker) > 0){
  
    ## if transaction type is a sale, quantity needs to be negative (in order to be substracted at a given point in time)
    df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type=="Sale"] <- -df.transaction.history.ticker$quantity[df.transaction.history.ticker$transaction_type=="Sale"]
    
    ## take cumulative sum of transactions to get quantity over time
    df.transaction.history.ticker <- df.transaction.history.ticker[,c("transaction_date","quantity")]
    df.transaction.history.ticker <- df.transaction.history.ticker[order(df.transaction.history.ticker$transaction_date),]
    df.transaction.history.ticker$cum_quantity <- cumsum(df.transaction.history.ticker$quantity)
    df.transaction.history.ticker <- df.transaction.history.ticker[,c("transaction_date","cum_quantity")]
    
    ## if there is a negative cumulative quantity, stop function because this must be an error (short selling not included)
    if(min(df.transaction.history.ticker$cum_quantity) >= 0){
    
    ## earliest transaction_date
    earliest.transaction.date <- min(df.transaction.history.ticker$transaction_date)
    
    ## first transaction for each ticker (to get panel of quantity for each ticker)
    df.transaction.history.ticker.first <- df.transaction.history.ticker[df.transaction.history.ticker$transaction_date == earliest.transaction.date,]
    
    ## create panel with date and quantity for ticker from transaction date until today (remove Saturday and Sunday)
    today <- Sys.Date()
    ## daily sequence from earliest transaction date until today
    dates <- seq(earliest.transaction.date,today,by=1)
    mysysgetlocale <- Sys.getlocale('LC_TIME')
    Sys.setlocale('LC_TIME','ENGLISH')
    dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
    Sys.setlocale('LC_TIME',mysysgetlocale)
    df.quantity.panel <- data.frame(date = dates)
    df.quantity.panel$ticker <- ticker
    
    ##
    data.table::setDT(df.quantity.panel)
    data.table::setDT(df.transaction.history.ticker)
    data.table::setkey(df.quantity.panel, "date")
    data.table::setkey(df.transaction.history.ticker, "transaction_date")
    DT.quantity.panel <- df.transaction.history.ticker[df.quantity.panel, roll = TRUE]
    df.quantity.panel <- data.table::setDF(DT.quantity.panel)
    names(df.quantity.panel)[names(df.quantity.panel)=="transaction_date"] <- "date"
    
    ## remove entries with zero cumulative quantity
    df.quantity.panel <- df.quantity.panel[df.quantity.panel$cum_quantity != 0,]
    
    ## start and end date
    from <- min(df.quantity.panel$date)
    to <- max(df.quantity.panel$date)
    
    ## file name
    filename.quantity.panel <- paste0("quantity_panel_",ticker,"_from_",from,"_to_",to,".csv")
    
    ## store quantity panel as csv
    data.table::fwrite(df.quantity.panel,paste0(path.data.processed.analytics.quantitypanel,filename.quantity.panel))
    
    print(paste("Quantity panel for",ticker,"successfully created!"))
    
    } else {print(paste0("Negative quantity for ",ticker,". Create quantity panel not possible."))} ## end of if else statement minimum quantity is positive
  
  } else(print("No purchases or sale transactions available.")) ## end of if else statement whether purchases or sales transactions are available
  
} ## end of function create.quantity.panel




get.price.panel <- function(){
  
  #### get panel of prices for each ticker
  
  ## load global
  source(paste0("global.R"))

  ## get tickers from history of transactions
  tickers <- get.tickers.from.transaction.history()
  
  ## delete all files in folder
  if(!(rlang::is_empty(list.files(path.data.processed.analytics.pricepanel)))){
    file.remove(paste0(path.data.processed.analytics.pricepanel,list.files(path.data.processed.analytics.pricepanel)))
  }
  
  ## list all files with financials
  if(!(rlang::is_empty(tickers))){
    for(i in 1:length(tickers)){
      
      ticker <- tickers[i]
      if(!(rlang::is_empty(list.files(paste0(path.data.raw.financials),pattern=ticker)))){
        df.financial.files <- data.frame(filenames = list.files(paste0(path.data.raw.financials),pattern=ticker))
        
        df.financial.files$filenames <- as.character(df.financial.files$filenames)
        
        ## load all financials (full time series for each ticker)
        df.all.financials.for.ticker <- lapply(df.financial.files$filenames,function(x)data.table::fread(paste0(path.data.raw.financials,x)))
        df.all.financials.for.ticker <- do.call(rbind,df.all.financials.for.ticker)
      
        
        ## start and end date
        from <- min(df.all.financials.for.ticker$date)
        to <- max(df.all.financials.for.ticker$date)
        
        ## file name
        filename.price.panel <- paste0("price_panel_",ticker,"_from_",from,"_to_",to,".csv")
        
        ## store price panel as csv
        data.table::fwrite(df.all.financials.for.ticker,paste0(path.data.processed.analytics.pricepanel,filename.price.panel))
        
        print("Price panel successfully created!")
      } else {print("No financials available!")}
    } ## end of for loop
  } else (return("No tickers for price panel available.")) ## end of if else statement 

} ## end of function get.price.panel




get.price.quantity.panels <- function(){
  
  #### get panels for prices times quantity for all tickers
  
  
  ## load global
  source(paste0("global.R"))
  
  ## get tickers from history of transactions
  tickers <- get.tickers.from.transaction.history()
  
  ## delete all files in folder
  if(!(rlang::is_empty(list.files(path.data.processed.analytics.pricequantitypanel)))){
  file.remove(paste0(path.data.processed.analytics.pricequantitypanel,list.files(path.data.processed.analytics.pricequantitypanel)))}
  
  ## get price quantity panel
  output <- lapply(tickers,get.price.quantity.panel)
  
} ## end of function get.price.quantity.panels



get.price.quantity.panel <- function(ticker){
  
  #### get price quantity panel for a ticker
  
  ## load price panel
  if(!(rlang::is_empty(list.files(paste0(path.data.processed.analytics.pricepanel),pattern=ticker)))){
  df.pricepanel <- data.table::fread(paste0(path.data.processed.analytics.pricepanel,list.files(paste0(path.data.processed.analytics.pricepanel),pattern=ticker)))
  
  ## load quantity panel
  if(!(rlang::is_empty(list.files(paste0(path.data.processed.analytics.quantitypanel),pattern=ticker)))){
    df.quantitypanel <- data.table::fread(paste0(path.data.processed.analytics.quantitypanel,list.files(paste0(path.data.processed.analytics.quantitypanel),pattern=ticker)))
    
    
    ## merge tables
    df.pricequantitypanel <- merge(df.pricepanel,df.quantitypanel,by="date")
    
    ## get value of investment at time t
    df.pricequantitypanel$value <- df.pricequantitypanel$adjusted * df.pricequantitypanel$cum_quantity
    
    ## start and end date
    from <- min(df.pricequantitypanel$date)
    to <- max(df.pricequantitypanel$date)
    
    ## file name
    filename.pricequantity.panel <- paste0("pricequantity_panel_",ticker,"_from_",from,"_to_",to,".csv")
    
    ## store price quantity panel as csv
    data.table::fwrite(df.pricequantitypanel,paste0(path.data.processed.analytics.pricequantitypanel,filename.pricequantity.panel))
    
    print("Price-quantity panel successfully created!")
    
  } else(print("No quantity panel available.")) ## end of if else statement to check whether quantity panel is available
  
  } else(print("No price panel available."))
  
} ## end of get.price.quantity.model


get.tickers.from.transaction.history <- function(){
  
  #### get all tickers from the history of all transactions
  
  ## load global
  source(paste0("global.R"))
  
  ## load full history of transaction data
  df.transaction.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.transaction.history))
  
  ## ISIN to ticker converter
  df.isin.ticket.converter <- get.ticker.from.isin()
  
  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history,df.isin.ticket.converter,by="isin")
  
  ## all tickers
  tickers <- unique(df.transaction.history$ticker)
  
  return(tickers)
  
} ## end of function get.tickers.from.transaction.history







