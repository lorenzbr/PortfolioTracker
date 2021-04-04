#### FUNCTIONS FOR DIVIDENDS and DIVIDEND YIELDS
####


## functions to get information on dividends and dividend yields
##
##
##


## set global settings
source(paste0("global.R"))



get.dividend.history <- function(){
  
  #### get dividend history
  
  ## set global settings
  source(paste0("global.R"))
  
  ## load transaction history
  df.transaction.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.transaction.history))
  
  ## get list of dividends
  df.dividend.history <- df.transaction.history[grepl("Dividend",df.transaction.history$transaction_type),]
  
  if(nrow(df.dividend.history) > 0){
    ## change to absolute values
    df.dividend.history$transaction_value <- abs(df.dividend.history$transaction_value)
  
    ## write dividend history
    data.table::fwrite(df.dividend.history,paste0(path.data.processed.transactions,filename.data.dividend.history))
  
  } else {
    print("No history of dividends.")
  } ## end of if else statement whether dividends exist
  
  # return(df.dividend.history)
  
} ## end of function get dfs for dividends



get.dividend.by.yr <- function(){
  
  #### get dividend by year from first dividend payment until today
  
  tryCatch({
  ## load dividend history
  df.dividend.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.dividend.history))
  
  ## storno needs to be a negative amount (i.e. payment)
  df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"] <- -df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"]
  
  ## get year
  df.dividend.history$year <- lubridate::year(df.dividend.history$transaction_date)
  
  ## get dividends by year
  df.dividend.history.sum.yr <- aggregate(transaction_value ~ year, data = df.dividend.history, sum)
  df.dividend.history.sum.yr <- df.dividend.history.sum.yr %>%
    dplyr::mutate(year = year) %>%
    tidyr::complete(year = seq(min(year), as.numeric(lubridate::year(Sys.Date())), by=1))
  df.dividend.history.sum.yr <- as.data.frame(df.dividend.history.sum.yr)
  df.dividend.history.sum.yr$transaction_value[is.na(df.dividend.history.sum.yr$transaction_value)] <- 0
  
  ## write dividends by year
  data.table::fwrite(df.dividend.history.sum.yr,paste0(path.data.processed.transactions,filename.data.dividend.yr))
  
  },
  error = function(e){
   message(e) 
  }) ## end of trycatch statement
  
} ## end of function get.dividend.by.yr

get.dividend.by.month <- function(){
  
  #### get dividend payments by month from first payment until today
  
  tryCatch({
  
  ## load dividend history
  df.dividend.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.dividend.history))
  
  ## storno needs to be a negative amount (i.e. payment)
  df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"] <- -df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"]
  
  ## get year-month
  df.dividend.history$yearmon <- lubridate::floor_date(df.dividend.history$transaction_date,unit="month")
  
  ## get dividends by month
  df.dividend.history.sum.month <- aggregate(transaction_value ~ yearmon, data = df.dividend.history, sum)
  df.dividend.history.sum.month <- df.dividend.history.sum.month %>%
    dplyr::mutate(yearmon = as.Date(yearmon)) %>%
    tidyr::complete(yearmon = seq.Date(min(yearmon), lubridate::floor_date(Sys.Date(),unit="month"), by="month"))
  df.dividend.history.sum.month <- as.data.frame(df.dividend.history.sum.month)
  df.dividend.history.sum.month$transaction_value[is.na(df.dividend.history.sum.month$transaction_value)] <- 0
  
  ## write dividends by year
  data.table::fwrite(df.dividend.history.sum.month,paste0(path.data.processed.transactions,filename.data.dividend.month))
  
  },
  error = function(e){
    message(e) 
  }) ## end of trycatch statement
  
} ## end of function get.dividend.by.month







