#### FUNCTIONS TO GET PORTFOLIO STATISTICS
####


## functions to get statistics for portfolio
## gains, losses, amount invested, portfolio value
##
##



## set global settings
source(paste0("global.R"))

## load functions to get financials (to get function get.ticker.from.isin)
source(paste0(path.src.data.financials,filename.src.get.data.financials))



get.dividends.max <- function(){
  
  #### get total dividends
  
  ## load dividend history
  df.dividend.history <- data.table::fread(paste0(path.data.processed.transactions,filename.data.dividend.history))
  
  ## compute max dividends
  dividends.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Dividend"])
  dividends.storno.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"])
  dividends.max <- dividends.max - dividends.storno.max
  return(dividends.max)
  
} ## end of function get.dividends.max



## total investment all time and until time t
amount.invested.alltime.purchase <- sum(df.transaction.history$transaction_value[grepl("^Purchase$",df.transaction.history$transaction_type)])
amount.invested.alltime.sale <- sum(df.transaction.history$transaction_value[grepl("^Sale$",df.transaction.history$transaction_type)])
amount.invested.max <- amount.invested.alltime.purchase - amount.invested.alltime.sale
## until time t
#...

