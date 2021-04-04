#### FUNCTIONS TO CREATE FINAL TABLES WITH PERFORMANCE AND SO ON
####


## functions to get final tables for performance, value over time, annual returns for each ticker, and all together
##
##
##



## set global settings
source(paste0("global.R"))

## load functions to get financials (to get function get.ticker.from.isin)
source(paste0(path.src.data.financials,filename.src.get.data.financials))




## run function to get transaction history
df.transaction.history <- get.transaction.history.table()


## get tickers from ISIN
df.isin.ticker.converter <- get.ticker.from.isin()

## keep ISIN and name
df.ticker.investmentnames <- unique(df.transaction.history[,c("isin","name")])

## add tickers
df.ticker.investmentnames <- merge(df.ticker.investmentnames,df.isin.ticker.converter,by="isin")
df.ticker.investmentnames <- unique(df.ticker.investmentnames[,c("ticker","name")])





## load price-quantity panel
if(!(rlang::is_empty(list.files(paste0(path.data.processed.analytics.pricequantitypanel))))){
  filenames <- paste0(path.data.processed.analytics.pricequantitypanel,list.files(paste0(path.data.processed.analytics.pricequantitypanel)))
  list.dfs <- lapply(filenames,data.table::fread)
  
  ## for loop over all price-quantity panel dfs in list. store them in separate dfs
  for(i in 1:length(list.dfs)){
    assign(paste0("df.pricequantity.panel",i),list.dfs[[i]])
    df.temp <- get(paste0("df.pricequantity.panel",i))
    df.temp$date <- as.Date(df.temp$date,"%Y-%m-%d")
    assign(paste0("df.pricequantity.panel",i),df.temp)
  } ## end of for loop
  rm(df.temp)
} ## end of if statement price-quantity panel is empty


## compute performance in %
# df.pricequantity.panel.temp <- df.pricequantity.panel1
# df.pricequantity.panel.temp <- df.pricequantity.panel.temp[order(df.pricequantity.panel.temp$date),]



# get.current.portfolio.list <- function(df.all){
  
  #### create table with current portfolio
if(exists("list.dfs")){
  ## get most recent entry in each price-quantity panel
  df.all <- do.call(rbind,list.dfs)
  
  ## keep latest date for each ticker
  df.all2 <- aggregate(date ~ ticker, data = df.all, max)
  
  ## add details
  df.all <- merge(df.all,df.all2,by=c("ticker","date"))
  
  ## keep investments with quantity greater zero
  df.all <- df.all[df.all$cum_quantity > 0,]
  
  df.all <- unique(df.all)
  rm(df.all2)
  
  ## add name and ISIN
  df.all <- merge(df.all,df.ticker.investmentnames,by="ticker")
  df.all <- merge(df.all,df.isin.ticker.converter,by="ticker")
  
  df.all <- df.all[,c("name","isin","ticker","adjusted","cum_quantity","value")]
  
  ## total portfolio and until time t
  total.portfolio.value <- sum(df.all$value)
  # ...
  
  ## compute weight of each investment in total portfolio value
  df.all$weight <- df.all$value / total.portfolio.value
  
} else {
  names.all <- c("name","isin","ticker","adjusted","cum_quantity","value", "weight")
  df.all <- data.frame(matrix(nrow=0,ncol=length(names.all),dimnames=list(NULL,names.all)))
} ## end of if else statement


##
if(nrow(df.all) > 0){

  df.investments <- df.all
  df.investments$weight <- df.investments$weight * 100
  df.investments$weight <- as.numeric(formatC(df.investments$weight,digits=2,format="f"))
  df.investments$adjusted <- as.numeric(formatC(df.investments$adjusted,digits=2,format="f"))

  # tickers <- unique(df.all$ticker)
  # df.all[df.all$ticker == tickers[1]]
  
  # } ## end of function get.current.portfolio.list
  
  ## total price gains and until time t
  price.gains.max <- total.portfolio.value - amount.invested.max
  # ...

} else {
  total.portfolio.value <- 0
  price.gains.max <- 0
  names.investments <- names(df.all)
  df.investments <- data.frame(matrix(nrow=0,ncol=length(names.investments),dimnames=list(NULL,names.investments)))
} ## end of if else statement df exists


## changes names of df.investments
names(df.investments) <- c("Name","ISIN","Ticker","Price [EUR]","Quantity","Value [EUR]", "Weight [%]")









