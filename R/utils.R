# Helpers -----------------------------------------------------------------

get_tickers_from_transactions <- function(df.transaction.history, path) {

  get_names(path)

  ## get table that converts ISIN to ticker
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

  ## add ticker to transaction data
  df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

  ## all tickers
  tickers <- unique(df.transaction.history$ticker)

  return(tickers)

}
