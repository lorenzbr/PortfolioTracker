# Helpers -----------------------------------------------------------------

get_tickers_from_transactions <- function(df.transaction.history, path) {

  get_names(path)

  isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

  if (isin.ticker.exists) {

    ## get table that converts ISIN to ticker
    df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))
    df.isin.ticker <- df.isin.ticker[df.isin.ticker$ticker != "", ]

    ## add ticker to transaction data
    df.transaction.history <- merge(df.transaction.history, df.isin.ticker, by = "isin")

    ## all tickers
    tickers <- unique(df.transaction.history$ticker)

    return(tickers)

  }

}
