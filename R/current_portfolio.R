#' Write current portfolio investments to a csv file
#'
#' @usage write_current_portfolio(path, file.name = "current_portfolio.csv",
#'                       file.tickers = "isin_ticker.csv",
#'                       file.transactions = "transaction_fullhistory.csv")
#' @param path A single character string. Directory of your data.
#' @param file.name A single character string. Name of created csv file containing current portfolio.
#' @param file.tickers A single character string. Name of csv containing ISIN-ticker pairs.
#' @param file.transactions A single character string. Name of csv file containing transactions.
#'
#' @export
write_current_portfolio <- function(path, file.name = "current_portfolio.csv",
                                    file.tickers = "isin_ticker.csv",
                                    file.transactions = "transaction_fullhistory.csv"){

  ## create folder if not exists and get folder name for quantity panel and tickers
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.pricequantitypanel <- list.paths$path.pricequantitypanel
  path.tickers <- list.paths$path.tickers
  path.transactions <- list.paths$path.transactions
  path.data <- list.paths$path.data

  ## load price quantity panels if exists
  if (!rlang::is_empty(list.files(path.pricequantitypanel))) {

    filenames <- paste0(path.pricequantitypanel, list.files(path.pricequantitypanel))
    list.dfs <- lapply(filenames, data.table::fread)

    ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
    df.isin.ticker <- data.table::fread(paste0(path.tickers, file.tickers))

    ## keep ISIN and name
    df.transaction.history <- data.table::fread(paste0(path.transactions, file.transactions))
    df.ticker.investmentnames <- unique(df.transaction.history[, c("isin", "name")])

    ## add tickers
    df.ticker.investmentnames <- merge(df.ticker.investmentnames, df.isin.ticker, by = "isin")
    df.ticker.investmentnames <- unique(df.ticker.investmentnames[, c("ticker", "name")])

    ## get most recent entry in each price-quantity panel
    df.all <- do.call(rbind, list.dfs)

    ## keep latest date for each ticker
    df.current <- stats::aggregate(date ~ ticker, data = df.all, max)

    ## add details
    df.current <- merge(df.all, df.current, by = c("ticker", "date"))

    ## keep investments with quantity greater zero
    df.current <- df.current[df.current$cum_quantity > 0, ]

    df.current <- unique(df.current)

    ## add name and ISIN
    df.current <- merge(df.current, df.ticker.investmentnames, by = "ticker")
    df.current <- merge(df.current, df.isin.ticker, by = "ticker")

    df.current <- df.current[, c("name", "isin", "ticker", "adjusted", "cum_quantity", "value")]

    ## current total portfolio value
    total.portfolio.value <- sum(df.current$value)

    ## compute weight of each investment in total portfolio value
    df.current$weight <- df.current$value / total.portfolio.value

    data.table::fwrite(df.current, paste0(path.data, file.name))

  } else { message("No price quantity panels available.") } ## end of if statement

} # end of function write_current_portfolio
