#' Write previous portfolio investments to a csv file
#'
#' @usage write_previous_investments(path, file.name = "previous_investments.csv",
#'                       file.tickers = "isin_ticker.csv",
#'                       file.transactions = "transaction_fullhistory.csv")
#' @param path A single character string. Directory of your data.
#' @param file.name A single character string. Name of created csv file containing current portfolio.
#' @param file.tickers A single character string. Name of csv containing ISIN-ticker pairs.
#' @param file.transactions A single character string. Name of csv file containing transactions.
#'
#' @export
write_previous_investments <- function(path, file.name = "previous_investments.csv",
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

    ## all price-quantity panels in one data frame
    df.all <- do.call(rbind, list.dfs)

    ## keep latest date for each ticker
    df.previous <- stats::aggregate(date ~ ticker, data = df.all, max)

    ## add details
    df.previous <- merge(df.all, df.previous, by = c("ticker", "date"))

    ## keep investments with quantity greater zero
    df.previous <- df.previous[df.previous$cum_quantity == 0, ]

    df.previous <- unique(df.previous)

    ## add name and ISIN
    df.previous <- merge(df.previous, df.ticker.investmentnames, by = "ticker")
    df.previous <- merge(df.previous, df.isin.ticker, by = "ticker")

    df.previous <- df.previous[, c("name", "isin", "ticker", "adjusted", "cum_quantity", "value")]

    data.table::fwrite(df.previous, paste0(path.data, file.name))

    ## to do use df.transaction.history and Purchases and Sales to get Realized return [EUR] and [%]
    ## to this for each ticker which has at least one sales transaction
    # ...

  } else { message("No price quantity panels available.") } ## end of if statement

} # end of function write_previous_investments
