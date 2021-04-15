#### FUNCTIONS TO GET PORTFOLIO STATISTICS
####


## functions to get statistics for portfolio
## gains, losses, amount invested, portfolio value
##
##
#' Get total dividend payments
#'
#' @usage get_dividends_max(path, file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Directory where all data are stored.
#' @param file.dividend.history A single character string. Name of csv containing full history of dividends.
#'
#' @export
get_dividends_max <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  #### get total dividends

  ## create folder if not exists and get folder name for price panel
  list.paths <- PortfolioTracker::create_portfoliotracker_dir(path)
  path.dividends <- list.paths$path.dividends

  ## load dividend history
  df.dividend.history <- data.table::fread(paste0(path.dividends, file.dividend.history))

  ## compute max dividends
  dividends.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Dividend"])
  dividends.storno.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"])
  dividends.max <- dividends.max - dividends.storno.max

  return(dividends.max)

} ## end of function get_dividends_max



# ## total investment all time and until time t
# amount.invested.alltime.purchase <- sum(df.transaction.history$transaction_value[grepl("^Purchase$",
#                                                                                        df.transaction.history$transaction_type)])
# amount.invested.alltime.sale <- sum(df.transaction.history$transaction_value[grepl("^Sale$",
#                                                                                    df.transaction.history$transaction_type)])
# amount.invested.max <- amount.invested.alltime.purchase - amount.invested.alltime.sale
# ## until time t
# #...

