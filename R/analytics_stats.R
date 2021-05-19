#' Write portfolio statistics
#'
#' @usage write_portfolio_stats(path)
#' @param path A single character string. Directory where all data are stored.
#'
#' @export
write_portfolio_stats <- function(path) {

  list.names <- get_names(path)
  path.returns <- list.names$path.returns
  path.data <- list.names$path.data
  path.transactions <- list.names$path.transactions
  file.stats <- list.names$file.stats

  df.current <- data.table::fread(paste0(path.data, "current_portfolio.csv"))
  df.transaction.history <- data.table::fread(paste0(path.transactions, "transaction_fullhistory.csv"))

  ## current total portfolio value
  total.portfolio.value <- sum(df.current$value)

  ## total investment all time and until time t
  amount.invested.alltime.purchase <- sum(df.transaction.history$transaction_value[grepl("^Purchase$",
                                                                                         df.transaction.history$transaction_type)])
  amount.invested.alltime.sale <- sum(df.transaction.history$transaction_value[grepl("^Sale$",
                                                                                     df.transaction.history$transaction_type)])
  amount.invested.max <- amount.invested.alltime.purchase - amount.invested.alltime.sale

  ## total price gains and until time t
  price.gains.max <- total.portfolio.value - amount.invested.max

  dividends.max <- get_dividends_max(path)

  df.stats <- data.frame(info = c("total_portfolio_value","amount_invested_max", "price_gains_max", "dividends_max"),
             max = c(total.portfolio.value, amount.invested.max, price.gains.max, dividends.max))

  data.table::fwrite(df.stats, paste0(path.data, file.stats))

}


#' Get total dividend payments
#'
#' @usage get_dividends_max(path, file.dividend.history = "dividends_fullhistory.csv")
#' @param path A single character string. Directory where all data are stored.
#' @param file.dividend.history A single character string. Name of csv containing full history of dividends.
#'
#' @export
get_dividends_max <- function(path, file.dividend.history = "dividends_fullhistory.csv") {

  list.names <- get_names(path)
  path.dividends <- list.names$path.dividends

  ## load dividend history
  df.dividend.history <- data.table::fread(paste0(path.dividends, file.dividend.history))

  ## compute max dividends
  dividends.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Dividend"])
  dividends.storno.max <- sum(df.dividend.history$transaction_value[df.dividend.history$transaction_type == "Storno - Dividend"])
  dividends.max <- dividends.max - dividends.storno.max

  return(dividends.max)

}
