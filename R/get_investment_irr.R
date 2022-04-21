#' Get investment IRR for all tickers
#'
#' @usage get_investment_irr(path)
#' @param path A single character string. Directory of the data.
#'
#' @return A data frame containing IRRs for all tickers
#'
#' @noRd
get_investment_irr <- function(path) {

  PortfolioTracker::get_user_names(path)

  if (file.exists(file.path(path.returns, file.returns.irr))) {

    df_returns <- data.table::fread(file.path(path.returns, file.returns.irr))

    df_returns <- PortfolioTracker::format_return_tables(path, df_returns)

  } else {

    col_names <- c("Ticker", "ISIN", "Name", "1Y", "3Y", "5Y", "10Y", "Max")
    df_returns <- as.data.frame(matrix(nrow = 0, ncol = length(col_names),
                                       dimnames = list(NULL, col_names)))

  }

  return(df_returns)

}
