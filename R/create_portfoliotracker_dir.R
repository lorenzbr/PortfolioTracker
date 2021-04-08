#' Create directory for portfoliotracker
#'
#' @usage create_portfoliotracker_dir(path)
#' @param path A single character string. Folder where all data are stored.
#'
#' @export
create_portfoliotracker_dir <- function(path){

  #### create directory for portfoliotracker

  ## create folders for tickers and prices (if not exists)
  path.tickers <- paste0(path, "data/tickers/")
  path.prices.raw <- paste0(path, "data/prices_raw/")
  if (!dir.exists(path.tickers)) dir.create(path.tickers, recursive = TRUE)
  if (!dir.exists(path.prices.raw)) dir.create(path.prices.raw, recursive = TRUE)

  output.list <- list(
    path.tickers = path.tickers,
    path.prices.raw = path.prices.raw
  )

  return(output.list)

} ## end of function create_portfoliotracker_dir
