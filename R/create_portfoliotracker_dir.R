#' Create directory for portfoliotracker
#'
#' @usage create_portfoliotracker_dir(path)
#' @param path A single character string. Folder where all data are stored.
#' @return create_portfoliotracker_dir returns a list with path names for data storage
#'
#' @export
create_portfoliotracker_dir <- function(path){

  #### create directory for portfoliotracker

  ## create folders for tickers and prices (if not exists)
  path.tickers <- paste0(path, "data/tickers/")
  path.prices.raw <- paste0(path, "data/prices_raw/")
  path.quantitypanel <- paste0(path, "data/quantity_panel/")
  path.pricepanel <- paste0(path, "data/price_panel/")
  path.pricequantitypanel <- paste0(path, "data/price_quantity_panel/")
  path.dividends <- paste0(path, "data/dividends/")
  path.returns <- paste0(path, "data/returns/")

  if (!dir.exists(path.tickers)) dir.create(path.tickers, recursive = TRUE)
  if (!dir.exists(path.prices.raw)) dir.create(path.prices.raw, recursive = TRUE)
  if (!dir.exists(path.quantitypanel)) dir.create(path.quantitypanel, recursive = TRUE)
  if (!dir.exists(path.pricepanel)) dir.create(path.pricepanel, recursive = TRUE)
  if (!dir.exists(path.pricequantitypanel)) dir.create(path.pricequantitypanel, recursive = TRUE)
  if (!dir.exists(path.dividends)) dir.create(path.dividends, recursive = TRUE)
  if (!dir.exists(path.returns)) dir.create(path.returns, recursive = TRUE)

  output.list <- list(
    path.tickers = path.tickers,
    path.prices.raw = path.prices.raw,
    path.quantitypanel = path.quantitypanel,
    path.pricepanel = path.pricepanel,
    path.pricequantitypanel = path.pricequantitypanel,
    path.dividends = path.dividends,
    path.returns = path.returns
  )

  return(output.list)

} ## end of function create_portfoliotracker_dir
