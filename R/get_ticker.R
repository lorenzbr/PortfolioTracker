#' Create ISIN-ticker table as csv if no such file exists
#'
#' @usage init_isin_ticker(path, file = "isin_ticker.csv")
#' @param path A single character string. Path where data are stored.
#' @param file A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv)
#'
#' @export
init_isin_ticker <- function(path, file = "isin_ticker.csv"){

  if(!(file.exists(paste0(path, file)))){
    df.isin.ticker.init <- data.frame(matrix(NA, nrow = 0, ncol = 2, dimnames = list(NULL, c("isin", "ticker"))))
    data.table::fwrite(df.isin.ticker.init, paste0(path, file))
  }

} ## end of function init_isin_ticker

#' Update ISIN-ticker table
#'
#' @usage update_ticker_isin(isins, path.tickers, file.ticker = "isin_ticker.csv")
#' @param isins A single character or vector of strings. ISINs.
#' @param path.tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv)
#'
#' @export
update_ticker_isin <- function(isins, path.tickers, file.ticker = "isin_ticker.csv"){

  isins <- unique(isins)
  isins <- isins[!grepl("^$", isins)]

  ## create csv if not exists
  init_isin_ticker(path.tickers, file.ticker)

  ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))

  ## identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
  new.isins <- isins[!(isins %in% unique(df.isin.ticker$isin))]

  if (!rlang::is_empty(new.isins)) {

    for (i in 1:length(new.isins)) {

      isin <- new.isins[i]

      try({

        ticker <- get_ticker_from_xetra(isin)

        df.isin.ticker.new <- data.frame(isin = isin, ticker = ticker)

        data.table::fwrite(df.isin.ticker.new, paste0(path.tickers, file.ticker), append = TRUE)

        print(paste("Ticker was missing.", ticker, "added."))

      })

    } ## end of for loop

  } else (print("New transactions, but ticker already available.")) ## end of if statement ISIN not in table is empty

} ## end of function update_ticker_isin

#' Get ticker based on ISIN by crawling investing.com
#'
#' @usage get_ticker_from_investing(isin, preferred.stock.exchange = "")
#' @param isin A single character string. ISIN.
#' @param preferred.stock.exchange A single character string. Stock exchange (e.g., Xetra)
#'
#' @export
get_ticker_from_investing <- function(isin, preferred.stock.exchange = ""){

  url <- "https://www.investing.com/search/?q="
  url.isin <- paste0(url, isin)

  html.output <- rvest::read_html(url.isin)
  html.output.div <- rvest::html_nodes(html.output, 'div.js-inner-all-results-quotes-wrapper')
  html.output.div <- rvest::html_nodes(html.output.div, 'a')
  html.output.text <- rvest::html_text(html.output.div)
  html.output.text.selected <- html.output.text[grepl(preferred.stock.exchange, html.output.text)]
  if(rlang::is_empty(html.output.text.selected)){
    ## if preferred stock exchange does not exist, select random one
    html.output.text.selected <- html.output.text[sample(length(html.output.text),1)]
  }
  html.output.text.selected <- gsub("\t","", html.output.text.selected)
  html.output.text.selected <- gsub("^(\n)+|(\n)+$","", html.output.text.selected)

  if (!(rlang::is_empty(html.output.text.selected))) {
    ticker <- strsplit(html.output.text.selected,"\n")[[1]][1]
    if (preferred.stock.exchange == "Xetra" & grepl(preferred.stock.exchange, html.output.text.selected)) {
      ticker <- paste0(ticker,".DE")}
  }

  return(ticker)

} ## end of function get_ticker_from_investing


#' Get ticker based on ISIN by crawling the Xetra website
#'
#' @usage get_ticker_from_xetra(isin, preferred.stock.exchange = "")
#' @param isin A single character string. ISIN.
#' @param preferred.stock.exchange A single character string. Stock exchange (default is empty string)
#'
#' @export
get_ticker_from_xetra <- function(isin, preferred.stock.exchange = ""){

  url <- "https://www.xetra.com/xetra-de/instrumente/alle-handelbaren-instrumente/boersefrankfurt/3906!search?query="
  url.isin <- paste0(url, isin)

  html.output <- rvest::read_html(url.isin)
  html.output.ol <- rvest::html_nodes(html.output, 'ol.list')
  url2 <- rvest::html_attr(html.output.ol, 'href')
  if (!(rlang::is_empty(url2))) {
    if (is.na(url2)) {
      html.output.ol <- rvest::html_nodes(html.output.ol, "a")
      url2 <- rvest::html_attr(html.output.ol, 'href')
    }
    if (is.na(url2)) message(paste("URL for", isin, "not found! Please add ticker manually."))
  } else {stop(paste("URL for", isin, "not found! Please add ticker manually."))}
  url2 <- paste0("https://www.xetra.com", url2)

  html.output <- rvest::read_html(url2)
  html.output.dl <- rvest::html_nodes(html.output, 'dl.list')

  names <- rvest::html_elements(html.output.dl, "dt")
  elements <- rvest::html_elements(html.output.dl, "dd")

  names <- rvest::html_text(names)
  elements <- rvest::html_text(elements)

  names <- gsub("\t|\n","", names)

  ticker <- elements[grep("K.?rzel", names)]

  if(preferred.stock.exchange == "Xetra"){ticker <- paste0(ticker, ".DE")}

  return(ticker)

} ## end of function get_ticker_from_xetra


#' Add ISIN-ticker pairs to table
#'
#' @usage add_ticker_manually(df.isin.ticker.new, path.tickers, file.ticker = "isin_ticker.csv")
#' @param df.isin.ticker.new A data frame containing a column for isin and ticker.
#' @param path.tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv).
#'
#' @export
add_ticker_manually <- function(df.isin.ticker.new, path.tickers, file.ticker = "isin_ticker.csv"){

  isins <- unique(df.isin.ticker.new$isin)
  isins <- isins[!grepl("^$", isins)]

  ## create csv if not exists
  init_isin_ticker(path.tickers, file.ticker)

  ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(paste0(path.tickers, file.ticker))

  ## identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
  new.isins <- isins[!(isins %in% unique(df.isin.ticker$isin))]

  if (!rlang::is_empty(new.isins)) {

    for (i in 1:length(new.isins)) {

      isin <- new.isins[i]

      try({

        df.isin.ticker.add <- df.isin.ticker.new[grepl(isin, df.isin.ticker.new$isin), ]

        data.table::fwrite(df.isin.ticker.add, paste0(path.tickers, file.ticker), append = TRUE)

        message("Ticker was missing. ", df.isin.ticker.add$ticker, " added.")

      })

    } ## end of for loop

  } else { message("Ticker already available.") } ## end of if statement ISIN not in table is empty

} ## end of function add_ticker_manually

