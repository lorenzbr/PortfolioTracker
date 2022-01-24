#' Create ISIN-ticker table as csv if no such file exists
#'
#' @usage init_isin_ticker(path, file = "isin_ticker.csv")
#' @param path A single character string. Path where data are stored.
#' @param file A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv)
#'
#' @export
init_isin_ticker <- function(path, file = "isin_ticker.csv"){

  if ( !file.exists(file.path(path, file)) ) {
    col.names <- c("isin", "ticker")
    df.isin.ticker.init <- data.frame(matrix(NA, nrow = 0, ncol = length(col.names),
                                             dimnames = list(NULL, col.names)))
    data.table::fwrite(df.isin.ticker.init, file.path(path, file))
  }

}

#' Update ISIN-ticker table
#'
#' @usage update_ticker_isin(isins, path.tickers, file.ticker = "isin_ticker.csv",
#'                    external.search = TRUE)
#' @param isins A single character or vector of strings. ISINs.
#' @param path.tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv)
#' @param external.search Logical if TRUE, the function searches external sources to find the ticker.
#'
#' @export
update_ticker_isin <- function(isins, path.tickers, file.ticker = "isin_ticker.csv",
                               external.search = TRUE){

  ## Variable isins is a vector containing ISINs for which tickers are needed
  isins <- unique(isins)
  ## Remove empty entries
  isins <- isins[!grepl("^$", isins)]

  ## Create csv if not exists
  init_isin_ticker(path.tickers, file.ticker)

  ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(file.path(path.tickers, file.ticker))

  ## Identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
  new.isins <- isins[!(isins %in% unique(df.isin.ticker$isin))]

  if ( length(new.isins) > 0 ) {

    for ( i in 1:length(new.isins) ) {

      isin <- new.isins[i]

      try({

        ##  WiP, better to include function argument with whatever data is collected
        df.isin.ticker.database <- data.table::fread(system.file("extdata", "isin_ticker_name_list_xetra_june2017.csv",
                                                                 package = "PortfolioTracker"))
        df.isin.ticker.database <- as.data.frame(df.isin.ticker.database)
        ticker <- df.isin.ticker.database$ticker[df.isin.ticker.database$isin == isin]

        if ( length(ticker) == 0 && external.search ) ticker <- get_ticker_from_xetra(isin)



        if ( ticker != "" ) {

          df.isin.ticker.new <- data.frame(isin = isin, ticker = ticker)

          data.table::fwrite(df.isin.ticker.new, file.path(path.tickers,
                                                           file.ticker),
                             append = TRUE)

          # print(paste("Ticker was missing.", ticker, "added."))

        } else {

          # message("Ticker not found! Please add manually!")

        }

      })

    }

  } else {

    # print("New transactions, but ticker already available.")

  }

}

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
  html.output.div <- rvest::html_nodes(html.output,
                                       'div.js-inner-all-results-quotes-wrapper')
  html.output.div <- rvest::html_nodes(html.output.div, 'a')
  html.output.text <- rvest::html_text(html.output.div)
  html.output.text.selected <- html.output.text[grepl(preferred.stock.exchange,
                                                      html.output.text)]
  if( rlang::is_empty(html.output.text.selected) ) {
    ## if preferred stock exchange does not exist, select random one
    html.output.text.selected <- html.output.text[sample(length(html.output.text), 1)]
  }
  html.output.text.selected <- gsub("\t", "", html.output.text.selected)
  html.output.text.selected <- gsub("^(\n)+|(\n)+$", "", html.output.text.selected)

  if ( !(rlang::is_empty(html.output.text.selected)) ) {
    ticker <- strsplit(html.output.text.selected,"\n")[[1]][1]
    if ( preferred.stock.exchange == "Xetra"
         && grepl(preferred.stock.exchange, html.output.text.selected) ) {
      ticker <- paste0(ticker, ".DE")}
  }

  return(ticker)

}


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

  if ( !(rlang::is_empty(url2)) ) {

    if ( is.na(url2) ) {
      html.output.ol <- rvest::html_nodes(html.output.ol, "a")
      url2 <- rvest::html_attr(html.output.ol, 'href')
    }

    # if ( is.na(url2) ) message(paste("URL for", isin, "not found! Please add ticker manually."))

  } else {

    stop( paste("URL for", isin, "not found! Please add ticker manually.") )

  }

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

}


#' Add ISIN-ticker pairs to table
#'
#' @usage add_ticker_manually(df.isin.ticker.new, path.tickers, file.ticker = "isin_ticker.csv")
#' @param df.isin.ticker.new A data frame containing a column for isin and ticker.
#' @param path.tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (Default: isin_ticker.csv).
#'
#' @export
add_ticker_manually <- function(df.isin.ticker.new, path.tickers,
                                file.ticker = "isin_ticker.csv"){

  isins <- unique(df.isin.ticker.new$isin)
  isins <- isins[!grepl("^$", isins)]

  ## create csv if not exists
  init_isin_ticker(path.tickers, file.ticker)

  ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df.isin.ticker <- data.table::fread(file.path(path.tickers, file.ticker))

  ## identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
  new.isins <- isins[!(isins %in% unique(df.isin.ticker$isin))]

  if ( !rlang::is_empty(new.isins) ) {

    for ( i in 1:length(new.isins) ) {

      isin <- new.isins[i]

      try({

        df.isin.ticker.add <- df.isin.ticker.new[grepl(isin, df.isin.ticker.new$isin), ]

        data.table::fwrite(df.isin.ticker.add,
                           file.path(path.tickers, file.ticker), append = TRUE)

        # message("Ticker was missing. ", df.isin.ticker.add$ticker, " added.")

      })

    }

  } else {

    # message("Ticker already available.")

  } ## End of if statement ISIN not in table is empty

}

