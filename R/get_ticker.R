#' Create ISIN-ticker table as csv if no such file exists
#'
#' @usage init_isin_ticker(path, file = "isin_ticker.csv")
#' @param path A single character string. Path where data are stored.
#' @param file A single character string. Name of ISIN-ticker csv file (default: isin_ticker.csv)
#'
#' @export
init_isin_ticker <- function(path, file = "isin_ticker.csv") {

  if (!file.exists(file.path(path, file))) {
    col_names <- c("isin", "ticker")
    df_isin_ticker_init <- data.frame(matrix(NA, nrow = 0, ncol = length(col_names),
                                             dimnames = list(NULL, col_names)))
    data.table::fwrite(df_isin_ticker_init, file.path(path, file))
  }

}

#' Update ISIN-ticker table
#'
#' @usage update_ticker_isin(isins, path_tickers, file_tickers, external_search = TRUE)
#' @param isins A single character or vector of strings. ISINs.
#' @param path_tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file_tickers A single character string. Name of ISIN-ticker csv file
#' @param external_search Logical if TRUE, the function searches external sources to find the ticker.
#'
#' @export
update_ticker_isin <- function(isins, path_tickers, file_tickers,
                               external_search = TRUE) {

  isins <- unique(isins)

  isins <- isins[!grepl("^$", isins)]

  init_isin_ticker(path_tickers, file_tickers)

  ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df_isin_ticker <- data.table::fread(file.path(path_tickers, file_tickers))

  new_isins <- isins[!(isins %in% unique(df_isin_ticker$isin))]

  if (length(new_isins) > 0) {

    for (i in 1:length(new_isins)) {

      isin <- new_isins[i]

      try({

        ##  WiP, better to include function argument with whatever data is collected
        df_isin_ticker_database <- data.table::fread(
          system.file("extdata", "isin_ticker_name_list_xetra_june2017.csv",
                      package = "PortfolioTracker"))
        df_isin_ticker_database <- as.data.frame(df_isin_ticker_database)
        ticker <- df_isin_ticker_database$ticker[df_isin_ticker_database$isin == isin]

        if (length(ticker) == 0 && external_search)
          ticker <- get_ticker_from_xetra(isin)

        if (ticker != "") {

          df_isin_ticker_new <- data.frame(isin = isin, ticker = ticker)

          data.table::fwrite(df_isin_ticker_new,
                             file.path(path_tickers, file_tickers),
                             append = TRUE)

        }

      })

    }

  }

}

#' Get ticker based on ISIN by crawling investing.com
#'
#' @usage get_ticker_from_investing(isin, preferred_exchange = "")
#' @param isin A single character string. ISIN.
#' @param preferred_exchange A single character string. Stock exchange (e.g., Xetra)
#'
#' @export
get_ticker_from_investing <- function(isin, preferred_exchange = "") {

  url <- "https://www.investing.com/search/?q="
  url_isin <- paste0(url, isin)

  html_output <- rvest::read_html(url_isin)
  html_output_div <- rvest::html_nodes(html_output,
                                       'div.js-inner-all-results-quotes-wrapper')
  html_output_div <- rvest::html_nodes(html_output_div, 'a')
  html_output_text <- rvest::html_text(html_output_div)
  ## If no regex is needed, grepl is faster with fixed = TRUE
  html_output_text_selected <- html_output_text[grepl(preferred_exchange,
                                                      html_output_text,
                                                      fixed = TRUE)]
  # html_output_text_selected <- html_output_text[grepl(preferred_exchange,
  #                                                     html_output_text)]
  if (length(html_output_text_selected) == 0) {
    ## If preferred stock exchange does not exist, select random one
    html_output_text_selected <- html_output_text[sample(length(html_output_text), 1)]
  }
  html_output_text_selected <- gsub("\t", "", html_output_text_selected)
  html_output_text_selected <- gsub("^(\n)+|(\n)+$", "", html_output_text_selected)

  if (length(html_output_text_selected) > 0) {
    ## strsplit namespace? should be stringr
    ticker <- strsplit(html_output_text_selected, "\n")[[1]][1]
    ## If no regex is needed, grepl is faster with fixed = TRUE
    if (preferred_exchange == "Xetra"
         && grepl(preferred_exchange, html_output_text_selected, fixed = TRUE)) {
      ticker <- paste0(ticker, ".DE") }
  }

  return(ticker)

}


#' Get ticker based on ISIN by crawling the Xetra website
#'
#' @usage get_ticker_from_xetra(isin, preferred_exchange = "")
#' @param isin A single character string. ISIN.
#' @param preferred_exchange A single character string. Stock exchange (default is empty string)
#'
#' @export
get_ticker_from_xetra <- function(isin, preferred_exchange = "") {

  url <- "https://www.xetra.com/xetra-de/instrumente/alle-handelbaren-instrumente/boersefrankfurt/3906!search?query="
  url_isin <- paste0(url, isin)

  html_output <- rvest::read_html(url_isin)
  html_output_ol <- rvest::html_nodes(html_output, 'ol.list')
  url2 <- rvest::html_attr(html_output_ol, 'href')

  if (length(url2) > 0) {

    if (is.na(url2)) {
      html_output_ol <- rvest::html_nodes(html_output_ol, "a")
      url2 <- rvest::html_attr(html_output_ol, 'href')
    }

  } else {

    stop(paste("URL for", isin, "not found! Please add ticker manually."))

  }

  url2 <- paste0("https://www.xetra.com", url2)

  html_output <- rvest::read_html(url2)
  html_output_dl <- rvest::html_nodes(html_output, 'dl.list')

  names <- rvest::html_elements(html_output_dl, "dt")
  elements <- rvest::html_elements(html_output_dl, "dd")

  names <- rvest::html_text(names)
  elements <- rvest::html_text(elements)

  names <- gsub("\t|\n", "", names)

  ticker <- elements[grep("K.?rzel", names)]

  if (preferred_exchange == "Xetra")
    ticker <- paste0(ticker, ".DE")

  return(ticker)

}


#' Add ISIN-ticker pairs to table
#'
#' @usage add_ticker_manually(df_isin_ticker_new, path.tickers, file.ticker = "isin_ticker.csv")
#' @param df_isin_ticker_new A data frame containing a column for \emph{isin} and \emph{ticker}.
#' @param path.tickers A single character string. Folder where ISIN-ticker table is stored.
#' @param file.ticker A single character string. Name of ISIN-ticker csv file (default: isin_ticker.csv).
#'
#' @export
add_ticker_manually <- function(df_isin_ticker_new, path.tickers,
                                file.ticker = "isin_ticker.csv") {

  isins <- unique(df_isin_ticker_new$isin)
  isins <- isins[!grepl("^$", isins)]

  init_isin_ticker(path.tickers, file.ticker)

  ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
  df_isin_ticker <- data.table::fread(file.path(path.tickers, file.ticker))

  ## Identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
  new_isins <- isins[!(isins %in% unique(df_isin_ticker$isin))]

  if (length(new_isins) > 0) {

    for (i in 1:length(new_isins)) {

      isin <- new_isins[i]

      try({

        ## If no regex is needed, grepl is faster with fixed = TRUE
        df_isin_ticker_add <- df_isin_ticker_new[grepl(isin, df_isin_ticker_new$isin,
                                                       fixed = TRUE), ]
        # df_isin_ticker_add <- df_isin_ticker_new[grepl(isin, df_isin_ticker_new$isin), ]

        data.table::fwrite(df_isin_ticker_add,
                           file.path(path.tickers, file.ticker), append = TRUE)

      })

    }

  }

}

