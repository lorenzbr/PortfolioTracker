#' Create ticker list if no such file exists
#'
#'
#' @export
init_ticker_list <- function(path, file){

  #### create ticker list if no such file exists

  if(!(file.exists(paste0(path, file)))){
    df.isin.ticker.init <- data.frame(matrix(NA, nrow=0, ncol=2, dimnames = list(NULL, c("isin", "ticker"))))
    data.table::fwrite(df.isin.ticker.init,paste0(path, file))
  }

} ## end of function init_ticker_list

#' Update ticker-ISIN list
#'
#' @export
update_ticker_isin_pairs <- function(path, file){

  #### update ticker-ISIN list

  ## get table with conversion of all relevant ISINs to ticker
  df.isin.ticker.converter <- portfoliotracker::get_ticker_from_isin()

  ## get all new processed transactions (csv's)
  filenames.processed.transaction.data <- list.files(path.data.processed.transactions.new)

  ## load new transactions if file names are not empty
  if(!(rlang::is_empty(filenames.processed.transaction.data))){

    ## load transaction data
    list.transactions <- lapply(paste0(path.data.processed.transactions.new, filenames.processed.transaction.data), data.table::fread)
    df.transactions.new <- do.call("rbind", list.transactions)

    ## identify all ISINs in transaction data and check whether the corresponding ticker is in the table already
    isins.not.in.table <- unique(df.transactions.new$isin)[!(unique(df.transactions.new$isin) %in% unique(df.isin.ticker.converter$isin))]
    if(!(rlang::is_empty(isins.not.in.table))){
      for(i in 1:length(isins.not.in.table)){
        isin <- isins.not.in.table[i]
        try({ticker <- get.isin.to.ticker.crawler.xetra(isin)
        df.isin.ticker.new <- data.frame(isin = isin, ticker = ticker)
        data.table::fwrite(df.isin.ticker.new,paste0(path.data.raw, filename.data.isin.ticker.converter), append = TRUE)
        print(paste(ticker, "added."))})
      } ## end of for loop
    } else(print("New transactions, but ticker already available.")) ## end of if statement ISIN not in table is empty
  } else (return("No new transactions. Current ticker-ISIN list complete!")) ## end of if else statement vector is empty

} ## end of function update_ticker_isin_pairs

#' Get ticker from ISIN
#'
#'
#' @export
get_ticker_from_isin <- function(){

  #### get conversion of ISIN to ticker

  ## table that converts ISIN to ticker (which is needed by yahoo finance)
  df.isin.ticker.converter <- data.table::fread(paste0(path.data.raw,filename.data.isin.ticker.converter))

  return(df.isin.ticker.converter)

} ## end of function get ticker from ISIN

#'
#' Get ticker based on ISIN by crawling investing.com
#'
#'
#' @export
get_ticker_from_investing <- function(isin, preferred.stock.exchange = ""){

  #### function get ticker from ISIN

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

  if(!(rlang::is_empty(html.output.text.selected))){
    ticker <- strsplit(html.output.text.selected,"\n")[[1]][1]
    if(preferred.stock.exchange == "Xetra" & grepl(preferred.stock.exchange, html.output.text.selected)){
      ticker <- paste0(ticker,".DE")}
  }

  return(ticker)

} ## end of function get_ticker_from_investing


#' Get ticker based on ISIN by crawling the Xetra website
#'
#'
#' @export
get_ticker_from_xetra <- function(isin, preferred.stock.exchange = "Xetra"){

  #### web crawler to get ticker from Xetra website based on ISIN code

  url <- "https://www.xetra.com/xetra-de/instrumente/alle-handelbaren-instrumente/boersefrankfurt/3906!search?query="
  url.isin <- paste0(url, isin)

  html.output <- rvest::read_html(url.isin)
  html.output.ol <- rvest::html_nodes(html.output, 'ol.list')
  url2 <- rvest::html_attr(html.output.ol, 'href')
  if(!(rlang::is_empty(url2))){
    if(is.na(url2)){
      html.output.ol <- rvest::html_nodes(html.output.ol, "a")
      url2 <- rvest::html_attr(html.output.ol, 'href')
    }
    if(is.na(url2)) message(paste("Url for", isin, "not found! Please add ticker manually."))
  } else{stop(paste("Url for", isin, "not found! Please add ticker manually."))}
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



