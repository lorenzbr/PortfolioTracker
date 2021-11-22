#' Write previous portfolio investments to a csv file
#'
#' @usage write_previous_investments(path)
#' @param path A single character string. Directory of your data.
#'
#' @export
write_previous_investments <- function(path) {

  get_names(path)

  ## load price quantity panels if exists
  if ( !rlang::is_empty(list.files(path.pricequantity.panel)) ) {

    files <- file.path(path.pricequantity.panel, list.files(path.pricequantity.panel))
    list.dfs <- lapply(files, data.table::fread)

    transaction.history.exists <- file.exists(file.path(path.transactions, file.transactions))
    isin.ticker.exists <- file.exists(file.path(path.tickers, file.tickers))

    if (isin.ticker.exists && transaction.history.exists) {

      ## get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df.isin.ticker <- data.table::fread(file.path(path.tickers, file.tickers))

      ## keep ISIN and name
      df.transaction.history <- data.table::fread(file.path(path.transactions, file.transactions))

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

      ## TO DO: KEEP INVESTMENTS WHICH HAVE AT LEAST ONE SALES TRANSACTION (USE TRANSACTION HISTORY TO IDENTIFY THOSE)

      ## keep investments with quantity equal to zero
      df.previous <- df.previous[df.previous$cum_quantity == 0, ]

      df.previous <- unique(df.previous)

      ## add name and ISIN
      df.previous <- merge(df.previous, df.ticker.investmentnames, by = "ticker")
      df.previous <- merge(df.previous, df.isin.ticker, by = "ticker")

      df.previous <- df.previous[, c("name", "isin", "ticker", "adjusted", "cum_quantity", "value")]

      previous.isins <- unique(df.previous$isin)

      isins.sold <- unique(df.transaction.history$isin[df.transaction.history$transaction_type == "Sale"])
      isins.purchase <- unique(df.transaction.history$isin[df.transaction.history$transaction_type == "Purchase"])
      isins.both <- intersect(isins.sold, isins.purchase)
      previous.isins <- intersect(previous.isins, isins.both)

      if(length(previous.isins) > 0) {

        ## keep all transactions from ISINS which have both Purchase and Sale transactions
        df.investments.sold <- unique(df.transaction.history[df.transaction.history$isin %in% isins.both, ])

        df <- data.frame(matrix(nrow = 0, ncol = 7, dimnames = list(NULL, c("isin", "name", "investment", "income",
                                                        "return_absolute", "return_percent", "quantity"))))

        ## for each sold ISIN
        for (i in 1:length(previous.isins)) {

          previous.isin <- previous.isins[i]
          df.sold.isin <- df.investments.sold[df.investments.sold$isin == previous.isin, ]

          name <- df.sold.isin$name[i]

          investment <- sum(df.sold.isin$transaction_value[df.sold.isin$transaction_type == "Purchase"
                                                           | df.sold.isin$transaction_type == "Steuerpflichtige Vorabpauschale"])
          income <- sum(df.sold.isin$transaction_value[df.sold.isin$transaction_type == "Dividend"
                                         | df.sold.isin$transaction_type == "Sale"
                                         | df.sold.isin$transaction_type == "Sale - Part"])
          return.abs <- income - investment
          return.perc <- return.abs / investment

          quantity.sold <- sum(df.sold.isin$quantity[df.sold.isin$transaction_type == "Sale"
                                                | df.sold.isin$transaction_type == "Sale - Part"])


          df.temp <- data.frame(isin = previous.isin, name, investment, income, return_absolute = return.abs,
                                return_percent = return.perc, quantity = quantity.sold)

          df <- rbind(df, df.temp)

        }

        data.table::fwrite(df, file.path(path.data, file.previous))

      }

    }

  } else { message("No price quantity panels available.") }

}
