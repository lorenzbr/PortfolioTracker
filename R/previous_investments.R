#' Write previous portfolio investments to a csv file
#'
#' @usage write_previous_investments(user_path, db_path)
#' @param user_path A single character string containing the directory of the user.
#' @param db_path A single character string containing the directory of the database.
#'
#' @export
write_previous_investments <- function(user_path, db_path) {

  get_user_names(user_path)
  get_db_names(db_path)

  if (length(list.files(path.pricequantity.panel)) > 0) {

    files <- file.path(path.pricequantity.panel, list.files(path.pricequantity.panel))
    list_dfs <- lapply(files, data.table::fread)

    file_path_transactions <- file.path(path.transactions, file.transactions)

    if (file.exists(file_path_transactions)) {

      ## Get table that converts ISIN to ticker (which is needed by Yahoo Finance)
      df_isin_ticker <- data.table::fread(file.path(path.database, file.tickers.db))

      ## Keep ISIN and name
      df_transactions <- data.table::fread(file_path_transactions)

      df_ticker_names <- unique(df_transactions[, c("isin", "name")])

      ## Add tickers
      df_ticker_names <- merge(df_ticker_names, df_isin_ticker, by = "isin")
      df_ticker_names <- unique(df_ticker_names[, c("ticker", "name")])

      ## All price-quantity panels in one data frame
      df_all <- do.call(rbind, list_dfs)

      df_previous <- stats::aggregate(date ~ ticker, data = df_all, max)
      df_previous <- merge(df_all, df_previous, by = c("ticker", "date"))

      ## TO DO: KEEP INVESTMENTS WHICH HAVE AT LEAST ONE SALES TRANSACTION
      ## (USE TRANSACTION HISTORY TO IDENTIFY THOSE)

      ## Keep investments with quantity equal to zero
      df_previous <- df_previous[df_previous$cum_quantity == 0, ]

      df_previous <- unique(df_previous)

      ## Add name and ISIN
      df_previous <- merge(df_previous, df_ticker_names, by = "ticker")
      df_previous <- merge(df_previous, df_isin_ticker, by = "ticker")

      df_previous <- df_previous[, c("name", "isin", "ticker", "adjusted",
                                     "cum_quantity", "value")]

      previous_isins <- unique(df_previous$isin)

      isins_sold <- unique(df_transactions$isin[df_transactions$transaction_type == "Sale"])
      isins_purchase <- unique(df_transactions$isin[df_transactions$transaction_type == "Purchase"])
      isins_both <- intersect(isins_sold, isins_purchase)
      previous_isins <- intersect(previous_isins, isins_both)

      if (length(previous_isins) > 0) {

        ## Keep all transactions from ISINS which have both Purchase and Sale transactions
        df_investments_sold <- unique(df_transactions[df_transactions$isin %in% isins_both, ])

        col_names <- c("isin", "name", "investment", "income",
                       "return_absolute", "return_percent", "quantity")
        df <- data.frame(matrix(nrow = 0, ncol = length(col_names),
                                dimnames = list(NULL, col_names)))

        ## For each sold ISIN
        for (i in 1:length(previous_isins)) {

          previous_isin <- previous_isins[i]
          df_sold_isin <- df_investments_sold[df_investments_sold$isin == previous_isin, ]

          name <- df_sold_isin$name[i]

          investment <- sum(df_sold_isin$transaction_value[df_sold_isin$transaction_type == "Purchase"
                                                           | df_sold_isin$transaction_type == "Steuerpflichtige Vorabpauschale"])
          income <- sum(df_sold_isin$transaction_value[df_sold_isin$transaction_type == "Dividend"
                                                       | df_sold_isin$transaction_type == "Sale"
                                                       | df_sold_isin$transaction_type == "Sale - Part"])
          return_abs <- income - investment
          return_perc <- return_abs / investment

          quantity_sold <- sum(df_sold_isin$quantity[df_sold_isin$transaction_type == "Sale"
                                                     | df_sold_isin$transaction_type == "Sale - Part"])


          df_temp <- data.frame(isin = previous_isin,
                                name,
                                investment,
                                income,
                                return_absolute = return_abs,
                                return_percent = return_perc,
                                quantity = quantity_sold
                                )

          df <- rbind(df, df_temp)

        }

        data.table::fwrite(df, file.path(path.data, file.previous))

      } else {

        ## If no previous ISINs exist, delete file with previous investments
        unlink(file.path(path.data, file.previous))

      }

    }

  }

}
