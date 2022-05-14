#' get transaction statistics
#'
#' @usage get_transaction_statistics(df_transactions)
#' @param df_transactions A data frame containing transactions.
#' @return A data frame containing transaction statistics.
#'
#' @export
get_transaction_statistics <- function(df_transactions) {

  df_transactions <- as.data.frame(df_transactions)

  if (nrow(df_transactions) > 0) {

    df_table_types <- data.frame(table(df_transactions$transaction_type))
    names(df_table_types) <- c("transaction_type", "frequency")

    df_types_fee <- stats::aggregate(df_transactions$transaction_fee,
                                     by = list(df_transactions$transaction_type),
                                     FUN = sum)
    names(df_types_fee) <- c("transaction_type", "transaction_fee")

    df_stats <- merge(df_table_types, df_types_fee,
                      by = "transaction_type",
                      all = TRUE)

  } else {

    df_stats <- data.frame(transaction_type = c("Purchase", "Sale", "Dividend"),
                           transaction_fee = c(0, 0, 0),
                           frequency = c(0, 0, 0))
    df_stats$frequency <- as.integer(df_stats$frequency)

  }

  return(df_stats)

}
