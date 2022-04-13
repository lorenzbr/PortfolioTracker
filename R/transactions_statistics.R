#' get transaction statistics
#'
#' @usage get_transaction_statistics(df_transactions)
#' @param df_transactions A data frame containing transactions.
#' @return A data frame containing transaction statistics.
#'
#' @export
get_transaction_statistics <- function(df_transactions) {

  df_table_types <- data.frame(table(df_transactions$transaction_type))
  names(df_table_types) <- c("transaction_type", "frequency")

  df_types_fee <- stats::aggregate(df_transactions$transaction_fee,
                                   by = list(df_transactions$transaction_type),
                                   FUN = sum)
  names(df_types_fee) <- c("transaction_type", "transaction_fee")

  df_stats <- merge(df_table_types, df_types_fee,
                    by = "transaction_type",
                    all = TRUE)

  return(df_stats)

}
