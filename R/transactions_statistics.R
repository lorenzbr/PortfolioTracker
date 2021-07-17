#' get transaction statistics
#'
#' @usage get_transaction_statistics(df.transactions)
#' @param df.transactions A data frame containing transactions.
#' @return df.stats A data frame containing transaction statistics.
#'
#' @export
get_transaction_statistics <- function(df.transactions) {

  df.table.types <- data.frame(table(df.transactions$transaction_type))
  names(df.table.types) <- c("transaction_type", "frequency")

  df.types.fee <- stats::aggregate(df.transactions$transaction_fee, by = list(df.transactions$transaction_type), FUN = sum)
  names(df.types.fee) <- c("transaction_type", "transaction_fee")

  df.stats <- merge(df.table.types, df.types.fee, by = "transaction_type", all = TRUE)

  return(df.stats)

}
