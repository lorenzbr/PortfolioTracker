#' Details on nine transactions extracted from bank statements
#'
#' A data set containing details on four transactions extracted from PDF bank statements.
#' The variables are as follows:
#'
#' @format A data frame with 4 rows and 12 columns/variables:
#' \describe{
#' \item{isin}{The identifier ISIN}
#' \item{wkn}{The identifier WKN}
#' \item{name}{Name of the investment}
#' \item{quantity}{Quantity/number of stocks.}
#' \item{transaction_price}{Price of the transaction.}
#' \item{transaction_value}{Value of the transaction (includes fees).}
#' \item{transaction_fee}{Fee of the transaction. Adds up all fees.}
#' \item{transaction_date}{Date of the tranaction.}
#' \item{transaction_time}{Time of the transaction.}
#' \item{transaction_type}{Type of the transaction (e.g., purchase, sale).}
#' \item{document_page}{Page of the transaction in the PDF document.}
#' \item{document_name}{Name of the PDF document.}
#' }
#' @source Data come from the R package \code{BankStatementParser}. See \url{https://github.com/lorenzbr/BankStatementParser}
"transactions"
