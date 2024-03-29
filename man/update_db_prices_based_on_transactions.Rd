% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_price_data.R
\name{update_db_prices_based_on_transactions}
\alias{update_db_prices_based_on_transactions}
\title{Update and store prices in database based on new transactions}
\usage{
update_db_prices_based_on_transactions(df_transactions, db_path,
         external_search = TRUE)
}
\arguments{
\item{df_transactions}{A data frame. Results from
\code{\link[BankStatementParser]{get_transactions}}.
At least three variables/columns are required: \emph{transaction_type},
\emph{isin} and \emph{transaction_date}}

\item{db_path}{A single character string containing the directory of the database.}

\item{external_search}{Logical; if TRUE, the function searches external
sources to find the ticker.}
}
\description{
Update and store prices in the database (currently csv files)
shared by all users. For each ticker, prices are written to a separate csv file.
Prices are updated if incoming transactions are older than the latest price
that is available according to a list with first and last date of a given ticker
(this list is stored in a csv as well). Prices are also updated if there are
no prices available for the given ticker.
}
