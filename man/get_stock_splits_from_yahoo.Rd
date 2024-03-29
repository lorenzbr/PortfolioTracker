% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stock_splits.R
\name{get_stock_splits_from_yahoo}
\alias{get_stock_splits_from_yahoo}
\title{Get stock splits from the Yahoo Finance API}
\usage{
get_stock_splits_from_yahoo(ticker, from = "1970-01-01",
                                   to = Sys.Date())
}
\arguments{
\item{ticker}{A single character string. Ticker symbol.}

\item{from}{A single character string. Start date. Default is 01-01-1970.}

\item{to}{A single character string. End date. Default is current date.}
}
\value{
A data frame containing dates and stock splits.
}
\description{
Get stock splits from the Yahoo Finance API
}
