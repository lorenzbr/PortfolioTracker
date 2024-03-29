% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_price_data.R
\name{get_ticker_prices}
\alias{get_ticker_prices}
\title{Get prices from the Yahoo Finance API fast and simple}
\usage{
get_ticker_prices(ticker, from, to)
}
\arguments{
\item{ticker}{A single character string. Ticker symbol.}

\item{from}{A single character string. Start date.}

\item{to}{A single character string. End date.}
}
\value{
A data frame containing dates, prices (open, high, low, close, adjusted) and volume.
}
\description{
Get prices from the Yahoo Finance API fast and simple
}
