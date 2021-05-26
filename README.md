# PortfolioTracker

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/lorenzbr/PortfolioTracker/workflows/R-CMD-check/badge.svg)](https://github.com/lorenzbr/PortfolioTracker/actions)
<!-- badges: end -->

This R package contains functions to track the performance of your investment portfolio.


## Installation

```R
devtools::install_github("lorenzbr/PortfolioTracker")
```


## Usage

```R
# Example

# directory
path <- "PortfolioTracker/"

## load transactions
df.transactions <- PortfolioTracker::transactions

# get price data from Yahoo based on transactions
PortfolioTracker::update_prices_based_on_transactions(df.transactions, path)

# update tickers to most recent day
PortfolioTracker::update_latest_prices(path)

# write panels as csv
PortfolioTracker::write_quantity_panels(df.transactions, path)
PortfolioTracker::write_price_panels(df.transactions, path)
PortfolioTracker::write_price_quantity_panels(df.transactions, path)
PortfolioTracker::write_all_value_panels(df.transaction.history, path)

# write current portfolio and create portfolio statistics
PortfolioTracker::write_current_portfolio(path)
PortfolioTracker::write_portfolio_stats(path)

# write returns
PortfolioTracker::write_returns(path)
PortfolioTracker::write_annualized_returns(path)
PortfolioTracker::write_portfolio_return(path)

# write dividends
PortfolioTracker::write_dividend_history(df.transactions, path)
PortfolioTracker::write_dividend_by_month(path)
PortfolioTracker::write_dividend_by_yr(path)
```


## Documentation

Annualized return
:   "An annualized total return is the geometric average amount of money earned by an investment each year over a given time period." See [here](https://www.investopedia.com/terms/a/annualized-total-return.asp) for further details.

Annual return
:   "The annual return is the return that an investment provides over a period of time, expressed as a time-weighted annual percentage." See [here](https://www.investopedia.com/terms/a/annual-return.asp) for further details.


## Contact

Please contact <lorenz.brachtendorf@gmx.de> if you want to:
* contribute to this project
* have additional features (e.g., returns on investments, cash reserves, other currencies, ...)

You can also submit bug reports and suggestions via e-mail or <https://github.com/lorenzbr/PortfolioTracker/issues> 


## License

This R package is licensed under the GNU General Public License v3.0.

See [here](https://github.com/lorenzbr/PortfolioTracker/blob/main/LICENSE) for further information.
