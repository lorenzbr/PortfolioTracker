# Portfolio Tracker
This R package contains functions to track the financial performance of your investments.


## Introduction

...

## Overview

...


## Contact

Please contact <lorenz.brachtendorf@gmx.de> if you want to:
* contribute to this project
* have additional features (e.g., other banks or brokers)

You can also submit bug reports and suggestions via e-mail or <https://github.com/lorenzbr/portfoliotracker/issues> 


## Installation


```R
devtools::install_github("lorenzbr/portfoliotracker")
```


## Dependencies

```R
install.packages("data.table")
install.packages("dplyr")
install.packages("magrittr")
install.packages("quantmod")
install.packages("rlang")
install.packages("rvest")
install.packages("stringr")
```

## Usage

```R
# Example

# directory
path <- "portfoliotracker/"

## load transactions
df.transactions <- transactions

# get price data from Yahoo based on transactions
update_prices_based_on_transactions(df.transactions, path)
```

## Documentation

Generally, [www.investopedia.com](https://www.investopedia.com) is a good starting point to understand the different approaches to measures financial returns.

Annualized return
:   "An annualized total return is the geometric average amount of money earned by an investment each year over a given time period." See [here](https://www.investopedia.com/terms/a/annualized-total-return.asp) for further details.

Annual return
:   "The annual return is the return that an investment provides over a period of time, expressed as a time-weighted annual percentage." See [here](https://www.investopedia.com/terms/a/annual-return.asp) for further details.

Geometric mean
:   ...

Investment return
:   ...

## License

This R package is licensed under the GNU General Public License v3.0.

See [here](https://github.com/lorenzbr/portfoliotracker/blob/main/LICENSE) for further information.
