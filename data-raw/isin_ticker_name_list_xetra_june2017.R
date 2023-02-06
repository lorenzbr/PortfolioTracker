## Raw data for tickers from https://github.com/Deutsche-Boerse/dbg-pds

## Download the data and store it as csv locally

## Read tickers from Extra as data table
config <- config::get("tickers_from_xetra")
path <- config$path
files <- file.path(path, list.files(path))
list_dts <- lapply(files, data.table::fread)
dt <- do.call(rbind, unname(list_dts))

dt <- dt[, c("ISIN", "Mnemonic", "SecurityDesc")]
names(dt) <- c("isin", "ticker", "name")

dt <- unique(dt)

output_filepath <- "inst/extdata/isin_ticker_name_list_xetra_june2017.csv"
data.table::fwrite(dt, output_filepath)
# data.table::fwrite(dt[, c("isin", "ticker")], output_filepath)

# dt_test <- data.table::fread(output_filepath)
