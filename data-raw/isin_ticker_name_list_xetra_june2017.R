## Raw data for tickers from https://github.com/Deutsche-Boerse/dbg-pds


##
path <- "G:/Dropbox/Dokumente/Anderes/Finanzen/Data/dbg-pds-master/docs/samples/xetra"
files <- file.path(path, list.files(path))
list.dfs <- lapply(files, data.table::fread)
df <- do.call(rbind, unname(list.dfs))

df <- df[, 1:3 ]

names(df) <- c("isin", "ticker", "name")

df <- unique(df)

data.table::fwrite(df, "inst/extdata/isin_ticker_name_list_xetra_june2017.csv")
# data.table::fwrite(df[, c("isin", "ticker")], "inst/extdata/isin_ticker_list_xetra_june2017.csv")

# df.test <- data.table::fread("inst/extdata/isin_ticker_name_list_xetra_june2017.csv")
