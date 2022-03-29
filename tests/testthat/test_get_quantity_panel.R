df_transactions <- transactions

df_isin_ticker <- data.table::fread(
  system.file("extdata", "isin_ticker_name_list_xetra_june2017.csv",
              package = "PortfolioTracker"))

df_isin_ticker <- as.data.frame(df_isin_ticker)

df_transactions_with_tickers <- merge(df_transactions,
                                      df_isin_ticker,
                                      by = "isin")

tickers <- unique(df_transactions_with_tickers$ticker)

list_quantity_panels <- mapply(get_quantity_panel, tickers,
                                 MoreArgs = list(df_transactions_with_tickers),
                                 SIMPLIFY = FALSE)
# df_quantity_panels <- do.call(rbind, unname(list_quantity_panels))

test_that("objects are data frames", {

  expect_s3_class(list_quantity_panels[[1]], "data.frame")
  expect_s3_class(list_quantity_panels[[2]], "data.frame")
  expect_s3_class(list_quantity_panels[[3]], "data.frame")

})

test_that("data frame has four columns", {

  expect_length(list_quantity_panels[[1]], 4)
  expect_length(list_quantity_panels[[2]], 4)
  expect_length(list_quantity_panels[[3]], 4)

})


