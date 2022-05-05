## Test get_quantity_panel
## Test get_price_quantity_panel2

df_transactions <- transactions
user_name <- "test_user1"
portfolio_name <- "portfolio_1"
db_path <- test_path("testdata")
user_path <- test_path(file.path("testdata/data/user_data", user_name))

## Delete all data for pt workflow
## To do: however, this means that I do not really
## test the function append_latest_prices_db --> use other user_name or
## portfolio_name
# unlink(test_path("testdata/data"), recursive = TRUE)
# unlink(user_path, recursive = TRUE)

test_that("Portfolio Tracker initiation is successful", {

  expect_error(init_portfolio_tracker(db_path = db_path), NA)
  expect_error(create_user_dir(user_path, portfolio_name), NA)
  expect_error(get_user_names(user_path, portfolio_name), NA)

})


df_isin_ticker <- data.table::fread(
  system.file("extdata", "isin_ticker_name_list_xetra_june2017.csv",
              package = "PortfolioTracker"))

df_isin_ticker <- as.data.frame(df_isin_ticker)
df_isin_ticker <- df_isin_ticker[, c("isin", "ticker")]

df_transactions_with_tickers <- merge(transactions,
                                      df_isin_ticker,
                                      by = "isin")

tickers <- unique(df_transactions_with_tickers$ticker)

list_quantity_panels <- mapply(
  get_quantity_panel, tickers,
  MoreArgs = list(df_transactions_with_tickers, user_path),
  SIMPLIFY = FALSE)
# df_quantity_panels <- do.call(rbind, unname(list_quantity_panels))

list_pricequantity_panels <- mapply(
  get_price_quantity_panel2, tickers,
  MoreArgs = list(df_transactions_with_tickers,
                  path.prices.db = test_path("testdata"),
                  user_path),
  SIMPLIFY = FALSE)

test_that("object is a data frame", {

  expect_s3_class(list_quantity_panels[[1]], "data.frame")
  expect_s3_class(list_quantity_panels[[2]], "data.frame")
  expect_s3_class(list_quantity_panels[[3]], "data.frame")
  expect_s3_class(list_pricequantity_panels[[1]], "data.frame")
  expect_s3_class(list_pricequantity_panels[[2]], "data.frame")
  expect_s3_class(list_pricequantity_panels[[3]], "data.frame")

})

test_that("data frame has four columns", {

  expect_length(list_quantity_panels[[1]], 4)
  expect_length(list_quantity_panels[[2]], 4)
  expect_length(list_quantity_panels[[3]], 4)
  expect_length(list_pricequantity_panels[[1]], 11)
  expect_length(list_pricequantity_panels[[2]], 11)
  expect_length(list_pricequantity_panels[[3]], 11)

})

test_that("all data in data frame are unequal NA", {

  expect_equal(all(is.na(list_quantity_panels[[1]])), FALSE)
  expect_equal(all(is.na(list_quantity_panels[[2]])), FALSE)
  expect_equal(all(is.na(list_quantity_panels[[3]])), FALSE)
  expect_equal(all(is.na(list_pricequantity_panels[[1]])), FALSE)
  expect_equal(all(is.na(list_pricequantity_panels[[2]])), FALSE)
  expect_equal(all(is.na(list_pricequantity_panels[[3]])), FALSE)

})
