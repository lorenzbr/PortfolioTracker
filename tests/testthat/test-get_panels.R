## To do: start with set of transactions stored in testdata
## create directory in testdata and run update_portfoliotracker2()
# df_transactions <- transactions
# user_name <- "test_user1"
# db_path <- test_path("testdata")
# user_path <- test_path(file.path("testdata/data/user_data", user_name))
# # db_path <- "."
# # user_path <- file.path("./data/user_data", user_name)
# create_main_dir(path = db_path)
# get_db_names(path = db_path)
# get_user_names(user_path, portfolio_name = "portfolio_1")
# create_user_dir(user_path, portfolio_name = "portfolio_1")
## Then run everything from update_portfoliotracker2()
# tickers <- PortfolioTracker::get_tickers_from_db(df_transactions, db_path)[[2]]
# PortfolioTracker::append_latest_prices_db(db_path, tickers)
# PortfolioTracker::update_db_prices_based_on_transactions(
#   df_transactions, db_path, external_search = TRUE)
# PortfolioTracker::write_price_quantity_panels2(
#   df_transactions, user_path, db_path)
# PortfolioTracker::write_all_value_panels(df_transactions, user_path)
# PortfolioTracker::write_complete_panels(user_path)
# ## I think investment value panels are not required!
# # PortfolioTracker::write_investment_value_panels(user_path)
# PortfolioTracker::write_portfolio_twr_factors(user_path)
# PortfolioTracker::write_current_portfolio(user_path)
# PortfolioTracker::write_portfolio_stats(user_path)
# PortfolioTracker::write_previous_investments(user_path)
# PortfolioTracker::write_returns(user_path)
# PortfolioTracker::write_annualized_returns(user_path)
# ## At the moment write portfolio return not really needed
# # PortfolioTracker::write_portfolio_return(user_path)
# PortfolioTracker::write_roi_by_period_all(user_path)
# PortfolioTracker::write_investment_irr_all(user_path)
# PortfolioTracker::write_dividend_history(df_transactions, user_path)
# PortfolioTracker::write_dividend_by_month(user_path)
# PortfolioTracker::write_dividend_by_yr(user_path)
## !!! Then test all of those results using testhat and load the data if needed !!!


df_transactions <- transactions

df_isin_ticker <- data.table::fread(
  system.file("extdata", "isin_ticker_name_list_xetra_june2017.csv",
              package = "PortfolioTracker"))

df_isin_ticker <- as.data.frame(df_isin_ticker)
df_isin_ticker <- df_isin_ticker[, c("isin", "ticker")]

df_transactions_with_tickers <- merge(transactions,
                                      df_isin_ticker,
                                      by = "isin")

tickers <- unique(df_transactions_with_tickers$ticker)

list_quantity_panels <- mapply(get_quantity_panel, tickers,
                                 MoreArgs = list(df_transactions_with_tickers),
                                 SIMPLIFY = FALSE)
# df_quantity_panels <- do.call(rbind, unname(list_quantity_panels))

list_pricequantity_panels <- mapply(get_price_quantity_panel2, tickers,
                               MoreArgs = list(df_transactions_with_tickers,
                                               path.prices.db = test_path("testdata")),
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
