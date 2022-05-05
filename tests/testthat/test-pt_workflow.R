## Test the entire workflow of the Portfolio Tracker
## To do: Test not only that functions do not produce any errors, but also
##        test that output actually makes sense (files exist, content is correct)

## Test of work flow is based on PortfolioTracker::transactions
## To do: add more/other transactions to test other variants and features
## (e.g., ...?)

df_transactions <- transactions
user_name <- "test_user1"
portfolio_name <- "portfolio_1"
db_path <- test_path("testdata")
user_path <- test_path(file.path("testdata/data/user_data", user_name))

## Delete all data for pt workflow
## To do: however, this means that I do not really
## test the function append_latest_prices_db --> use other user_name or
## portfolio_name
unlink(test_path("testdata/data"), recursive = TRUE)
# unlink(user_path, recursive = TRUE)

test_that("Portfolio Tracker initiation is successful", {

  expect_error(init_portfolio_tracker(db_path = db_path), NA)
  expect_error(create_user_dir(user_path, portfolio_name), NA)
  expect_error(get_user_names(user_path, portfolio_name), NA)

})


test_that("Portfolio Tracker workflow is successful", {

  expect_error(data.table::fwrite(
    df_transactions, file.path(path.transactions, file.transactions)), NA)
  expect_error(update_db_prices_based_on_transactions(
    df_transactions, db_path, external_search = TRUE), NA)
  ## Tickers need to be loaded
  expect_error(tickers <- get_tickers_from_db(df_transactions,
                                              db_path)[[2]], NA)
  expect_error(append_latest_prices_db(db_path, tickers), NA)
  expect_error(write_price_quantity_panels2(
    df_transactions, user_path, db_path), NA)
  expect_error(write_all_value_panels(df_transactions, user_path,
                                      db_path), NA)
  expect_error(write_complete_panels(user_path, db_path), NA)
  ## I think investment value panels are not required!
  # expect_error(write_investment_value_panels(user_path), NA)
  expect_error(write_portfolio_twr_factors(user_path), NA)
  expect_error(write_current_portfolio(user_path, db_path), NA)
  expect_error(write_portfolio_stats(user_path), NA)
  expect_error(write_previous_investments(user_path, db_path), NA)
  expect_error(write_returns(user_path), NA)
  expect_error(write_annualized_returns(user_path, db_path), NA)
  ## At the moment write_portfolio_return not really needed
  # expect_error(write_portfolio_return(user_path), NA)
  expect_error(write_roi_by_period_all(user_path, db_path), NA)
  expect_error(write_investment_irr_all(user_path), NA)
  expect_error(write_dividend_history(df_transactions, user_path), NA)
  expect_error(write_dividend_by_month(user_path), NA)
  expect_error(write_dividend_by_yr(user_path), NA)

})

test_that("all files exist", {

  expect_true(file.exists(file.path(path.transactions, file.transactions)))
  expect_true(file.exists(file.path(path.database, file.tickers.db)))
  expect_true(file.exists(file.path(path.database, file.ticker.exchange.db)))
  expect_true(file.exists(file.path(path.database, file.ticker.price.available.db)))
  expect_true(file.exists(file.path(path.database, file.stock.splits.db)))
  ## To do: Check the following files/functions as well
  # ... write_price_quantity_panels2
  # ... write_all_value_panels
  # ... write_complete_panels
  # ... write_investment_value_panels
  expect_true(file.exists(file.path(path.returns, file.returns.twr.daily)))
  expect_true(file.exists(file.path(path.data, file.current)))
  expect_true(file.exists(file.path(path.data, file.stats)))
  expect_true(file.exists(file.path(path.data, file.previous)))
  expect_true(file.exists(file.path(path.returns, file.returns.daily)))
  expect_true(file.exists(file.path(path.returns, file.returns.monthly)))
  expect_true(file.exists(file.path(path.returns, file.returns.annual)))
  expect_true(file.exists(file.path(path.returns, file.returns.annualized)))
  ## To do: Check the following files/functions as well
  # ... write_portfolio_return
  # ... write_roi_by_period_all
  expect_true(file.exists(file.path(path.returns, file.returns.irr)))
  expect_true(file.exists(file.path(path.dividends, file.dividend.history)))
  expect_true(file.exists(file.path(path.dividends, file.dividend.month)))
  expect_true(file.exists(file.path(path.dividends, file.dividend.year)))

})

test_that("prices in database for each date and ticker are unique", {

  files <- file.path(path.prices.db, list.files(path.prices.db))
  list_dfs <- lapply(files, data.table::fread)
  for(i in 1:length(list_dfs)) {
    df <- list_dfs[[i]]
    expect_true(length(unique(df$date)) == length(df$date))
  }

})

## Clean up: delete everything after testing
# unlink(test_path("testdata/data"), recursive = TRUE)
# unlink(user_path, recursive = TRUE)
