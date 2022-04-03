df_transactions <- transactions
user_name <- "test_user1"
db_path <- test_path("testdata")
user_path <- test_path(file.path("testdata/data/user_data", user_name))
tickers <- get_tickers_from_db(df_transactions, db_path)[[2]]
get_db_names(path = db_path)
get_user_names(user_path, portfolio_name = "portfolio_1")
data.table::fwrite(
  df_transactions, file.path(path.transactions, file.transactions))

test_that("functions are successful", {

  expect_error(create_main_dir(path = db_path), NA)
  expect_error(create_user_dir(user_path, portfolio_name = "portfolio_1"), NA)
  expect_error(get_db_names(path = db_path), NA)
  expect_error(get_user_names(user_path, portfolio_name = "portfolio_1"), NA)
  expect_error(append_latest_prices_db(db_path, tickers), NA)
  expect_error(update_db_prices_based_on_transactions(
    df_transactions, db_path, external_search = TRUE), NA)
  expect_error(write_price_quantity_panels2(
    df_transactions, user_path, db_path), NA)
  expect_error(write_all_value_panels(df_transactions, user_path, db_path), NA)
  expect_error(write_complete_panels(user_path, db_path), NA)
  ## I think investment value panels are not required!
  # expect_error(write_investment_value_panels(user_path), NA)
  expect_error(write_portfolio_twr_factors(user_path), NA)
  expect_error(write_current_portfolio(user_path, db_path), NA)
  expect_error(write_portfolio_stats(user_path), NA)
  expect_error(write_previous_investments(user_path, db_path), NA)
  expect_error(write_returns(user_path), NA)

})

# ## To do: still need to check manually:
# write_annualized_returns(user_path)
# ## At the moment write portfolio return not really needed
# # write_portfolio_return(user_path)
# write_roi_by_period_all(user_path)
# write_investment_irr_all(user_path)
# write_dividend_history(df_transactions, user_path)
# write_dividend_by_month(user_path)
# write_dividend_by_yr(user_path)
# ## !!! Then test all of those results using testthat
# ## Load the data to test it



# # divide_by_y <- function(x, y) {
# #   if (y == 0) {
# #     stop("do not divide by zero")
# #   } else {
# #     x / y
# #   }
# # }
#
# test_that("functions are successful", {
#
#   # expect_error(divide_by_y(5, 0))
#   # expect_error(divide_by_y(5, 2), NA)
#   # try(divide_by_y(3, 0))
#   # expect_failure(divide_by_y(5, 0))
#   # expect_success(divide_by_y(5, 0))
#
# })
# # divide_by_y(5, 0)
