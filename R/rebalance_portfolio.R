#' Produces a suggestion to rebalance your portfolio
#'
#' @usage rebalance_portfolio(df, df_target_shares, money_to_invest, step_size = 500,
#'                            exclude_from_rebalancing = NA)
#' @param df A data frame containing at least a column for \code{isin}
#' (character) and \code{value} (numeric)
#' @param df_target_shares A data frame containing a column for \code{isin}
#' (character) and \code{target_share} (numeric)
#' @param money_to_invest A single numeric. Amount to invest for rebalancing.
#' @param step_size A single numeric. Step size (default is 500).
#' @param exclude_from_rebalancing A single character or vector of strings.
#' Should contain ISINs (default is NA).
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
rebalance_portfolio <- function(df, df_target_shares, money_to_invest, step_size = 500,
                                exclude_from_rebalancing = NA) {

  ## function should be iterative or nested function, resp., with step size of
  ## investing money and adapting actual shares, stop when
  ## target shares are reached more or less or when money to invest is too small
  ## function needs one row per ISIN, target shares for each ISIN in df, step
  ## size of money to invest

  df <- as.data.frame(df)
  df_target_shares <- as.data.frame(df_target_shares)

  nb_iterations <- floor(money_to_invest / step_size)

  ## Choose entries with most recent date for each ISIN if column with date exists
  if (any(names(df) == "date")) {
    df$date <- as.Date(df$date, format = "%d-%m-%Y")
    df <- df %>%
      dplyr::group_by(.data$isin) %>%
      dplyr::filter(date == max(.data$date))
    df <- df %>%
      dplyr::group_by(.data$isin) %>%
      dplyr::sample_n(size = 1)
  }

  df <- df[, c("isin", "value")]

  df <- df[!(df$isin %in% exclude_from_rebalancing), ]

  df$actual_share <- df$value / sum(df$value)

  ## Merge target_shares and keep only those for which there is a target
  ## share specified
  df <- merge(df, df_target_shares, by = "isin")

  df$value_to_invest <- 0
  df$new_value <- df$value
  df$new_share <- df$actual_share

  for (i in 1:nb_iterations) {

    df$new_share <- df$new_value / sum(df$new_value)

    df$target_value <- df$target_share * sum(df$new_value)

    df$share_deviation <- df$new_share - df$target_share
    df$value_deviation <- df$new_value - df$target_value

    ## Identify ISIN with maximum negative deviation which is at least the amount of step_size
    ## Invest in ISIN with max deviation
    isin_with_max_deviation <- df$isin[df$value_deviation == min(df$value_deviation)]
    is_isin_with_max_deviation <- df$isin == isin_with_max_deviation
    df$new_value[is_isin_with_max_deviation] <- df$new_value[is_isin_with_max_deviation] +
      step_size
    df$value_to_invest[is_isin_with_max_deviation] <- df$value_to_invest[is_isin_with_max_deviation] +
      step_size

    df$new_share <- df$new_value / sum(df$new_value)

  }

  return(df)

}

#' Write default target shares to csv file
#'
#' @usage write_default_target_shares(df_target_shares, path)
#' @param df_target_shares A data frame containing a column for \code{isin}
#' (character) and \code{target_share} (numeric)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_default_target_shares <- function(df_target_shares, path) {

  get_user_names(path)

  if (is.data.frame(df_target_shares)) {

    if (all(c("isin", "target_share") %in% names(df_target_shares)))
      data.table::fwrite(df_target_shares,
                         file.path(path.rebalance, file.target.shares))

  }

}
