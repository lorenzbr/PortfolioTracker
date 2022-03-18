#' Produces a suggestion to rebalance your portfolio
#'
#' @usage rebalance_portfolio(df, df.target.shares, money.to.invest, step.size = 500,
#'                            exclude.from.rebalancing = NA)
#' @param df A data frame containing at least a column for \code{isin} (character) and \code{value} (numeric)
#' @param df.target.shares A data frame containing a column for \code{isin} (character) and \code{target_share} (numeric)
#' @param money.to.invest A single numeric. Amount to invest for rebalancing.
#' @param step.size A single numeric. Step size (default is 500).
#' @param exclude.from.rebalancing A single character or vector of strings. Should contain ISINs (default is NA).
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
rebalance_portfolio <- function(df, df.target.shares, money.to.invest, step.size = 500,
                                exclude.from.rebalancing = NA) {

  ## function should be iterative or nested function, resp., with step size of investing money and adapting actual shares, stop when
  ## target shares are reached more or less or when money to invest is too small
  ## function needs one row per ISIN, target shares for each ISIN in df, step size of money to invest

  df <- as.data.frame(df)
  df.target.shares <- as.data.frame(df.target.shares)

  ## number of iterations
  nb.iter <- floor(money.to.invest / step.size)

  ## choose entries with most recent date for each ISIN if column with date exists
  if (any(names(df) == "date")) {
    df$date <- as.Date(df$date, "%d-%m-%Y")
    df <- df %>% dplyr::group_by(.data$isin) %>% dplyr::filter(date == max(.data$date))
    df <- df %>% dplyr::group_by(.data$isin) %>% dplyr::sample_n(size = 1)
  }

  ## keep only relevant columns
  df <- df[,c("isin", "value")]

  ## do not consider the following ISINs
  df <- df[!(df$isin %in% exclude.from.rebalancing), ]

  ## actual shares
  df$actual_share <- df$value / sum(df$value)

  ## merge target_shares and keep only those for which there is a target share specified
  df <- merge(df, df.target.shares, by = "isin")

  ## new columns
  df$value_to_invest <- 0
  df$new_value <- df$value
  df$new_share <- df$actual_share

  for (i in 1:nb.iter) {

    ## actual shares
    df$new_share <- df$new_value / sum(df$new_value)

    ## compute target values
    df$target_value <- df$target_share * sum(df$new_value)

    ## deviation
    df$share_deviation <- df$new_share - df$target_share
    df$value_deviation <- df$new_value - df$target_value

    ## identify ISIN with maximum negative deviation which is at least the amount of step.size
    isin.with.max.deviation <- df$isin[df$value_deviation == min(df$value_deviation)]

    ## invest in ISIN with max deviation
    df$new_value[df$isin == isin.with.max.deviation] <- df$new_value[df$isin == isin.with.max.deviation] + step.size
    df$value_to_invest[df$isin == isin.with.max.deviation] <- df$value_to_invest[df$isin == isin.with.max.deviation] + step.size

    ## calculate new shares
    df$new_share <- df$new_value / sum(df$new_value)

  }

  return(df)

}

#' Write default target shares to csv file
#'
#' @usage write_default_target_shares(df.target.shares, path)
#' @param df.target.shares A data frame containing a column for \code{isin} (character) and \code{target_share} (numeric)
#' @param path A single character string. Path where data are stored.
#'
#' @export
write_default_target_shares <- function(df.target.shares, path) {

  get_user_names(path)

  if (is.data.frame(df.target.shares)) {

    if ( all( c("isin", "target_share") %in% names(df.target.shares) )) {

      data.table::fwrite(df.target.shares, file.path(path.rebalance, file.target.shares))

    }

    else {

      "Data.frame needs to contain at least two columns with names 'isin' and 'target_share'"

      }

    } else {

      # message("Input has to be of type data.frame. ")

  }

}
