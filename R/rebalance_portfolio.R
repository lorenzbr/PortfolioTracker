#' Produces a suggestion to rebalance your portfolio
#'
#'
#' @export
rebalance_portfolio <- function(df, df.target.shares, exclude.from.rebalancing, money.to.invest, step.size = 500){

  ## function should be iterative or nested function, resp., with step size of investing money and adapting actual shares, stop when
  ## target shares are reached more or less or when money to invest is too small
  ## function needs one row per ISIN, target shares for each ISIN in df, step size of money to invest

  ## number of iterations
  nb.iter <- floor(money.to.invest / step.size)

  ## choose most recent date
  df <- df %>% group_by(isin) %>% filter(date == max(date))
  df <- df %>% group_by(isin) %>% sample_n(size = 1)


  ## keep only relevant columns
  df <- df[,c("isin", "value")]

  ## do not consider the following ISIN's
  df <- df[!(df$isin %in% exclude.from.rebalancing),]

  ## actual shares
  df$actual_share <- df$value / sum(df$value)

  ## merge target_shares and keep only those for which there is a target share specified
  df <- merge(df, df.target.shares, by = "isin")

  ## new columns
  df$value_to_invest <- 0
  df$new_value <- df$value
  df$new_share <- df$actual_share

  for(i in 1:nb.iter){

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

    print(i)

  } ## end of for loop

  return(df)

  ## print nice statement with old and new shares and what to invest
  # print()

} ## end of function rebalance_portfolio
