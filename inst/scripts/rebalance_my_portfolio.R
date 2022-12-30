## Rebalancing for my portfolio

money_to_invest <- 30000
step_size <- 50


file <- file.path(dirname(dirname(getwd())), "Rebalancing_ETFs.xlsx")
df <- readxl::read_xlsx(file)

df <- df[df$status != "old", ]

df <- df[, c("category", "value_sum", "share_soll")]

df$actual_share <- df$value_sum / sum(df$value_sum)

df$value_to_invest <- 0
df$new_value <- df$value_sum
df$new_share <- df$actual_share


nb_iterations <- floor(money_to_invest / step_size)

for (i in 1:nb_iterations) {

  df$new_share <- df$new_value / sum(df$new_value)

  df$target_value <- df$share_soll * sum(df$new_value)

  df$share_deviation <- df$new_share - df$share_soll
  df$value_deviation <- df$new_value - df$target_value

  ## Identify ISIN with maximum negative deviation which is at least the amount of step_size
  ## Invest in ISIN with max deviation
  isin_with_max_deviation <- df$category[df$value_deviation == min(df$value_deviation)]
  is_isin_with_max_deviation <- df$category == isin_with_max_deviation
  df$new_value[is_isin_with_max_deviation] <- df$new_value[is_isin_with_max_deviation] + step_size
  df$value_to_invest[is_isin_with_max_deviation] <- df$value_to_invest[is_isin_with_max_deviation] + step_size

  df$new_share <- df$new_value / sum(df$new_value)

}

View(df)
