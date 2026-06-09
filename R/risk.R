compute_var  <- function(r, conf = 0.95) quantile(r, 1 - conf, na.rm = TRUE)
compute_cvar <- function(r, conf = 0.95) {
  thresh <- compute_var(r, conf)
  mean(r[r <= thresh], na.rm = TRUE)
}

rolling_var <- function(r, window = 60, conf = 0.95) {
  n   <- length(r)
  out <- rep(NA_real_, n)
  for (i in window:n) out[i] <- compute_var(r[(i - window + 1):i], conf)
  out
}

full_metrics <- function(r, rf_annual = 0.05) {
  r       <- r[!is.na(r)]
  rf_day  <- (1 + rf_annual)^(1/252) - 1
  ann_ret <- mean(r) * 252
  ann_vol <- sd(r) * sqrt(252)
  sharpe  <- (ann_ret - rf_annual) / ann_vol

  down_r  <- r[r < rf_day] - rf_day
  sortino <- (ann_ret - rf_annual) / (sd(down_r) * sqrt(252))

  r_xts  <- xts(r, order.by = seq.Date(Sys.Date() - length(r), by = "day", length.out = length(r)))
  max_dd <- as.numeric(maxDrawdown(r_xts))
  calmar <- ann_ret / abs(max_dd)

  tibble(
    Metric = c(
      "Annualized Return", "Annualized Volatility",
      "Sharpe Ratio", "Sortino Ratio",
      "Max Drawdown", "Calmar Ratio",
      "VaR (95%)", "CVaR (95%)",
      "Skewness", "Excess Kurtosis",
      "% Positive Days", "Best Day", "Worst Day"
    ),
    Value = c(
      format_pct(ann_ret), format_pct(ann_vol),
      round(sharpe, 3), round(sortino, 3),
      format_pct(max_dd), round(calmar, 3),
      format_pct(compute_var(r)), format_pct(compute_cvar(r)),
      round(moments::skewness(r), 3), round(moments::kurtosis(r) - 3, 3),
      format_pct(mean(r > 0)), format_pct(max(r)), format_pct(min(r))
    )
  )
}
