ohlcv_to_df <- function(xts_obj) {
  data.frame(
    date   = index(xts_obj),
    open   = as.numeric(Op(xts_obj)),
    high   = as.numeric(Hi(xts_obj)),
    low    = as.numeric(Lo(xts_obj)),
    close  = as.numeric(Cl(xts_obj)),
    volume = as.numeric(Vo(xts_obj)),
    adj    = as.numeric(Ad(xts_obj))
  ) %>% as_tibble()
}

add_indicators <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      sma_20  = as.numeric(SMA(adj, 20)),
      sma_50  = as.numeric(SMA(adj, 50)),
      sma_200 = as.numeric(SMA(adj, 200)),
      bb_mid  = as.numeric(SMA(adj, 20)),
      bb_sd   = as.numeric(runSD(adj, 20)),
      bb_up   = bb_mid + 2 * bb_sd,
      bb_lo   = bb_mid - 2 * bb_sd,
      ret     = adj / lag(adj) - 1,
      vol_20  = as.numeric(runSD(ret, 20)) * sqrt(252),
      rsi_14  = as.numeric(RSI(adj, 14))
    )
}

get_macd_df <- function(adj_prices, dates) {
  m <- MACD(adj_prices, nFast = 12, nSlow = 26, nSig = 9)
  tibble(
    date      = dates,
    macd      = as.numeric(m[, "macd"]),
    signal    = as.numeric(m[, "signal"]),
    histogram = as.numeric(m[, "macd"]) - as.numeric(m[, "signal"])
  )
}

make_candlestick <- function(df, ticker, show_sma, show_bb) {
  p <- plot_ly(df, x = ~date, type = "candlestick",
    open  = ~open, high = ~high, low = ~low, close = ~close,
    name  = ticker,
    increasing = list(line = list(color = "#3fb950"), fillcolor = "rgba(63,185,80,0.8)"),
    decreasing = list(line = list(color = "#f78166"), fillcolor = "rgba(247,129,102,0.8)"))

  if (show_bb && !all(is.na(df$bb_up))) {
    p <- p %>%
      add_ribbons(x = ~date, ymin = ~bb_lo, ymax = ~bb_up,
        name = "Bollinger Bands",
        fillcolor = "rgba(88,166,255,0.07)",
        line = list(color = "rgba(88,166,255,0.35)", width = 1),
        inherit = FALSE)
  }

  if (show_sma) {
    p <- p %>%
      add_lines(x = ~date, y = ~sma_20,  name = "SMA 20",
        line = list(color = "#d2a8ff", width = 1.5), inherit = FALSE) %>%
      add_lines(x = ~date, y = ~sma_50,  name = "SMA 50",
        line = list(color = "#ffa657", width = 1.5), inherit = FALSE) %>%
      add_lines(x = ~date, y = ~sma_200, name = "SMA 200",
        line = list(color = "#f78166", width = 1.5), inherit = FALSE)
  }

  p %>%
    layout(
      xaxis  = list(rangeslider = list(visible = FALSE)),
      yaxis  = list(title = "Price (USD)", tickprefix = "$"),
      legend = list(orientation = "h", y = -0.06)
    ) %>%
    apply_dark()
}
