simulate_gbm <- function(S0, mu, sigma, T_years = 1, n_steps = 252, n_sims = 1000) {
  dt      <- T_years / n_steps
  Z       <- matrix(rnorm(n_steps * n_sims), nrow = n_steps)
  log_inc <- (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z
  paths   <- rbind(rep(S0, n_sims), apply(log_inc, 2, function(r) S0 * cumprod(exp(r))))

  list(
    paths     = paths,
    final     = paths[nrow(paths), ],
    time_axis = seq(0, T_years, length.out = n_steps + 1),
    S0 = S0, mu = mu, sigma = sigma
  )
}

path_bands <- function(sim, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  mat <- t(apply(sim$paths, 1, quantile, probs = probs, na.rm = TRUE))
  as.data.frame(mat) %>%
    setNames(paste0("p", probs * 100)) %>%
    mutate(time = sim$time_axis)
}

mc_summary_table <- function(sim, investment = 10000) {
  vals <- sim$final / sim$S0 * investment
  tibble(
    Percentile = c("5th", "25th", "50th (Median)", "75th", "95th", "Mean"),
    `Final Value`     = dollar(c(quantile(vals, c(0.05, 0.25, 0.5, 0.75, 0.95)), mean(vals))),
    `P&L`             = dollar(c(quantile(vals, c(0.05, 0.25, 0.5, 0.75, 0.95)), mean(vals)) - investment),
    `Return`          = format_pct(c(quantile(vals, c(0.05, 0.25, 0.5, 0.75, 0.95)), mean(vals)) / investment - 1)
  )
}
