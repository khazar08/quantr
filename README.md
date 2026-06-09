# QuantR — Quantitative Finance Analytics Dashboard

A multi-tab interactive Shiny dashboard for quantitative financial analysis. Pulls live data from Yahoo Finance and combines portfolio optimization, risk analytics, Monte Carlo simulation, technical analysis, and time-series forecasting in a single dark-themed interface.

---

## Features

### Market Analysis
- Candlestick chart with volume overlay
- Toggleable SMA (20 / 50 / 200-day), Bollinger Bands
- RSI (14) and MACD (12, 26, 9) sub-panels
- Full statistics table per asset

### Portfolio Optimization (Markowitz)
- Efficient frontier computed via quadratic programming
- Maximum Sharpe Ratio and Minimum Variance portfolios highlighted
- Optimal weight allocation pie chart
- Cumulative performance vs equal-weight and SPY benchmark

### Risk Analysis
- Historical VaR (95% / 99%) and CVaR (Expected Shortfall)
- Underwater / drawdown chart
- Rolling 60-day VaR
- 13-metric performance table: Sharpe, Sortino, Calmar, skewness, kurtosis, best/worst day, and more

### Monte Carlo Simulation (GBM)
- Geometric Brownian Motion with up to 10,000 paths
- Fan chart with configurable confidence bands and IQR
- Final portfolio value distribution with scenario table

### Forecasting
- Auto ARIMA, ETS, and Theta models
- 80% and 95% confidence intervals
- STL decomposition trend overlay
- ACF / PACF correlogram

### Overview
- Per-asset KPI cards (total return, volatility, Sharpe)
- Normalized multi-asset performance chart
- Return correlation heatmap
- Return distribution histograms and rolling volatility

---

## Setup

### 1. Install dependencies

```r
source("install_packages.R")
```

Packages used: `shiny`, `shinydashboard`, `shinyWidgets`, `tidyverse`, `quantmod`, `PerformanceAnalytics`, `plotly`, `DT`, `quadprog`, `forecast`, `xts`, `zoo`, `TTR`, `moments`, `scales`, `lubridate`.

### 2. Launch

```r
shiny::runApp(".", launch.browser = TRUE)
```

Or open `quantr.Rproj` in RStudio and run `source("run_app.R")`.

> **Note:** An internet connection is required — data is fetched live from Yahoo Finance via `quantmod::getSymbols()`.

---

## Usage

1. Select assets from the sidebar picker (supports stocks, ETFs, crypto)
2. Set the date range and risk-free rate
3. Click **Fetch Market Data**
4. Navigate between tabs to explore the analysis
5. On the Monte Carlo tab, configure simulations and click **Run Simulation**
6. On the Forecast tab, choose a model and click **Fit & Forecast**

---

## Project Structure

```
QuantR/
├── global.R              # Package loading, constants, shared theme helpers
├── ui.R                  # Shiny UI (shinydashboard, 6 tabs)
├── server.R              # Reactive server logic (~500 lines)
├── run_app.R             # One-liner launcher
├── install_packages.R    # Dependency installer
├── R/
│   ├── portfolio.R       # Markowitz efficient frontier (quadprog)
│   ├── risk.R            # VaR, CVaR, rolling risk, full metrics table
│   ├── montecarlo.R      # GBM simulation, path bands, summary table
│   └── technical.R       # OHLCV helpers, SMA/BB/RSI/MACD, candlestick builder
└── www/
    └── custom.css        # Dark GitHub-style theme (~250 lines)
```

---

## Tech Stack

| Layer | Libraries |
|-------|-----------|
| UI framework | `shiny`, `shinydashboard`, `shinyWidgets` |
| Data | `quantmod`, `xts`, `zoo` |
| Optimization | `quadprog` |
| Risk / performance | `PerformanceAnalytics`, `moments` |
| Charting | `plotly` |
| Forecasting | `forecast` (ARIMA, ETS, Theta) |
| Technical indicators | `TTR` |
| Data wrangling | `tidyverse` |

---

## License

MIT
