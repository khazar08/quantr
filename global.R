suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(tidyverse)
  library(quantmod)
  library(PerformanceAnalytics)
  library(plotly)
  library(DT)
  library(quadprog)
  library(forecast)
  library(xts)
  library(zoo)
  library(scales)
  library(TTR)
  library(lubridate)
  library(moments)
})

source("R/portfolio.R")
source("R/risk.R")
source("R/montecarlo.R")
source("R/technical.R")

DEFAULT_TICKERS <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META")
DEFAULT_START   <- Sys.Date() - 730
DEFAULT_END     <- Sys.Date()

ASSET_COLORS <- c(
  "#58a6ff", "#3fb950", "#d2a8ff", "#ffa657", "#f78166",
  "#79c0ff", "#7ee787", "#e3b341", "#ff7b72", "#a5d6ff"
)

dark_layout <- function() {
  list(
    paper_bgcolor = "#0d1117",
    plot_bgcolor  = "#0d1117",
    font = list(color = "#8b949e", family = "'Inter', sans-serif", size = 12),
    xaxis = list(
      gridcolor     = "rgba(48,54,61,0.7)",
      linecolor     = "#30363d",
      tickcolor     = "#30363d",
      tickfont      = list(color = "#8b949e"),
      zerolinecolor = "#30363d"
    ),
    yaxis = list(
      gridcolor     = "rgba(48,54,61,0.7)",
      linecolor     = "#30363d",
      tickcolor     = "#30363d",
      tickfont      = list(color = "#8b949e"),
      zerolinecolor = "#30363d"
    ),
    legend = list(
      bgcolor     = "rgba(22,27,34,0.95)",
      bordercolor = "#30363d",
      borderwidth = 1,
      font        = list(color = "#c9d1d9")
    ),
    margin     = list(l = 55, r = 20, t = 35, b = 45),
    hoverlabel = list(
      bgcolor     = "#161b22",
      bordercolor = "#30363d",
      font        = list(color = "#e6edf3", family = "'Inter', sans-serif")
    )
  )
}

apply_dark <- function(p) {
  p %>%
    layout(!!!dark_layout()) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

format_pct <- function(x, digits = 2) paste0(round(x * 100, digits), "%")
