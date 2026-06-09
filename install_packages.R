pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "quantmod", "PerformanceAnalytics",
  "plotly", "DT", "quadprog", "forecast",
  "xts", "zoo", "scales", "TTR", "lubridate", "moments"
)
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, dependencies = TRUE)
cat("All packages ready.\n")
