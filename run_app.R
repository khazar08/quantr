# Run from the QuantR directory:  source("run_app.R")
# Or from anywhere:               shiny::runApp("path/to/QuantR")

if (!requireNamespace("shiny", quietly = TRUE)) source("install_packages.R")
shiny::runApp(".", launch.browser = TRUE)
