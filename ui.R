ui <- dashboardPage(
  skin = "black",

  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      style = "font-weight:700; font-size:17px; letter-spacing:-0.3px;",
      icon("chart-line", style = "color:#58a6ff; margin-right:6px;"),
      "QuantR"
    ),
    tags$li(
      class = "dropdown",
      style = "padding:14px 16px; color:#8b949e; font-size:11px; font-family:'Inter',sans-serif;",
      textOutput("header_status", inline = TRUE)
    )
  ),

  # ── Sidebar ──────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 250,
    tags$style(HTML("
      .sidebar-input-block { padding: 14px 16px 0; }
      .sidebar-input-block label { color:#8b949e !important; }
    ")),

    tags$div(class = "sidebar-input-block",
      pickerInput("tickers", tags$span(icon("layer-group"), " Assets"),
        choices  = c("AAPL","MSFT","GOOGL","AMZN","META","NVDA","TSLA",
                     "JPM","GS","BAC","SPY","QQQ","GLD","BTC-USD"),
        selected = DEFAULT_TICKERS,
        multiple = TRUE,
        options  = list(`live-search` = TRUE, `max-options` = 10,
                        style = "btn-dark btn-sm", title = "Select assets...")
      ),

      dateRangeInput("dates", tags$span(icon("calendar-alt"), " Date Range"),
        start = DEFAULT_START, end = DEFAULT_END),

      sliderInput("rf_rate", tags$span(icon("percentage"), " Risk-Free Rate (%)"),
        min = 0, max = 10, value = 5, step = 0.25),

      tags$hr(style = "border-color:#30363d; margin:12px 0;"),

      actionBttn("fetch_data", "Fetch Market Data",
        style = "gradient", color = "primary",
        icon  = icon("download"), size = "sm", block = TRUE),

      tags$hr(style = "border-color:#30363d; margin:12px 0;"),

      conditionalPanel("output.data_ready == true",
        tags$p(style = "font-size:10px; text-transform:uppercase; letter-spacing:0.6px; color:#8b949e; margin-bottom:8px;",
               "Monte Carlo"),
        numericInput("mc_sims",   "Simulations",     1000, min=100, max=10000, step=100),
        numericInput("mc_years",  "Horizon (years)",    1, min=0.25, max=5, step=0.25),
        numericInput("mc_invest", "Investment ($)", 10000, min=100, max=1e7, step=1000)
      )
    ),

    sidebarMenu(id = "tabs",
      menuItem("Overview",        tabName = "overview",   icon = icon("tachometer-alt")),
      menuItem("Market Analysis", tabName = "market",     icon = icon("chart-area")),
      menuItem("Portfolio",       tabName = "portfolio",  icon = icon("balance-scale")),
      menuItem("Risk Analysis",   tabName = "risk",       icon = icon("shield-alt")),
      menuItem("Monte Carlo",     tabName = "montecarlo", icon = icon("dice")),
      menuItem("Forecast",        tabName = "forecast",   icon = icon("magic"))
    )
  ),

  # ── Body ─────────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap")
    ),

    tabItems(

      # ── Overview ─────────────────────────────────────────────────────────────
      tabItem("overview",
        fluidRow(uiOutput("kpi_cards")),
        fluidRow(
          box(title = "Normalized Price Performance", width = 8,
              solidHeader = TRUE, status = "primary",
              plotlyOutput("perf_chart", height = "340px")),
          box(title = "Return Correlation Matrix", width = 4,
              solidHeader = TRUE, status = "primary",
              plotlyOutput("corr_heatmap", height = "340px"))
        ),
        fluidRow(
          box(title = "Daily Return Distributions", width = 6,
              solidHeader = TRUE, status = "info",
              plotlyOutput("ret_dist", height = "280px")),
          box(title = "60-Day Rolling Volatility (Annualized)", width = 6,
              solidHeader = TRUE, status = "info",
              plotlyOutput("roll_vol", height = "280px"))
        )
      ),

      # ── Market Analysis ───────────────────────────────────────────────────────
      tabItem("market",
        fluidRow(
          box(width = 12,
            fluidRow(
              column(3, selectInput("mk_ticker", "Asset", choices = DEFAULT_TICKERS)),
              column(3, checkboxGroupInput("mk_overlay", "Overlays",
                choices = c("SMA (20/50/200)" = "sma", "Bollinger Bands" = "bb"),
                selected = c("sma", "bb"), inline = FALSE)),
              column(3, checkboxGroupInput("mk_panels", "Sub-panels",
                choices = c("RSI (14)" = "rsi", "MACD" = "macd"),
                selected = c("rsi", "macd"), inline = FALSE))
            )
          )
        ),
        fluidRow(
          box(title = uiOutput("candle_title"), width = 12,
              solidHeader = TRUE, status = "primary",
              plotlyOutput("candlestick", height = "440px"))
        ),
        fluidRow(
          conditionalPanel("input.mk_panels.indexOf('rsi') >= 0",
            box(title = "RSI (14)", width = 6, solidHeader = TRUE, status = "warning",
                plotlyOutput("rsi_plot", height = "200px"))
          ),
          conditionalPanel("input.mk_panels.indexOf('macd') >= 0",
            box(title = "MACD (12, 26, 9)", width = 6, solidHeader = TRUE, status = "warning",
                plotlyOutput("macd_plot", height = "200px"))
          )
        ),
        fluidRow(
          box(title = "Asset Statistics (Full Period)", width = 12,
              solidHeader = TRUE, status = "info",
              DTOutput("stats_table"))
        )
      ),

      # ── Portfolio ─────────────────────────────────────────────────────────────
      tabItem("portfolio",
        fluidRow(
          box(title = "Efficient Frontier", width = 8,
              solidHeader = TRUE, status = "primary",
              plotlyOutput("frontier_plot", height = "440px")),
          box(title = "Optimal Portfolio Composition", width = 4,
              solidHeader = TRUE, status = "primary",
              tags$p(style="color:#8b949e;font-size:12px;margin:0 0 8px;",
                     icon("star"), " Maximum Sharpe Ratio weights"),
              plotlyOutput("weights_pie", height = "230px"),
              tags$hr(style="border-color:#30363d;"),
              tableOutput("portfolio_stats_tbl"))
        ),
        fluidRow(
          box(title = "Portfolio vs Equal-Weight vs SPY Benchmark", width = 12,
              solidHeader = TRUE, status = "info",
              plotlyOutput("port_vs_bench", height = "320px"))
        )
      ),

      # ── Risk Analysis ─────────────────────────────────────────────────────────
      tabItem("risk",
        fluidRow(uiOutput("risk_kpis")),
        fluidRow(
          box(title = "Return Distribution + VaR Thresholds", width = 6,
              solidHeader = TRUE, status = "danger",
              plotlyOutput("var_plot", height = "340px")),
          box(title = "Underwater (Drawdown) Chart", width = 6,
              solidHeader = TRUE, status = "danger",
              plotlyOutput("drawdown_plot", height = "340px"))
        ),
        fluidRow(
          box(title = "Rolling 60-Day VaR (95%)", width = 6,
              solidHeader = TRUE, status = "warning",
              plotlyOutput("roll_var_plot", height = "290px")),
          box(title = "Full Performance Metrics", width = 6,
              solidHeader = TRUE, status = "warning",
              selectInput("risk_ticker", "Select Asset", choices = DEFAULT_TICKERS),
              DTOutput("metrics_tbl"))
        )
      ),

      # ── Monte Carlo ───────────────────────────────────────────────────────────
      tabItem("montecarlo",
        fluidRow(
          box(width = 12,
            fluidRow(
              column(4, selectInput("mc_ticker", "Asset to Simulate",
                                   choices = DEFAULT_TICKERS)),
              column(4, sliderInput("mc_conf", "Confidence Band",
                                   min = 80, max = 99, value = 90, post = "%")),
              column(4, br(), actionBttn("run_mc", "Run Simulation",
                                        style = "gradient", color = "success",
                                        icon = icon("play"), size = "sm", block = TRUE))
            )
          )
        ),
        fluidRow(
          box(title = "Simulated Price Paths (GBM)", width = 8,
              solidHeader = TRUE, status = "success",
              plotlyOutput("mc_paths_plot", height = "400px")),
          box(title = "Distribution of Final Values", width = 4,
              solidHeader = TRUE, status = "success",
              plotlyOutput("mc_dist_plot", height = "400px"))
        ),
        fluidRow(
          box(title = "Simulation Summary", width = 12,
              solidHeader = TRUE, status = "info",
              DTOutput("mc_table"))
        )
      ),

      # ── Forecast ─────────────────────────────────────────────────────────────
      tabItem("forecast",
        fluidRow(
          box(width = 12,
            fluidRow(
              column(3, selectInput("fc_ticker", "Asset", choices = DEFAULT_TICKERS)),
              column(3, numericInput("fc_h", "Horizon (trading days)",
                                    30, min = 5, max = 252, step = 5)),
              column(3, selectInput("fc_model", "Model",
                choices = c("Auto ARIMA" = "arima", "ETS" = "ets", "Theta" = "theta"))),
              column(3, br(), actionBttn("run_fc", "Fit & Forecast",
                                        style = "gradient", color = "royal",
                                        icon = icon("magic"), size = "sm", block = TRUE))
            )
          )
        ),
        fluidRow(
          box(title = "Price Forecast with Confidence Intervals", width = 9,
              solidHeader = TRUE, status = "primary",
              plotlyOutput("fc_chart", height = "390px")),
          box(title = "Model Details", width = 3,
              solidHeader = TRUE, status = "primary",
              verbatimTextOutput("fc_model_info"))
        ),
        fluidRow(
          box(title = "STL Decomposition (Trend)", width = 6,
              solidHeader = TRUE, status = "info",
              plotlyOutput("decomp_plot", height = "320px")),
          box(title = "Autocorrelation (ACF / PACF)", width = 6,
              solidHeader = TRUE, status = "info",
              plotlyOutput("acf_plot", height = "320px"))
        )
      )
    )
  )
)
