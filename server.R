server <- function(input, output, session) {

  rv <- reactiveValues(
    prices   = NULL,
    returns  = NULL,
    frontier = NULL,
    mc_sim   = NULL,
    fc_out   = NULL
  )

  observeEvent(input$fetch_data, {
    req(length(input$tickers) >= 2)

    withProgress(message = "Fetching data from Yahoo Finance…", value = 0, {
      prices_list <- list()
      for (i in seq_along(input$tickers)) {
        sym <- input$tickers[i]
        incProgress(1 / length(input$tickers), detail = sym)
        tryCatch({
          prices_list[[sym]] <- getSymbols(
            sym, from = input$dates[1], to = input$dates[2],
            auto.assign = FALSE, warnings = FALSE)
        }, error = function(e) {
          showNotification(paste("Could not fetch", sym), type = "warning", duration = 4)
        })
      }
      if (length(prices_list) < 2) {
        showNotification("Need at least 2 assets. Check connection.", type = "error"); return()
      }
      rv$prices <- prices_list

      # Aligned adjusted-close matrix
      adj <- do.call(merge, lapply(prices_list, Ad))
      colnames(adj) <- names(prices_list)
      adj <- adj[!apply(is.na(coredata(adj)), 1, all), ]
      rv$returns <- diff(log(adj))[-1, ]

      # Update all selectors
      tks <- names(prices_list)
      updateSelectInput(session, "mk_ticker",   choices = tks, selected = tks[1])
      updateSelectInput(session, "risk_ticker", choices = tks, selected = tks[1])
      updateSelectInput(session, "mc_ticker",   choices = tks, selected = tks[1])
      updateSelectInput(session, "fc_ticker",   choices = tks, selected = tks[1])

      # Efficient frontier (background)
      rv$frontier <- tryCatch(
        compute_efficient_frontier(coredata(rv$returns)),
        error = function(e) NULL)
    })

    showNotification(
      paste0(icon("check"), " Loaded ", length(rv$prices), " assets"),
      type = "message", duration = 3)
  })

  output$data_ready <- reactive(!is.null(rv$prices))
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  output$header_status <- renderText({
    if (is.null(rv$prices)) "No data loaded" else
      paste0(length(rv$prices), " assets  |  ",
             format(min(index(rv$returns)), "%b %d %Y"), " – ",
             format(max(index(rv$returns)), "%b %d %Y"))
  })

  output$kpi_cards <- renderUI({
    req(rv$returns)
    tks <- colnames(rv$returns)
    cards <- imap(tks, function(tk, i) {
      r         <- as.numeric(rv$returns[, tk]); r <- r[!is.na(r)]
      total_ret <- prod(1 + r) - 1
      ann_vol   <- sd(r) * sqrt(252)
      sharpe    <- (mean(r) * 252) / ann_vol
      col       <- ASSET_COLORS[(i - 1) %% length(ASSET_COLORS) + 1]
      sign      <- if (total_ret >= 0) "+" else ""
      cls       <- if (total_ret >= 0) "kpi-pos" else "kpi-neg"
      column(width = max(2, floor(12 / length(tks))),
        tags$div(class = "kpi-card", style = paste0("border-top:3px solid ", col, ";"),
          tags$div(class = "kpi-ticker", tk),
          tags$div(class = paste("kpi-ret", cls),
                   paste0(sign, round(total_ret * 100, 1), "%")),
          tags$div(class = "kpi-lbl", "Total Return"),
          tags$hr(style = "margin:7px 0; border-color:#30363d;"),
          tags$div(class = "kpi-meta",
            tags$span(paste0("σ ", round(ann_vol*100,1), "%")),
            tags$span(paste0("SR ", round(sharpe, 2)))
          )
        )
      )
    })
    fluidRow(!!!cards)
  })

  output$perf_chart <- renderPlotly({
    req(rv$prices)
    p <- plot_ly()
    for (i in seq_along(rv$prices)) {
      tk  <- names(rv$prices)[i]
      px  <- as.numeric(Ad(rv$prices[[tk]]))
      cum <- px / px[1] - 1
      p   <- add_lines(p, x = index(Ad(rv$prices[[tk]])), y = cum * 100,
               name = tk, line = list(color = ASSET_COLORS[i], width = 2),
               hovertemplate = paste0("<b>", tk, "</b><br>%{x|%b %d %Y}<br>%{y:.2f}%<extra></extra>"))
    }
    p %>%
      layout(yaxis = list(title = "Return (%)", ticksuffix = "%"), xaxis = list(title = ""),
             hovermode = "x unified") %>% apply_dark()
  })

  output$corr_heatmap <- renderPlotly({
    req(rv$returns)
    cm  <- cor(coredata(rv$returns), use = "pairwise.complete.obs")
    tks <- colnames(cm)
    plot_ly(x = tks, y = tks, z = cm, type = "heatmap",
      colorscale = list(c(0,"#f78166"), c(0.5,"#21262d"), c(1,"#58a6ff")),
      zmin = -1, zmax = 1,
      text = round(cm, 2), texttemplate = "%{text}",
      hovertemplate = "%{x} / %{y}: %{z:.3f}<extra></extra>") %>%
      layout(margin = list(l=70,r=10,t=10,b=70)) %>% apply_dark()
  })

  output$ret_dist <- renderPlotly({
    req(rv$returns)
    p <- plot_ly()
    for (i in seq_along(colnames(rv$returns))) {
      tk <- colnames(rv$returns)[i]
      r  <- as.numeric(rv$returns[, tk]); r <- r[!is.na(r)]
      p  <- add_histogram(p, x = r * 100, name = tk, opacity = 0.55,
               marker = list(color = ASSET_COLORS[i]), nbinsx = 55)
    }
    p %>% layout(barmode = "overlay",
      xaxis = list(title = "Daily Return (%)", ticksuffix = "%"),
      yaxis = list(title = "Frequency")) %>% apply_dark()
  })

  output$roll_vol <- renderPlotly({
    req(rv$returns)
    p <- plot_ly()
    for (i in seq_along(colnames(rv$returns))) {
      tk  <- colnames(rv$returns)[i]
      r   <- rv$returns[, tk]
      rvv <- rollapply(r, 60, sd, fill = NA, na.rm = TRUE) * sqrt(252)
      p   <- add_lines(p, x = index(rvv), y = as.numeric(rvv) * 100, name = tk,
               line = list(color = ASSET_COLORS[i], width = 1.5))
    }
    p %>% layout(yaxis = list(title = "Vol (%)", ticksuffix = "%"),
                 xaxis = list(title = ""), hovermode = "x unified") %>% apply_dark()
  })

  mk_df <- reactive({
    req(rv$prices, input$mk_ticker %in% names(rv$prices))
    ohlcv_to_df(rv$prices[[input$mk_ticker]]) %>% add_indicators()
  })

  output$candle_title <- renderUI({
    req(input$mk_ticker)
    tags$span(icon("chart-candlestick"), " ", input$mk_ticker, " — Candlestick")
  })

  output$candlestick <- renderPlotly({
    req(mk_df())
    make_candlestick(mk_df(), input$mk_ticker,
      show_sma = "sma" %in% input$mk_overlay,
      show_bb  = "bb"  %in% input$mk_overlay)
  })

  output$rsi_plot <- renderPlotly({
    req(mk_df())
    df <- mk_df()
    n  <- nrow(df)
    plot_ly(df, x = ~date) %>%
      add_lines(y = ~rsi_14, name = "RSI", line = list(color = "#d2a8ff", width = 1.5)) %>%
      add_lines(y = rep(70, n), name = "OB", line = list(color="#f78166",dash="dot",width=1),
                showlegend = FALSE) %>%
      add_lines(y = rep(30, n), name = "OS", line = list(color="#3fb950",dash="dot",width=1),
                showlegend = FALSE) %>%
      add_ribbons(x = ~date, ymin = rep(30,n), ymax = rep(70,n), inherit = FALSE,
        fillcolor = "rgba(88,166,255,0.05)", line = list(color="transparent"),
        showlegend = FALSE) %>%
      layout(yaxis = list(title = "RSI", range = c(0,100)), xaxis = list(title = ""),
             showlegend = FALSE) %>% apply_dark()
  })

  output$macd_plot <- renderPlotly({
    req(mk_df())
    df  <- mk_df()
    mac <- get_macd_df(df$adj, df$date)
    bar_cols <- ifelse(!is.na(mac$histogram) & mac$histogram >= 0, "#3fb950", "#f78166")
    plot_ly(mac, x = ~date) %>%
      add_bars(y = ~histogram, name = "Histogram",
               marker = list(color = bar_cols, opacity = 0.8)) %>%
      add_lines(y = ~macd,   name = "MACD",   line = list(color="#58a6ff",width=1.5)) %>%
      add_lines(y = ~signal, name = "Signal", line = list(color="#ffa657",width=1.5)) %>%
      layout(yaxis = list(title = "MACD"), xaxis = list(title = "")) %>% apply_dark()
  })

  output$stats_table <- renderDT({
    req(rv$returns, rv$prices)
    tks <- colnames(rv$returns)
    df  <- map_df(tks, function(tk) {
      r       <- as.numeric(rv$returns[, tk]); r <- r[!is.na(r)]
      last_px <- as.numeric(last(Ad(rv$prices[[tk]])))
      tibble(
        Asset            = tk,
        `Last Price`     = dollar(last_px),
        `Total Return`   = format_pct(prod(1 + r) - 1),
        `Ann. Return`    = format_pct(mean(r) * 252),
        `Ann. Vol`       = format_pct(sd(r) * sqrt(252)),
        `Sharpe`         = round((mean(r)*252)/(sd(r)*sqrt(252)), 3),
        `Max DD`         = format_pct(as.numeric(maxDrawdown(
                              xts(r, order.by=seq.Date(Sys.Date()-length(r),by="day",length.out=length(r)))))),
        `Skew`           = round(moments::skewness(r), 3),
        `Kurt`           = round(moments::kurtosis(r) - 3, 3)
      )
    })
    datatable(df, rownames = FALSE, class = "display compact",
      options = list(dom = "t", pageLength = 20)) %>%
      formatStyle("Total Return", color = styleInterval(c(-0.0001, 0.0001),
                                                        c("#f78166","#8b949e","#3fb950")))
  })

  output$frontier_plot <- renderPlotly({
    req(rv$frontier, rv$returns)
    front  <- rv$frontier
    rf     <- input$rf_rate / 100
    ms     <- find_max_sharpe(front, rf)
    mv     <- find_min_var(front)

    # Individual asset dots
    ret_mat <- coredata(rv$returns)
    assets <- map_df(colnames(ret_mat), function(tk) {
      r <- ret_mat[, tk]; r <- r[!is.na(r)]
      tibble(ticker=tk, ret=mean(r)*252, vol=sd(r)*sqrt(252))
    })

    plot_ly() %>%
      add_markers(data = front, x = ~vol, y = ~ret, color = ~sharpe,
        colors = "viridis", size = 4, sizes = c(3,3),
        marker = list(showscale = TRUE, colorbar = list(title="Sharpe",
          thickness=10, tickfont=list(color="#8b949e"), titlefont=list(color="#8b949e"))),
        showlegend = FALSE,
        hovertemplate = "Vol: %{x:.2%}<br>Return: %{y:.2%}<extra></extra>") %>%
      add_lines(data = front, x = ~vol, y = ~ret,
        line = list(color="#58a6ff", width=2), name="Frontier",
        hovertemplate = "Vol: %{x:.2%}<br>Return: %{y:.2%}<extra></extra>") %>%
      add_markers(data = ms, x = ~vol, y = ~ret,
        marker = list(color="#3fb950",size=16,symbol="star",
          line=list(color="white",width=2)), name="Max Sharpe") %>%
      add_markers(data = mv, x = ~vol, y = ~ret,
        marker = list(color="#d2a8ff",size=14,symbol="diamond",
          line=list(color="white",width=2)), name="Min Variance") %>%
      add_markers(data = assets, x = ~vol, y = ~ret, text = ~ticker,
        marker = list(color="#ffa657",size=10,line=list(color="white",width=1.5)),
        mode = "markers+text", textposition = "top right",
        textfont = list(color="#c9d1d9",size=11), name="Assets",
        hovertemplate = "<b>%{text}</b><br>Vol: %{x:.2%}<br>Ret: %{y:.2%}<extra></extra>") %>%
      layout(
        xaxis = list(title="Annualized Volatility", tickformat=".0%"),
        yaxis = list(title="Annualized Return", tickformat=".0%"),
        hovermode = "closest") %>% apply_dark()
  })

  output$weights_pie <- renderPlotly({
    req(rv$frontier)
    ms <- find_max_sharpe(rv$frontier, input$rf_rate/100)
    if (nrow(ms) == 0) return(plotly_empty())
    w  <- ms$weights[[1]]; w <- w[w > 0.005]
    plot_ly(labels=names(w), values=w, type="pie",
      marker = list(colors=ASSET_COLORS[seq_along(w)],
                    line=list(color="#0d1117",width=2)),
      textinfo="label+percent",
      hovertemplate="<b>%{label}</b>: %{percent}<extra></extra>") %>%
      layout(showlegend=FALSE, margin=list(t=0,b=0,l=0,r=0)) %>% apply_dark()
  })

  output$portfolio_stats_tbl <- renderTable({
    req(rv$frontier)
    rf <- input$rf_rate / 100
    ms <- find_max_sharpe(rv$frontier, rf)
    mv <- find_min_var(rv$frontier)
    tibble(
      Portfolio  = c("Max Sharpe", "Min Variance"),
      Return     = c(format_pct(ms$ret), format_pct(mv$ret)),
      Volatility = c(format_pct(ms$vol), format_pct(mv$vol)),
      Sharpe     = c(round((ms$ret-rf)/ms$vol,3), round((mv$ret-rf)/mv$vol,3))
    )
  }, striped=FALSE, hover=TRUE, bordered=FALSE, spacing="s", width="100%")

  output$port_vs_bench <- renderPlotly({
    req(rv$frontier, rv$returns)
    ms      <- find_max_sharpe(rv$frontier, input$rf_rate/100)
    if (nrow(ms) == 0) return(plotly_empty())
    w       <- ms$weights[[1]]
    ret_mat <- coredata(rv$returns)
    valid   <- intersect(names(w), colnames(ret_mat))
    wv      <- w[valid]; wv <- wv / sum(wv)
    pr      <- ret_mat[, valid] %*% wv
    ew      <- rowMeans(ret_mat[, valid], na.rm = TRUE)
    dates   <- index(rv$returns)

    p <- plot_ly(x = dates) %>%
      add_lines(y = (cumprod(1+pr)-1)*100, name="Optimal (Max Sharpe)",
                line=list(color="#3fb950",width=2.5)) %>%
      add_lines(y = (cumprod(1+ew)-1)*100, name="Equal Weight",
                line=list(color="#58a6ff",width=2,dash="dash"))

    tryCatch({
      spy  <- getSymbols("SPY", from=min(dates), to=max(dates), auto.assign=FALSE, warnings=FALSE)
      spya <- as.numeric(Ad(spy))
      p    <- add_lines(p, x=index(spy), y=(spya/spya[1]-1)*100, name="SPY",
                        line=list(color="#ffa657",width=1.5,dash="dot"))
    }, error=function(e) NULL)

    p %>% layout(yaxis=list(title="Return (%)", ticksuffix="%"),
                 xaxis=list(title=""), hovermode="x unified") %>% apply_dark()
  })

  risk_r <- reactive({
    req(rv$returns, input$risk_ticker %in% colnames(rv$returns))
    r <- as.numeric(rv$returns[, input$risk_ticker]); r[!is.na(r)]
  })

  output$risk_kpis <- renderUI({
    req(risk_r())
    r   <- risk_r(); rf <- input$rf_rate/100
    ar  <- mean(r)*252; av <- sd(r)*sqrt(252)
    sh  <- (ar-rf)/av
    v95 <- compute_var(r); cv95 <- compute_cvar(r)
    make <- function(val, lbl, col)
      tags$div(class="risk-kpi", style=paste0("border-left:4px solid ",col,";"),
        tags$div(class="risk-val", val), tags$div(class="risk-lbl", lbl))
    fluidRow(
      column(2, make(format_pct(ar),  "Ann. Return",    if(ar>=0)"#3fb950" else "#f78166")),
      column(2, make(format_pct(av),  "Ann. Volatility","#58a6ff")),
      column(2, make(round(sh,3),     "Sharpe Ratio",   if(sh>=1)"#3fb950" else if(sh>=0)"#ffa657" else "#f78166")),
      column(2, make(format_pct(v95), "VaR (95%)",      "#d2a8ff")),
      column(2, make(format_pct(cv95),"CVaR (95%)",     "#f78166")),
      column(2, make(input$risk_ticker,"Asset",         "#8b949e"))
    )
  })

  output$var_plot <- renderPlotly({
    req(risk_r())
    r <- risk_r(); v95 <- compute_var(r); v99 <- compute_var(r, 0.99)
    plot_ly(x = r*100, type="histogram", nbinsx=60, name="Returns",
      marker=list(color="rgba(88,166,255,0.6)")) %>%
      layout(
        shapes = list(
          list(type="line",x0=v95*100,x1=v95*100,y0=0,y1=1,yref="paper",
               line=list(color="#ffa657",dash="dash",width=2)),
          list(type="line",x0=v99*100,x1=v99*100,y0=0,y1=1,yref="paper",
               line=list(color="#f78166",dash="dash",width=2))
        ),
        annotations = list(
          list(x=v95*100,y=0.9,yref="paper",text="VaR 95%",showarrow=TRUE,
               arrowcolor="#ffa657",font=list(color="#ffa657")),
          list(x=v99*100,y=0.75,yref="paper",text="VaR 99%",showarrow=TRUE,
               arrowcolor="#f78166",font=list(color="#f78166"))
        ),
        xaxis=list(title="Daily Return (%)",ticksuffix="%"),
        yaxis=list(title="Frequency"), showlegend=FALSE) %>% apply_dark()
  })

  output$drawdown_plot <- renderPlotly({
    req(rv$returns, input$risk_ticker %in% colnames(rv$returns))
    r  <- rv$returns[, input$risk_ticker]
    dd <- Drawdowns(r)
    plot_ly(x=index(dd), y=as.numeric(dd)*100, type="scatter", mode="lines",
      fill="tozeroy", fillcolor="rgba(247,129,102,0.15)",
      line=list(color="#f78166",width=1.5), name="Drawdown",
      hovertemplate="%{x|%b %d %Y}: %{y:.2f}%<extra></extra>") %>%
      layout(yaxis=list(title="Drawdown (%)",ticksuffix="%"),
             xaxis=list(title=""), showlegend=FALSE) %>% apply_dark()
  })

  output$roll_var_plot <- renderPlotly({
    req(rv$returns, input$risk_ticker %in% colnames(rv$returns))
    r   <- as.numeric(rv$returns[, input$risk_ticker])
    rv_ <- rolling_var(r, window=60)
    dates <- index(rv$returns)
    plot_ly(x=dates, y=rv_*100, type="scatter", mode="lines",
      line=list(color="#d2a8ff",width=1.5), name="Rolling VaR 95%",
      hovertemplate="%{x|%b %d %Y}: %{y:.3f}%<extra></extra>") %>%
      layout(yaxis=list(title="VaR (%)",ticksuffix="%"),
             xaxis=list(title=""), showlegend=FALSE) %>% apply_dark()
  })

  output$metrics_tbl <- renderDT({
    req(risk_r())
    full_metrics(risk_r(), input$rf_rate/100) %>%
      datatable(rownames=FALSE, class="display compact",
                options=list(dom="t", pageLength=20))
  })

  observeEvent(input$run_mc, {
    req(rv$prices, rv$returns, input$mc_ticker %in% colnames(rv$returns))
    r     <- as.numeric(rv$returns[, input$mc_ticker]); r <- r[!is.na(r)]
    S0    <- as.numeric(last(Ad(rv$prices[[input$mc_ticker]])))
    mu    <- mean(r) * 252; sigma <- sd(r) * sqrt(252)
    n_s   <- round(input$mc_years * 252)

    withProgress(message = "Running Monte Carlo simulation…", {
      rv$mc_sim <- simulate_gbm(S0, mu, sigma, input$mc_years, n_s, input$mc_sims)
    })
  })

  output$mc_paths_plot <- renderPlotly({
    req(rv$mc_sim)
    sim  <- rv$mc_sim
    pct  <- (100 - input$mc_conf) / 100
    band <- path_bands(sim, probs = c(pct, 0.25, 0.5, 0.75, 1-pct))

    p <- plot_ly()

    # Plot 120 random paths (dimmed)
    idx <- sample(ncol(sim$paths), min(120, ncol(sim$paths)))
    for (j in idx)
      p <- add_lines(p, x=sim$time_axis, y=sim$paths[,j],
        line=list(color="rgba(88,166,255,0.04)",width=0.4),
        showlegend=FALSE, hoverinfo="skip")

    ci_lo <- names(band)[1]; ci_hi <- names(band)[5]
    p %>%
      add_ribbons(data=band, x=~time, ymin=band[[ci_lo]], ymax=band[[ci_hi]],
        fillcolor="rgba(88,166,255,0.10)", line=list(color="transparent"),
        name=paste0(input$mc_conf,"% CI")) %>%
      add_ribbons(data=band, x=~time, ymin=~p25, ymax=~p75,
        fillcolor="rgba(88,166,255,0.22)", line=list(color="transparent"),
        name="IQR") %>%
      add_lines(data=band, x=~time, y=band$p50,
        line=list(color="#3fb950",width=2.5), name="Median") %>%
      add_lines(x=sim$time_axis, y=rep(sim$S0, length(sim$time_axis)),
        line=list(color="#ffa657",dash="dot",width=1.5), name="Initial Price") %>%
      layout(
        yaxis=list(title=paste(input$mc_ticker,"Price (USD)"),tickprefix="$"),
        xaxis=list(title="Years"), hovermode="x unified") %>% apply_dark()
  })

  output$mc_dist_plot <- renderPlotly({
    req(rv$mc_sim)
    vals <- rv$mc_sim$final / rv$mc_sim$S0 * input$mc_invest
    med  <- median(vals)
    plot_ly(x=vals, type="histogram", nbinsx=60,
      marker=list(color="rgba(63,185,80,0.65)",line=list(color="#3fb950",width=0.5))) %>%
      layout(
        shapes = list(
          list(type="line",x0=input$mc_invest,x1=input$mc_invest,y0=0,y1=1,yref="paper",
               line=list(color="#ffa657",dash="dash",width=2)),
          list(type="line",x0=med,x1=med,y0=0,y1=1,yref="paper",
               line=list(color="#3fb950",width=2))
        ),
        annotations = list(
          list(x=input$mc_invest,y=0.9,yref="paper",text="Initial",showarrow=TRUE,
               arrowcolor="#ffa657",font=list(color="#ffa657")),
          list(x=med,y=0.75,yref="paper",text="Median",showarrow=TRUE,
               arrowcolor="#3fb950",font=list(color="#3fb950"))
        ),
        xaxis=list(title="Final Portfolio Value",tickprefix="$"),
        yaxis=list(title="Frequency"), showlegend=FALSE) %>% apply_dark()
  })

  output$mc_table <- renderDT({
    req(rv$mc_sim)
    mc_summary_table(rv$mc_sim, input$mc_invest) %>%
      datatable(rownames=FALSE, class="display compact",
                options=list(dom="t", pageLength=10))
  })

  # ── Forecast ─────────────────────────────────────────────────────────────────
  observeEvent(input$run_fc, {
    req(rv$prices, input$fc_ticker %in% names(rv$prices))
    px     <- Ad(rv$prices[[input$fc_ticker]])
    log_px <- log(as.numeric(px))

    withProgress(message = "Fitting model…", {
      ts_dat <- ts(log_px, frequency = 252)
      model  <- switch(input$fc_model,
        "arima" = auto.arima(ts_dat, stepwise=TRUE, approximation=TRUE),
        "ets"   = ets(ts_dat),
        "theta" = thetaf(ts_dat, h=input$fc_h)
      )
      fc <- if (input$fc_model == "theta") model else
              forecast(model, h=input$fc_h, level=c(80,95))

      rv$fc_out <- list(model=model, fc=fc, px=px, log_px=log_px,
                        ticker=input$fc_ticker, h=input$fc_h)
    })
  })

  output$fc_chart <- renderPlotly({
    req(rv$fc_out)
    fd    <- rv$fc_out; fc <- fd$fc; px <- fd$px
    dates <- index(px)
    last  <- max(dates)
    fd_dates <- seq(last+1, by="day", length.out=fd$h*2)
    fd_dates <- fd_dates[!weekdays(fd_dates) %in% c("Saturday","Sunday")][seq_len(fd$h)]
    fc_mean  <- exp(as.numeric(fc$mean)[seq_len(length(fd_dates))])

    p <- plot_ly() %>%
      add_lines(x=dates, y=as.numeric(px), name="Historical",
                line=list(color="#58a6ff",width=1.5))

    if (!is.null(fc$upper)) {
      n <- length(fd_dates)
      h95 <- exp(as.numeric(fc$upper[,2])[seq_len(n)])
      l95 <- exp(as.numeric(fc$lower[,2])[seq_len(n)])
      h80 <- exp(as.numeric(fc$upper[,1])[seq_len(n)])
      l80 <- exp(as.numeric(fc$lower[,1])[seq_len(n)])
      p <- p %>%
        add_ribbons(x=fd_dates,ymin=l95,ymax=h95,name="95% CI",
          fillcolor="rgba(88,166,255,0.12)",line=list(color="transparent")) %>%
        add_ribbons(x=fd_dates,ymin=l80,ymax=h80,name="80% CI",
          fillcolor="rgba(88,166,255,0.25)",line=list(color="transparent"))
    }
    p %>%
      add_lines(x=fd_dates, y=fc_mean, name="Forecast",
                line=list(color="#3fb950",width=2.2,dash="dash")) %>%
      layout(yaxis=list(title="Price (USD)",tickprefix="$"),
             xaxis=list(title=""), hovermode="x unified") %>% apply_dark()
  })

  output$fc_model_info <- renderPrint({
    req(rv$fc_out)
    cat("Model:", class(rv$fc_out$model)[1], "\n\n")
    tryCatch(print(rv$fc_out$model), error=function(e) cat(e$message))
  })

  output$decomp_plot <- renderPlotly({
    req(rv$prices, input$fc_ticker %in% names(rv$prices))
    px <- Ad(rv$prices[[input$fc_ticker]])
    tryCatch({
      dcmp  <- stl(ts(as.numeric(px), frequency=52), s.window="periodic")
      trend <- as.numeric(dcmp$time.series[,"trend"])
      plot_ly(x=index(px)) %>%
        add_lines(y=as.numeric(px), name="Price",   line=list(color="#8b949e",width=1)) %>%
        add_lines(y=trend,          name="Trend",   line=list(color="#3fb950",width=2.2)) %>%
        layout(yaxis=list(title="USD",tickprefix="$"), xaxis=list(title=""),
               hovermode="x unified") %>% apply_dark()
    }, error=function(e) plotly_empty() %>% apply_dark())
  })

  output$acf_plot <- renderPlotly({
    req(rv$returns, input$fc_ticker %in% colnames(rv$returns))
    r  <- as.numeric(rv$returns[, input$fc_ticker]); r <- r[!is.na(r)]
    a  <- acf(r,  lag.max=40, plot=FALSE)
    pa <- pacf(r, lag.max=40, plot=FALSE)
    ci <- 1.96 / sqrt(length(r))

    plot_ly() %>%
      add_bars(x=as.numeric(a$lag[-1]),  y=as.numeric(a$acf[-1]),  name="ACF",
               marker=list(color="rgba(88,166,255,0.75)")) %>%
      add_bars(x=as.numeric(pa$lag), y=as.numeric(pa$acf), name="PACF",
               marker=list(color="rgba(210,168,255,0.75)"), visible="legendonly") %>%
      layout(
        shapes = list(
          list(type="line",x0=0,x1=40,y0=ci, y1=ci, line=list(color="#f78166",dash="dot",width=1.5)),
          list(type="line",x0=0,x1=40,y0=-ci,y1=-ci,line=list(color="#f78166",dash="dot",width=1.5))
        ),
        barmode="group",
        xaxis=list(title="Lag"),
        yaxis=list(title="Correlation", range=c(-0.25,0.25))) %>% apply_dark()
  })
}
