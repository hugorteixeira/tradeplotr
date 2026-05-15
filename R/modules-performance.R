# Internal JS helpers for dynamic axis rescaling
.js_pad <- function(n) paste(rep(" ", max(0, as.integer(n))), collapse = "")

.js_rescale_body <- function(min_expr = "chart.xAxis[0].min", max_expr = "chart.xAxis[0].max", ensure_zero = FALSE, indent = 6) {
  pad <- .js_pad(indent)
  zero <- if (ensure_zero) paste0(pad, "if (isFinite(yMax)) { yMax = Math.max(yMax, 0); }\n") else ""
  paste0(
    pad, "var axis = chart.yAxis && chart.yAxis[0];\n",
    pad, "if (!axis) { return; }\n",
    pad, "var minX = ", min_expr, ",\n",
    pad, "    maxX = ", max_expr, ",\n",
    pad, "    yMin = Infinity,\n",
    pad, "    yMax = -Infinity,\n",
    pad, "    hasPoints = false;\n",
    pad, "Highcharts.each(chart.series, function(series) {\n",
    pad, "  if (!series.visible) { return; }\n",
    pad, "  Highcharts.each(series.points, function(point) {\n",
    pad, "    if (point.x >= minX && point.x <= maxX) {\n",
    pad, "      yMin = Math.min(yMin, point.y);\n",
    pad, "      yMax = Math.max(yMax, point.y);\n",
    pad, "      hasPoints = true;\n",
    pad, "    }\n",
    pad, "  });\n",
    pad, "});\n",
    pad, "if (!hasPoints) {\n",
    pad, "  var ext = axis.getExtremes();\n",
    pad, "  yMin = isFinite(ext.dataMin) ? ext.dataMin : 0;\n",
    pad, "  yMax = isFinite(ext.dataMax) ? ext.dataMax : 0;\n",
    pad, "}\n",
    zero,
    pad, "if (!isFinite(yMin) || !isFinite(yMax)) { return; }\n",
    pad, "if (yMin === yMax) {\n",
    pad, "  var padY = Math.abs(yMin) * 0.05 || 1;\n",
    pad, "  yMin -= padY;\n",
    pad, "  yMax += padY;\n",
    pad, "}\n",
    pad, "axis.setExtremes(yMin, yMax, undefined, false, {trigger: 'syncExtremes'});\n"
  )
}

.js_visibility_events <- function(ensure_zero = FALSE) {
  body <- .js_rescale_body(indent = 4, ensure_zero = ensure_zero)
  list(
    show = JS(paste0(
      "function(){\n",
      "  var chart = this.chart;\n",
      "  if (!chart) { return; }\n",
      "  setTimeout(function(){\n",
      body,
      "  }, 0);\n",
      "}\n"
    )),
    hide = JS(paste0(
      "function(){\n",
      "  var chart = this.chart;\n",
      "  if (!chart) { return; }\n",
      "  setTimeout(function(){\n",
      body,
      "  }, 0);\n",
      "}\n"
    ))
  )
}

.js_asset_legend_events <- function(events = list()) {
  events$legendItemClick <- JS(
    "function(){
       return false;
     }"
  )
  events
}

#' Renders the Cumulative Returns Chart Module
#' @param ret_cum An xts object with cumulative returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param link_charts Logical, whether to link legends across charts.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
cumret_module <- function(ret_cum, datas, ativo, benchs, theme, link_charts = FALSE, sync_with_candles = FALSE) {
  comeca <- Sys.time()
  pal <- theme$palette
  cl <- theme$colors
  hei <- .compact_chart_height(theme)
  legend_enabled <- isTRUE(getOption("tplot.show_asset_legends", FALSE))
  hc <- highchart() %>%
    hc_size(height = hei) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift"
    ) %>%
    hc_xAxis(
      type = "datetime",
      labels = list(
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      )
    ) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = "Cumulative Returns",
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format = "{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      ),
      min = min(ret_cum, na.rm = TRUE),
      max = max(ret_cum, na.rm = TRUE)
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals = 2,
      xDateFormat = "%Y-%m-%d"
    ) %>%
    hc_legend(
      enabled = legend_enabled,
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal",
      itemStyle = list(
        color      = cl$legend_txt,
        fontFamily = theme$font_family,
        fontSize   = paste0(theme$font_sizes$legend, "px"),
        fontWeight = "bold"
      )
    )

  if (sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "cumret-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x = datas, y = ret_cum[, ativo])),
    type = "line", name = ativo, color = pal[1], id = ativo
  )
  for (i in seq_along(benchs)) {
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x = datas, y = ret_cum[, benchs[i]])),
      type = "line", name = benchs[i], color = pal[i + 1], id = benchs[i]
    )
  }
  series_events <- .js_asset_legend_events(.js_visibility_events())
  hc <- hc %>% hc_plotOptions(series = list(events = series_events))
  termina <- Sys.time()
  message(sprintf(
    "Module 'cumret' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc <- hc %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS(paste0(
          "function(e){\n",
          "  var thisChart = this.chart;\n",
          "  if (e.trigger !== 'syncExtremes') {\n",
          "    Highcharts.each(Highcharts.charts, function(chart) {\n",
          "      if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('volume') || chart.options.chart.renderTo.includes('position'))) {\n",
          "        if (chart.xAxis[0].setExtremes) {\n",
          "          chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'});\n",
          "        }\n",
          "      }\n",
          "    });\n",
          "  }\n",
          "  var chart = this.chart;\n",
          .js_rescale_body(min_expr = "e.min", max_expr = "e.max", indent = 2),
          "}\n"
        ))
      )
    )
  hc
}

#' Renders the Rolling Returns Chart Module
#' @param ret_cum An xts object with cumulative returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
rollingret_module <- function(ret_cum, datas, ativo, benchs, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()
  if (inherits(ret_cum, c("xts", "zoo"))) {
    M <- coredata(ret_cum)
  } else if (is.data.frame(ret_cum)) {
    M <- as.matrix(ret_cum)
  } else if (is.matrix(ret_cum)) {
    M <- ret_cum
  } else {
    stop("ret_cum must be xts, zoo, data.frame or matrix")
  }
  colnames(M) <- colnames(ret_cum)
  N <- 1 + M / 100
  n <- nrow(N)
  p <- ncol(N)
  # Use a dynamic window: 10% of available candles (at least 2)
  k <- max(2L, as.integer(round(n * 0.10)))
  R <- matrix(NA_real_, n, p,
    dimnames = list(rownames(N), colnames(N))
  )
  if (n > k) {
    R[(k + 1):n, ] <- (N[(k + 1):n, , drop = FALSE] /
      N[1:(n - k), , drop = FALSE] - 1) * 100
  }
  # 3) paleta e cores
  hei <- .compact_chart_height(theme)
  pal <- theme$palette
  cl <- theme$colors
  hc <- highchart() %>%
    hc_size(height = hei) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift"
    ) %>%
    hc_xAxis(
      type = "datetime",
      lineWidth = 0,
      tickLength = 0,
      labels = list(enabled = FALSE)
    ) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = paste0("Rolling Returns ", k, "p"),
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format = "{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      ),
      min = min(R, na.rm = TRUE),
      max = max(R, na.rm = TRUE)
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals = 2,
      xDateFormat = "%Y-%m-%d"
    ) %>%
    hc_legend(enabled = FALSE)

  if (sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "rolling-chart"
    )
  }

  # ativo principal
  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x = datas, y = R[, ativo])),
    type = "line", name = ativo, color = pal[1], id = ativo
  )
  # benchmarks
  for (i in seq_along(benchs)) {
    nm <- benchs[i]
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x = datas, y = R[, nm])),
      type = "line", name = nm, color = pal[i + 1], id = nm
    )
  }
  series_events <- .js_asset_legend_events(.js_visibility_events())
  hc <- hc %>% hc_plotOptions(series = list(events = series_events))
  # 6) mensagem de tempo e retorno
  termina <- Sys.time()
  message(sprintf(
    "Module 'rollingret' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc <- hc %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS(paste0(
          "function(e){\n",
          "  var thisChart = this.chart;\n",
          "  if (e.trigger !== 'syncExtremes') {\n",
          "    Highcharts.each(Highcharts.charts, function(chart) {\n",
          "      if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('volume') || chart.options.chart.renderTo.includes('position'))) {\n",
          "        if (chart.xAxis[0].setExtremes) {\n",
          "          chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'});\n",
          "        }\n",
          "      }\n",
          "    });\n",
          "  }\n",
          "  var chart = this.chart;\n",
          .js_rescale_body(min_expr = "e.min", max_expr = "e.max", indent = 2),
          "}\n"
        ))
      )
    )
  hc
}

#' Renders the Rolling Correlation Chart Module
#' @param rolling_corr An xts object with rolling pairwise correlations.
#' @param datas A numeric vector of dates in milliseconds.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
rollingcorr_module <- function(rolling_corr, datas, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()
  if (is.null(rolling_corr) || !xts::is.xts(rolling_corr) || NCOL(rolling_corr) == 0) {
    return(NULL)
  }
  pal <- theme$palette
  cl <- theme$colors
  n_pairs <- NCOL(rolling_corr)
  height_steps <- floor((n_pairs - 1L) / 10L)
  height <- .compact_chart_height(theme) * 2 * (1 + 0.2 * height_steps)
  k <- attr(rolling_corr, "window", exact = TRUE)
  if (is.null(k) || !is.finite(k)) k <- NA_integer_
  title <- if (is.na(k)) "Rolling Correlation" else paste0("Rolling Correlation ", k, "p")
  series_ids <- paste0("corr-", seq_len(NCOL(rolling_corr)))
  series_colors <- pal[((seq_len(NCOL(rolling_corr)) - 1L) %% length(pal)) + 1L]
  hc <- highcharter::highchart() %>%
    highcharter::hc_size(height = height) %>%
    highcharter::hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift",
      events = list(load = highcharter::JS("function(){ if (window.tplotRescaleCorrChart) window.tplotRescaleCorrChart(this); }"))
    ) %>%
    highcharter::hc_xAxis(type = "datetime", lineWidth = 0, tickLength = 0, labels = list(enabled = FALSE)) %>%
    highcharter::hc_yAxis(
      floor = -1,
      ceiling = 1,
      startOnTick = FALSE,
      endOnTick = FALSE,
      minPadding = 0,
      maxPadding = 0,
      title = list(text = title, style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
      plotLines = list(list(value = 0, width = 1, color = cl$axis_txt, dashStyle = "ShortDash")),
      labels = list(format = "{value:.2f}", style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
    ) %>%
    highcharter::hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:.3f}<br>", valueDecimals = 3, xDateFormat = "%Y-%m-%d") %>%
    highcharter::hc_legend(enabled = FALSE)
  if (sync_with_candles) {
    hc <- hc %>% highcharter::hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "rolling-corr-chart"
    )
  }
  for (i in seq_len(NCOL(rolling_corr))) {
    nm <- colnames(rolling_corr)[i]
    pair_assets <- strsplit(nm, " x ", fixed = TRUE)[[1]]
    hc <- hc %>% highcharter::hc_add_series(
      data = highcharter::list_parse2(data.frame(x = datas, y = as.numeric(rolling_corr[, i]))),
      type = "line",
      name = nm,
      color = series_colors[i],
      id = series_ids[i],
      lineWidth = 2,
      zIndex = 1,
      custom = list(assets = pair_assets)
    )
  }
  hc <- hc %>%
    highcharter::hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = highcharter::JS(paste0(
          "function(e){\n",
          "  var thisChart = this.chart;\n",
          "  if (e.trigger !== 'syncExtremes') {\n",
          "    Highcharts.each(Highcharts.charts, function(chart) {\n",
          "      if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('volume') || chart.options.chart.renderTo.includes('position'))) {\n",
          "        if (chart.xAxis[0].setExtremes) {\n",
          "          chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger:'syncExtremes'});\n",
          "        }\n",
          "      }\n",
          "    });\n",
          "  }\n",
          "  if (window.tplotRescaleCorrChart) window.tplotRescaleCorrChart(thisChart);\n",
          "}\n"
        ))
      )
    )
  termina <- Sys.time()
  message(sprintf("Module 'rolling_corr' rendered in %.2f seconds.", as.numeric(difftime(termina, comeca, units = "secs"))))
  legend_buttons <- lapply(seq_len(NCOL(rolling_corr)), function(i) {
    sid <- series_ids[i]
    nm <- colnames(rolling_corr)[i]
    col <- series_colors[i]
    htmltools::tags$button(
      type = "button",
      `data-tplot-corr-series` = sid,
      `aria-pressed` = "false",
      onclick = paste0("window.tplotToggleCorrSeries(", jsonlite::toJSON(sid, auto_unbox = TRUE), ", this);"),
      onmouseenter = paste0("window.tplotHighlightCorrSeries(", jsonlite::toJSON(sid, auto_unbox = TRUE), ", true);"),
      onmouseleave = paste0("window.tplotHighlightCorrSeries(", jsonlite::toJSON(sid, auto_unbox = TRUE), ", false);"),
      style = paste0(
        "display:inline-flex;align-items:center;gap:6px;margin:0;padding:2px 0;",
        "border:0;background:transparent;color:", cl$legend_txt, ";cursor:pointer;",
        "font-family:", theme$font_family, ";font-size:", theme$font_sizes$legend, "px;",
        "font-weight:bold;line-height:1.2;"
      ),
      htmltools::tags$span(
        style = paste0(
          "display:inline-block;width:18px;height:0;border-top:3px solid ", col, ";",
          "border-radius:2px;flex:0 0 auto;"
        )
      ),
      htmltools::tags$span(htmltools::htmlEscape(nm))
    )
  })
  htmltools::tagList(
    hc,
    htmltools::tags$div(
      class = "tplot-corr-legend",
      style = paste0(
        "display:flex;flex-wrap:wrap;align-items:center;justify-content:center;gap:5px 14px;",
        "min-height:24px;margin:6px 0 0 0;padding:0 0 2px 0;"
      ),
      legend_buttons
    )
  )
}

#' Renders the Period Returns Chart Module
#' @param ret_simple An xts object with simple returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
periodret_module <- function(ret_simple, datas, ativo, benchs, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()
  pal <- theme$palette
  cl <- theme$colors
  hei <- .compact_chart_height(theme)
  hc <- highchart() %>%
    hc_size(height = hei) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift"
    ) %>%
    hc_xAxis(type = "datetime", tickLength = 0, lineWidth = 0, labels = list(enabled = FALSE)) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = "Periodic Returns",
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format = "{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      ),
      min = min(ret_simple, na.rm = TRUE),
      max = max(ret_simple, na.rm = TRUE)
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals = 2,
      xDateFormat = "%Y-%m-%d"
    ) %>%
    hc_legend(enabled = FALSE)

  if (sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "period-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x = datas, y = ret_simple[, ativo])),
    type = "line", name = ativo, color = pal[1], id = ativo
  )
  for (i in seq_along(benchs)) {
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x = datas, y = ret_simple[, benchs[i]])),
      type = "line", name = benchs[i], color = pal[i + 1], id = benchs[i]
    )
  }
  series_events <- .js_visibility_events()
  hc <- hc %>% hc_plotOptions(series = list(events = series_events))
  termina <- Sys.time()
  message(sprintf(
    "Module 'periodret' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc <- hc %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS(paste0(
          "function(e){\n",
          "  var thisChart = this.chart;\n",
          "  if (e.trigger !== 'syncExtremes') {\n",
          "    Highcharts.each(Highcharts.charts, function(chart) {\n",
          "      if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('volume') || chart.options.chart.renderTo.includes('position'))) {\n",
          "        if (chart.xAxis[0].setExtremes) {\n",
          "          chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'});\n",
          "        }\n",
          "      }\n",
          "    });\n",
          "  }\n",
          "  var chart = this.chart;\n",
          .js_rescale_body(min_expr = "e.min", max_expr = "e.max", indent = 2),
          "}\n"
        ))
      )
    )
  hc
}

#' Renders the Drawdowns Chart Module
#' @param drawdowns An xts object with the drawdown series.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
drawdown_module <- function(drawdowns, datas, ativo, benchs, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()
  pal <- theme$palette
  cl <- theme$colors
  hei <- .compact_chart_height(theme)

  hc <- highchart() %>%
    hc_size(height = hei) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift"
    ) %>%
    hc_xAxis(type = "datetime", lineWidth = 0, tickLength = 0, labels = list(enabled = FALSE)) %>%
    hc_yAxis(
      title = list(
        text = "Drawdowns",
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      # Keep Y-axis labels as they are
      labels = list(
        format = "{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      ),
      min = min(drawdowns, na.rm = TRUE),
      max = 0
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals = 2,
      xDateFormat = "%Y-%m-%d"
    ) %>%
    hc_legend(enabled = FALSE)

  if (sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "drawdown-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x = datas, y = drawdowns[, ativo])),
    type = "line", name = ativo, color = pal[1], id = ativo
  )

  for (i in seq_along(benchs)) {
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x = datas, y = drawdowns[, benchs[i]])),
      type = "line", name = benchs[i], color = pal[i + 1], id = benchs[i]
    )
  }

  series_events <- .js_visibility_events(ensure_zero = TRUE)
  hc <- hc %>% hc_plotOptions(series = list(events = series_events))

  termina <- Sys.time()
  message(sprintf(
    "Module 'drawdown' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))

  hc <- hc %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        # Keep X-axis synchronized and dynamically set Y-axis extremes to visible range
        afterSetExtremes = JS(paste0(
          "function(e){\n",
          "  var thisChart = this.chart;\n",
          "  if (e.trigger !== 'syncExtremes') {\n",
          "    Highcharts.each(Highcharts.charts, function(chart) {\n",
          "      if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('volume') || chart.options.chart.renderTo.includes('position'))) {\n",
          "        if (chart.xAxis[0].setExtremes) {\n",
          "          chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'});\n",
          "        }\n",
          "      }\n",
          "    });\n",
          "  }\n",
          "  var chart = this.chart;\n",
          .js_rescale_body(min_expr = "e.min", max_expr = "e.max", ensure_zero = TRUE, indent = 2),
          "}\n"
        ))
      )
    )

  hc
}
