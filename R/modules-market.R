# Simplified Volume module (separate from candles)
#' @keywords internal
.ix_to_ms <- function(ix) {
  if (inherits(ix, "Date")) as.numeric(as.POSIXct(ix, tz = "UTC")) * 1000 else as.numeric(ix) * 1000
}

#' @keywords internal
.thin_ohlc_for_chart <- function(std, max_points = getOption("tplot.max_candles", 6000L), label = "candles") {
  max_points <- max(500L, as.integer(max_points[1]))
  if (is.null(std) || !xts::is.xts(std) || NROW(std) <= max_points) {
    return(std)
  }
  needed <- c("Open", "High", "Low", "Close")
  if (!all(needed %in% colnames(std))) {
    return(std)
  }
  n <- NROW(std)
  step <- ceiling(n / max_points)
  grp <- ceiling(seq_len(n) / step)
  mat <- coredata(std)
  col_idx <- match(needed, colnames(std))
  vol_idx <- match("Volume", colnames(std))
  open <- as.numeric(mat[!duplicated(grp), col_idx[1]])
  high <- as.numeric(tapply(mat[, col_idx[2]], grp, max, na.rm = TRUE))
  low <- as.numeric(tapply(mat[, col_idx[3]], grp, min, na.rm = TRUE))
  close <- as.numeric(mat[!duplicated(grp, fromLast = TRUE), col_idx[4]])
  out <- data.frame(Open = open, High = high, Low = low, Close = close)
  if (!is.na(vol_idx)) {
    out$Volume <- as.numeric(tapply(mat[, vol_idx], grp, sum, na.rm = TRUE))
  }
  idx <- index(std)[!duplicated(grp, fromLast = TRUE)]
  ans <- xts::xts(out, order.by = idx)
  message(sprintf("[tplot] Reduced %s payload from %d to %d points.", label, n, NROW(ans)))
  ans
}

#' @keywords internal
.thin_xts_last_for_chart <- function(x, max_points = getOption("tplot.max_position_points", 6000L), label = "series") {
  max_points <- max(500L, as.integer(max_points[1]))
  if (is.null(x) || !xts::is.xts(x) || NROW(x) <= max_points) {
    return(x)
  }
  n <- NROW(x)
  step <- ceiling(n / max_points)
  grp <- ceiling(seq_len(n) / step)
  mat <- coredata(x)
  keep <- !duplicated(grp, fromLast = TRUE)
  ans <- xts::xts(mat[keep, , drop = FALSE], order.by = index(x)[keep])
  colnames(ans) <- colnames(x)
  message(sprintf("[tplot] Reduced %s payload from %d to %d points.", label, n, NROW(ans)))
  ans
}

#' @keywords internal
volume_module <- function(mktdata, theme) {
  if (is.null(mktdata) || !xts::is.xts(mktdata)) {
    return(NULL)
  }
  std <- try(.to_ohlc_standard(mktdata), silent = TRUE)
  if (inherits(std, "try-error") || is.null(std)) {
    return(NULL)
  }
  # Deduplicate repeated timestamps (keep last occurrence)
  std <- std[!duplicated(index(std), fromLast = TRUE), ]
  if (!("Volume" %in% colnames(std))) {
    return(NULL)
  }
  std <- .thin_ohlc_for_chart(std, getOption("tplot.max_volume_points", getOption("tplot.max_candles", 6000L)), "volume")

  pal <- theme$palette
  cl <- theme$colors
  hei <- .compact_chart_height(theme)
  idx_ms <- .ix_to_ms(index(std))
  vol_data <- highcharter::list_parse2(data.frame(x = idx_ms, y = as.numeric(std$Volume)))

  hc <- highcharter::highchart() %>%
    hc_size(height = hei) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg,
      renderTo = "volume-chart",
      zoomType = "x", panning = list(enabled = TRUE, type = "x"), panKey = "shift"
    ) %>%
    hc_xAxis(type = "datetime", labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))) %>%
    hc_yAxis(
      title = list(text = "Volume", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
      labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
    ) %>%
    hc_plotOptions(column = list(dataGrouping = list(enabled = FALSE))) %>%
    hc_add_series(data = vol_data, type = "column", color = pal[1], name = "Volume", showInLegend = FALSE) %>%
    hc_tooltip(xDateFormat = "%Y-%m-%d") %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS(
          "function(e) { var thisChart = this.chart; if (e.trigger !== 'syncExtremes') { Highcharts.each(Highcharts.charts, function(chart) { if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('candles') || chart.options.chart.renderTo.includes('position'))) { if (chart.xAxis[0].setExtremes) { chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'}); } } }); } }"
        )
      )
    )
  hc
}

# Override candles module with axis-safe, volume-free version
#' @keywords internal
candles_module <- function(mktdata, txns, theme, asset_name = NULL) {
  comeca <- Sys.time()
  has_txns <- !is.null(txns) && xts::is.xts(txns) && nrow(txns) > 0
  if (has_txns) {
    cat(" -> Trades detected.\n")
    buys <- txns[txns$Txn.Qty > 0, ]
    sells <- txns[txns$Txn.Qty < 0, ]
  }
  if (is.null(mktdata) || !xts::is.xts(mktdata)) {
    return(NULL)
  }
  if (.isDI(mktdata)) {
    cat(" -> Futures data detected (Brazilian DI).\n")
    std <- mktdata
  } else {
    std <- try(.to_ohlc_standard(mktdata), silent = TRUE)
  }
  if (inherits(std, "try-error") || is.null(std)) {
    return(NULL)
  }

  di_flag <- .isDI(std)
  maturity_date <- attr(std, "maturity")
  maturity_date <- as.character(maturity_date)
  has_maturity <- length(maturity_date) == 1L && !is.na(maturity_date) && nzchar(maturity_date)
  if (has_maturity) cat(" -> Futures maturity date is", maturity_date, ".\n")

  pal <- theme$palette
  corx <- pal[1]
  cory <- pal[3]
  cl <- theme$colors

  # Candle + range selector style with safe fallbacks
  candle_cfg <- theme$candles %||% list()
  cw <- candle_cfg$point_width %||% 4
  cgrp <- candle_cfg$grouping %||% FALSE
  cup <- candle_cfg$up_color %||% "#00c176"
  cdown <- candle_cfg$down_color %||% "#ff4d4d"
  cline <- candle_cfg$line_color %||% "#cccccc"
  clw <- candle_cfg$line_width %||% 1
  cheight <- candle_cfg$height %||% 500
  rs_txt <- cl$range_selector_txt %||% cl$axis_txt
  rs_fill <- cl$range_selector_bg %||% cl$chart_bg
  rs_stk <- cl$range_selector_border %||% cl$axis_txt

  if (has_txns && di_flag && has_maturity) {
    buys$Txn.Price <- mapply(get_DI_price, buys$Txn.Price, index(buys), MoreArgs = list(maturity = maturity_date))
    sells$Txn.Price <- mapply(get_DI_price, sells$Txn.Price, index(sells), MoreArgs = list(maturity = maturity_date))
  }
  if (has_txns) {
    if (di_flag) {
      tmp <- buys
      buys <- sells
      sells <- tmp
    }
    buys_data <- highcharter::list_parse2(data.frame(
      x = .ix_to_ms(index(buys)),
      y = as.numeric(buys$Txn.Price),
      z = as.numeric(buys$Txn.Qty)
    ))
    sells_data <- highcharter::list_parse2(data.frame(
      x = .ix_to_ms(index(sells)),
      y = as.numeric(sells$Txn.Price),
      z = as.numeric(sells$Txn.Qty)
    ))
  }

  std <- .thin_ohlc_for_chart(std, getOption("tplot.max_candles", 6000L), "candles")
  idx_ms <- .ix_to_ms(index(std))
  ohlc_data <- highcharter::list_parse2(data.frame(
    x = idx_ms,
    open = as.numeric(std$Open),
    high = as.numeric(std$High),
    low = as.numeric(std$Low),
    close = as.numeric(std$Close)
  ))

  abrev3 <- JS("function () { var raw=this.value+''; var hasPerc=raw.indexOf('%')!==-1; if(hasPerc) raw=raw.replace('%',''); var v=parseFloat(raw); if(isNaN(v)) return this.value; var neg=v<0?'-':''; v=Math.abs(v); var txt; if(v>=1e6){ txt=(v/1e6).toFixed(v>=1e7?0:1)+'M'; } else if(v>=1e3){ txt=(v/1e3).toFixed(v>=1e4?0:1)+'k'; } else if(v>=100){ txt=v.toFixed(0);} else { txt=v.toFixed(2);} return neg+txt+(hasPerc?'%':''); }")

  hc <- highcharter::highchart() %>%
    hc_size(height = cheight) %>%
    hc_chart(
      spacing = theme$hc_spacing,
      margin = c(theme$hc_margin[1], theme$hc_margin[2], theme$hc_margin[3], theme$hc_margin[4]),
      backgroundColor = cl$chart_bg,
      renderTo = "candles-chart",
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift",
      events = list(
        load = JS("function(){ var xa=this.xAxis && this.xAxis[0]; if(xa){ xa.setExtremes(xa.dataMin, xa.dataMax, true, false); } }")
      )
    ) %>%
    highcharter::hc_boost(enabled = TRUE, useGPUTranslations = TRUE, usePreAllocated = TRUE) %>%
    hc_add_yAxis(
      id = "price", startOnTick = FALSE, endOnTick = FALSE,
      title = list(text = "Price", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
      labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold")),
      relative = 1, opposite = FALSE
    )

  # Add candlestick series with pre-evaluated style to avoid NSE scoping issues
  ohlc_style <- list(upColor = cup, color = cdown, lineColor = cline, lineWidth = clw, pointWidth = cw)
  series_name <- if (!is.null(asset_name) && is.character(asset_name) && length(asset_name) == 1 && nzchar(asset_name)) asset_name else "Asset"
  hc <- do.call(
    highcharter::hc_add_series,
    c(
      list(hc = hc, data = ohlc_data, type = "candlestick", name = series_name, yAxis = "price"),
      ohlc_style
    )
  ) %>%
    hc_plotOptions(candlestick = list(dataGrouping = list(enabled = cgrp || NROW(std) > 1000L)), series = list(dataGrouping = list(enabled = FALSE), turboThreshold = 0)) %>%
    hc_xAxis(
      type = "datetime",
      ordinal = FALSE,
      minPadding = 0, maxPadding = 0,
      labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold")),
      events = list(afterSetExtremes = JS("function(e){ var thisChart=this.chart; if(e.trigger!=='syncExtremes'){ Highcharts.each(Highcharts.charts,function(chart){ if(chart && chart!==thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('cumret')||chart.options.chart.renderTo.includes('rolling')||chart.options.chart.renderTo.includes('period')||chart.options.chart.renderTo.includes('drawdown')||chart.options.chart.renderTo.includes('volume')||chart.options.chart.renderTo.includes('position'))){ if(chart.xAxis[0].setExtremes){ chart.xAxis[0].setExtremes(e.min,e.max,undefined,false,{trigger:'syncExtremes'}); } } }); }}"))
    ) %>%
    hc_rangeSelector(
      enabled = TRUE,
      allButtonsEnabled = TRUE,
      selected = 1,
      buttons = list(
        list(type = "ytd", text = "YTD"),
        list(type = "all", text = "All")
      ),
      buttonTheme = list(
        style = list(color = rs_txt),
        fill = rs_fill,
        stroke = rs_stk,
        states = list(
          hover  = list(fill = rs_fill, style = list(color = rs_txt)),
          select = list(fill = rs_fill, style = list(color = rs_txt))
        )
      ),
      inputStyle = list(color = rs_txt),
      labelStyle = list(color = rs_txt)
    ) %>%
    hc_navigator(outlineWidth = 1, series = list(color = pal[1], lineWidth = 2, type = "areaspline", fillColor = "white"), handles = list(backgroundColor = pal[4], borderColor = pal[3])) %>%
    hc_scrollbar(barBackgroundColor = "lightgray", barBorderRadius = 7, barBorderWidth = 0, buttonBackgroundColor = "lightgray", buttonBorderWidth = 0, buttonArrowColor = "yellow", buttonBorderRadius = 7, rifleColor = "yellow", trackBackgroundColor = "white", trackBorderWidth = 1, trackBorderColor = "silver", trackBorderRadius = 7) %>%
    hc_tooltip(xDateFormat = "%Y-%m-%d") %>%
    hc_legend(enabled = FALSE, verticalAlign = "bottom") %>%
    onRender("function(el,x){ Highcharts.setOptions({global:{useUTC:false}}); }")

  if (has_txns) {
    hc <- hc %>%
      hc_add_series(
        data = buys_data, yAxis = "price", type = "scatter", name = "Buys", color = corx,
        marker = list(enabled = TRUE, symbol = "triangle", radius = 5, fillColor = corx, lineColor = corx, lineWidth = 2),
        tooltip = list(headerFormat = "", pointFormat = "Buy<br>Date: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"), showInLegend = TRUE,
        boostThreshold = 1
      ) %>%
      hc_add_series(
        data = sells_data, yAxis = "price", type = "scatter", name = "Sells", color = cory,
        marker = list(enabled = TRUE, symbol = "triangle-down", radius = 5, fillColor = cory, lineColor = cory, lineWidth = 1),
        tooltip = list(headerFormat = "", pointFormat = "Sell<br>Date: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"), showInLegend = TRUE,
        boostThreshold = 1
      )
  }

  termina <- Sys.time()
  message(sprintf("Module 'candles' rendered in %.2f seconds.", as.numeric(difftime(termina, comeca, units = "secs"))))
  hc
}

#' Renders the Position Chart Module
#' @param mktdata An xts object with market data.
#' @param txns An xts object with transactions.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
position_module <- function(mktdata, txns, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()

  if (is.null(mktdata) || is.null(txns) || nrow(txns) == 0) {
    return(NULL)
  }

  # Drop known dummy initialization row (e.g., 1950-01-01 with zero qty/value)
  txns <- try(
    {
      tx <- txns
      if (NROW(tx) > 0) {
        # remove leading rows with zero quantity and very early dates (before 1970)
        idx <- index(tx)
        tz0 <- attr(idx, "tzone")
        if (is.null(tz0) || !nzchar(tz0)) tz0 <- "UTC"
        cutoff <- as.POSIXct("1970-01-01 00:00:00", tz = tz0)
        keep <- (idx >= cutoff) | (abs(suppressWarnings(as.numeric(tx$Txn.Qty))) > 0)
        if (any(!keep)) tx <- tx[keep]
      }
      tx
    },
    silent = TRUE
  )
  if (inherits(txns, "try-error") || is.null(txns) || NROW(txns) == 0) {
    return(NULL)
  }

  # Prefer Pos.Qty (blotter) if available; otherwise compute cumsum(Txn.Qty)
  pos_tx <- NULL
  if ("Pos.Qty" %in% colnames(txns)) {
    qtyp <- tryCatch(as.numeric(txns$Pos.Qty), error = function(e) NULL)
    if (!is.null(qtyp)) {
      pos_tx <- xts::xts(qtyp, order.by = index(txns))
    }
  }
  if (is.null(pos_tx)) {
    qty <- tryCatch(as.numeric(txns$Txn.Qty), error = function(e) NULL)
    if (is.null(qty)) {
      return(NULL)
    }
    tx_times <- index(txns)
    cumv <- cumsum(qty)
    df_tx <- data.frame(t = tx_times, cum = cumv)
    agg <- stats::aggregate(cum ~ t, data = df_tx, FUN = function(v) tail(v, 1))
    pos_tx <- xts::xts(agg$cum, order.by = agg$t)
  }
  # Union timeline to cover both candles and txn events
  combined_idx <- sort(unique(c(index(mktdata), index(pos_tx))))
  pos_union <- xts::xts(rep(NA_real_, length(combined_idx)), order.by = combined_idx)
  pos <- merge(pos_union, pos_tx, join = "outer")
  # carry forward and replace NA with 0 before first transaction
  pos <- zoo::na.locf(pos, na.rm = FALSE)
  pos[is.na(pos)] <- 0
  # Reduce to a single column named Pos.Qty
  pos <- pos[, ncol(pos), drop = FALSE]
  colnames(pos) <- "Pos.Qty"
  # Align display timezone with mktdata (for identical tooltips/labels)
  try(
    {
      attr(index(pos), "tzone") <- attr(index(mktdata), "tzone")
    },
    silent = TRUE
  )
  pos <- .thin_xts_last_for_chart(pos, getOption("tplot.max_position_points", 6000L), "position")

  # Prepare data for chart
  pos_data <- highcharter::list_parse2(
    data.frame(
      x = .ix_to_ms(index(pos)),
      y = as.numeric(coredata(pos))
    )
  )

  pal <- theme$palette
  zones <- list(
    list(value = 0, color = pal[3]),
    list(color = pal[1])
  )
  hei <- .compact_chart_height(theme)

  hc <- highcharter::highchart() %>%
    hc_size(height = hei) %>%
    highcharter::hc_boost(enabled = TRUE, useGPUTranslations = TRUE, usePreAllocated = TRUE) %>%
    hc_chart(
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = theme$colors$chart_bg,
      renderTo = if (sync_with_candles) "position-chart" else NULL,
      zoomType = "x",
      panning = list(enabled = TRUE, type = "x"),
      panKey = "shift"
    ) %>%
    hc_xAxis(type = "datetime", lineWidth = 0, tickLength = 0, labels = list(enabled = FALSE)) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = "Position",
        style = list(
          color      = theme$colors$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        style = list(
          color      = theme$colors$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      )
    ) %>%
    hc_plotOptions(series = list(dataGrouping = list(enabled = FALSE))) %>%
    hc_add_series(
      data = pos_data,
      type = "column",
      name = "Position",
      colorByPoint = FALSE,
      zones = zones,
      pointPadding = 0,
      groupPadding = 0,
      borderWidth = 0,
      showInLegend = FALSE,
      boostThreshold = 1
    ) %>%
    hc_tooltip(xDateFormat = "%Y-%m-%d") %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS("function(e){ var thisChart=this.chart; if(e.trigger!=='syncExtremes'){ Highcharts.each(Highcharts.charts,function(chart){ if(chart && chart!==thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('candles')||chart.options.chart.renderTo.includes('cumret')||chart.options.chart.renderTo.includes('rolling')||chart.options.chart.renderTo.includes('period')||chart.options.chart.renderTo.includes('drawdown')||chart.options.chart.renderTo.includes('volume')||chart.options.chart.renderTo.includes('position'))){ if(chart.xAxis[0].setExtremes){ chart.xAxis[0].setExtremes(e.min,e.max,undefined,false,{trigger:'syncExtremes'}); } } }); }}")
      )
    )
  termina <- Sys.time()
  message(sprintf(
    "Module 'position' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc
}
