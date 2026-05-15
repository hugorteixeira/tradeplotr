## Backtest-specific modules

#' @keywords internal
.fmt_num <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) {
    return("-")
  }
  ax <- abs(x)
  if (ax >= 1e9) {
    return(paste0(format(round(x / 1e9, 2), trim = TRUE), "B"))
  }
  if (ax >= 1e6) {
    return(paste0(format(round(x / 1e6, 2), trim = TRUE), "M"))
  }
  if (ax >= 1e3) {
    return(paste0(format(round(x / 1e3, 1), trim = TRUE), "k"))
  }
  format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' @keywords internal
.fmt_pct <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) {
    return("-")
  }
  paste0(format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE), "%")
}

#' @keywords internal
.html_simple_table <- function(df, theme, asset_order = NULL, percent_cols = character(0), digits = 2) {
  if (is.null(df) || !is.data.frame(df) || !NROW(df)) {
    return("")
  }
  cl <- theme$colors
  pal <- theme$palette
  if (is.null(asset_order)) {
    asset_order <- if ("Asset" %in% colnames(df)) unique(as.character(df$Asset)) else character(0)
  }
  display_names <- c(
    Trades = "Trd",
    Contracts = "Ctr",
    GrossPnL = "Gross",
    NetPnL = "Net",
    Slippage = "Slip",
    TotalCost = "Cost",
    CostImpactPct = "Impact%",
    WinRate = "Win%",
    ProfitFactor = "PF",
    MedianR = "MedR",
    WorstR = "Worst",
    MedianBars = "Bars",
    AvgNetPnL = "NetAvg",
    MedianMFE_R = "MFE",
    MedianMAE_R = "MAE"
  )
  col_label <- function(nm) {
    if (nm %in% names(display_names)) display_names[[nm]] else nm
  }
  rgba <- function(col, alpha = 0.18) {
    rgb_col <- tryCatch(grDevices::col2rgb(col), error = function(e) matrix(c(200, 200, 200), ncol = 1))
    paste0("rgba(", paste(rgb_col[, 1], collapse = ","), ",", alpha, ")")
  }
  table_font <- max(8, theme$font_sizes$table - 1)
  header <- paste(vapply(colnames(df), function(nm) {
    sprintf(
      "<th style='background:%s;color:%s;padding:7px 8px;border:1px solid rgba(0,0,0,0.1);font-family:%s;font-size:%spx;font-weight:bold;white-space:normal;line-height:1.1;'>%s</th>",
      cl$table_header_bg, cl$table_header_txt, theme$font_family, table_font,
      htmltools::htmlEscape(col_label(nm))
    )
  }, character(1)), collapse = "")
  rows <- vapply(seq_len(NROW(df)), function(i) {
    asset <- if ("Asset" %in% colnames(df)) as.character(df$Asset[i]) else ""
    pal_idx <- match(asset, asset_order)
    pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
    row_bg <- rgba(pal_col)
    cells <- vapply(colnames(df), function(nm) {
      val <- df[[nm]][i]
      txt <- if (is.numeric(val)) {
        if (nm %in% percent_cols) .fmt_pct(val, digits) else .fmt_num(val, digits)
      } else {
        htmltools::htmlEscape(as.character(val))
      }
      align <- if (nm == "Asset") "left" else "right"
      sprintf(
        "<td style='padding:7px 8px;border:1px solid rgba(0,0,0,0.1);font-family:%s;font-size:%spx;color:%s;text-align:%s;white-space:normal;word-break:break-word;line-height:1.15;'>%s</td>",
        theme$font_family, table_font, cl$table_row_txt, align, txt
      )
    }, character(1))
    paste0("<tr style='background-color:", row_bg, ";'>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0(
    "<div style='width:100%;max-width:100%;overflow:hidden;margin-top:8px;'>",
    "<table style='width:100%;border-collapse:collapse;background:", cl$page_bg, ";table-layout:fixed;'>",
    "<tr>", header, "</tr>", paste(rows, collapse = ""), "</table></div>"
  )
}

#' @keywords internal
.pal_pick <- function(pal, i, fallback = "#777777") {
  if (length(pal) >= i && !is.na(pal[i]) && nzchar(pal[i])) pal[i] else fallback
}

#' Renders the Costs/Friction Module
#' @param costs_df A normalized costs data.frame.
#' @param theme The theme list object.
#' @return An HTML/highcharter module.
#' @keywords internal
costs_module <- function(costs_df, theme) {
  comeca <- Sys.time()
  if (is.null(costs_df) || !is.data.frame(costs_df) || NROW(costs_df) == 0) {
    return(NULL)
  }
  cl <- theme$colors
  pal <- theme$palette
  height <- .compact_chart_height(theme)
  assets <- as.character(costs_df$Asset)
  legend_enabled <- isTRUE(getOption("tplot.show_asset_legends", FALSE))
  hc <- highcharter::highchart() %>%
    highcharter::hc_size(height = height) %>%
    highcharter::hc_chart(
      type = "column",
      spacing = .compact_chart_spacing(theme),
      margin = .compact_chart_margin(theme),
      backgroundColor = cl$chart_bg
    ) %>%
    highcharter::hc_xAxis(
      categories = c("Fees", "Slip"),
      labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = "Cost", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
      labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
    ) %>%
    highcharter::hc_plotOptions(column = list(grouping = TRUE), series = list(events = .js_asset_legend_events())) %>%
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>%
    highcharter::hc_legend(enabled = legend_enabled, itemStyle = list(color = cl$legend_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$legend, "px"), fontWeight = "bold"))
  for (i in seq_along(assets)) {
    fees <- if ("Fees" %in% colnames(costs_df)) as.numeric(costs_df$Fees[i]) else NA_real_
    slip <- if ("Slippage" %in% colnames(costs_df)) as.numeric(costs_df$Slippage[i]) else NA_real_
    hc <- hc %>% highcharter::hc_add_series(
      name = assets[i],
      id = assets[i],
      custom = list(assets = assets[i]),
      data = c(fees, slip),
      color = .pal_pick(pal, i, .pal_pick(pal, 1))
    )
  }
  table_cols <- intersect(c("Asset", "Trades", "Contracts", "GrossPnL", "NetPnL", "Fees", "Slippage", "TotalCost", "CostImpactPct"), colnames(costs_df))
  ui <- htmltools::tagList(
    htmltools::HTML(.html_simple_table(costs_df[, table_cols, drop = FALSE], theme, asset_order = assets, percent_cols = "CostImpactPct")),
    htmltools::tags$div(style = "margin-top:8px;", hc)
  )
  termina <- Sys.time()
  message(sprintf("Module 'costs' rendered in %.2f seconds.", as.numeric(difftime(termina, comeca, units = "secs"))))
  ui
}

#' Renders the Trade Quality Module
#' @param quality_df A normalized trade-quality summary table.
#' @param points_df A normalized trade-quality points table.
#' @param theme The theme list object.
#' @return An HTML/highcharter module.
#' @keywords internal
trade_quality_module <- function(quality_df, points_df, theme) {
  comeca <- Sys.time()
  if (is.null(quality_df) || !is.data.frame(quality_df) || NROW(quality_df) == 0) {
    return(NULL)
  }
  cl <- theme$colors
  pal <- theme$palette
  height <- .compact_chart_height(theme)
  max_points <- getOption("tplot.trade_quality.max_points", 5000L)
  max_points <- max(100L, as.integer(max_points[1]))
  hc <- NULL
  if (!is.null(points_df) && is.data.frame(points_df) && NROW(points_df) > 0) {
    if (NROW(points_df) > max_points) {
      idx <- unique(as.integer(round(seq(1, NROW(points_df), length.out = max_points))))
      points_df <- points_df[idx, , drop = FALSE]
    }
    legend_enabled <- isTRUE(getOption("tplot.show_asset_legends", FALSE))
    hc <- highcharter::highchart() %>%
      highcharter::hc_size(height = height) %>%
      highcharter::hc_chart(
        type = "scatter",
        spacing = .compact_chart_spacing(theme),
        margin = .compact_chart_margin(theme),
        backgroundColor = cl$chart_bg,
        zoomType = "xy"
      ) %>%
      highcharter::hc_xAxis(
        title = list(text = "MFE (R)", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
        labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "Final R", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
        plotLines = list(list(value = 0, width = 1, color = cl$axis_txt, dashStyle = "ShortDash")),
        labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))
      ) %>%
      highcharter::hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{series.name}</b><br>Trade: {point.trade}<br>Side: {point.side}<br>MFE: {point.x:.2f}R<br>Final: {point.y:.2f}R<br>Bars: {point.bars}<br>Net P/L: {point.net:.2f}"
      ) %>%
      highcharter::hc_plotOptions(series = list(events = .js_asset_legend_events())) %>%
      highcharter::hc_legend(enabled = legend_enabled, itemStyle = list(color = cl$legend_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$legend, "px"), fontWeight = "bold"))
    assets <- unique(as.character(points_df$Asset))
    for (i in seq_along(assets)) {
      df <- points_df[points_df$Asset == assets[i], , drop = FALSE]
      data <- lapply(seq_len(NROW(df)), function(j) {
        list(
          x = as.numeric(df$MFE_R[j]),
          y = as.numeric(df$FinalR[j]),
          trade = as.character(df$TradeID[j]),
          side = as.character(df$Side[j]),
          bars = as.numeric(df$Bars[j]),
          net = as.numeric(df$NetPnL[j])
        )
      })
      hc <- hc %>% highcharter::hc_add_series(
        data = data,
        name = assets[i],
        id = assets[i],
        custom = list(assets = assets[i]),
        type = "scatter",
        color = pal[((i - 1L) %% length(pal)) + 1L],
        marker = list(radius = 3, symbol = "circle")
      )
    }
  }
  table_cols <- intersect(c("Asset", "Trades", "WinRate", "ProfitFactor", "AvgR", "MedianR", "BestR", "WorstR", "MedianBars", "AvgNetPnL", "MedianMFE_R", "MedianMAE_R"), colnames(quality_df))
  ui <- htmltools::tagList(
    htmltools::HTML(.html_simple_table(quality_df[, table_cols, drop = FALSE], theme, asset_order = as.character(quality_df$Asset), percent_cols = "WinRate")),
    htmltools::tags$div(style = "margin-top:8px;", hc)
  )
  termina <- Sys.time()
  message(sprintf("Module 'trade_quality' rendered in %.2f seconds.", as.numeric(difftime(termina, comeca, units = "secs"))))
  ui
}
