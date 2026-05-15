#' Renders the tplot output in JSON format
#' @param prep The list of data prepared by .tplot_prepare.
#' @param modules The modules to include.
#' @return A JSON string.
#' @keywords internal
.tplot_render_json <- function(prep, modules) {
  out <- list()
  if ("stats" %in% modules) out$stats <- stats_json(prep$stats_df %||% prep$carteira_df)
  if ("costs" %in% modules) out$costs <- stats_json(prep$costs_df)
  if ("trade_quality" %in% modules) out$trade_quality <- stats_json(prep$trade_quality_df)
  if ("cumulative" %in% modules) out$cumulative <- series_json(prep$cum_returns %||% prep$ret_cum, prep$timestamps %||% prep$datas)
  if ("period" %in% modules) out$period <- series_json(prep$period_returns %||% prep$ret_sim, prep$timestamps %||% prep$datas)
  if ("drawdowns" %in% modules) out$drawdowns <- series_json(prep$drawdowns %||% prep$dds, prep$timestamps %||% prep$datas)
  if ("rolling_corr" %in% modules) out$rolling_corr <- series_json(prep$rolling_corr, prep$rolling_corr_timestamps)
  if ("table" %in% modules) out$table <- rentab_json(prep$returns_tables %||% prep$lista_tabs)
  jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

.tplot_asset_toggle_script <- function() {
  tags$script(HTML(
    "window.tplotHiddenAssets = window.tplotHiddenAssets || {};
     window.tplotSetAssetHidden = function(asset, hidden) {
       window.tplotHiddenAssets[asset] = hidden;
       if (window.Highcharts && Highcharts.charts) {
         Highcharts.charts.forEach(function(chart) {
           if (!chart) return;
           var matched = false;
           chart.series.forEach(function(series) {
             var opts = series.options || {};
             var custom = opts.custom || {};
             var assets = custom.assets || [];
             if (!Array.isArray(assets)) assets = [assets];
             var match = opts.id === asset || series.name === asset || assets.indexOf(asset) >= 0;
             if (match) {
               matched = true;
               if (hidden) series.hide(false);
               else series.show(false);
             }
           });
           if (matched) window.tplotRescaleCorrChart(chart);
           chart.redraw(false);
         });
       }
       document.querySelectorAll('[data-tplot-eye]').forEach(function(el) {
         if (el.getAttribute('data-tplot-eye') === asset) {
           el.style.opacity = hidden ? '0.38' : '1';
           el.setAttribute('aria-pressed', hidden ? 'true' : 'false');
           el.setAttribute('title', hidden ? 'Show asset in charts' : 'Hide asset in charts');
         }
       });
     };
     window.tplotToggleAsset = function(asset) {
       var hidden = !window.tplotHiddenAssets[asset];
       window.tplotSetAssetHidden(asset, hidden);
     };
     window.tplotRescaleCorrChart = function(chart) {
       if (!chart || !chart.xAxis || !chart.xAxis[0] || !chart.yAxis || !chart.yAxis[0]) return;
       var xExt = chart.xAxis[0].getExtremes ? chart.xAxis[0].getExtremes() : {};
       var minX = isFinite(xExt.min) ? xExt.min : xExt.dataMin;
       var maxX = isFinite(xExt.max) ? xExt.max : xExt.dataMax;
       var yMin = Infinity;
       var yMax = -Infinity;
       var hasPoints = false;
       var hasCorrSeries = false;
       chart.series.forEach(function(series) {
         var opts = series.options || {};
         if (!opts.id || String(opts.id).indexOf('corr-') !== 0) return;
         hasCorrSeries = true;
         if (!series.visible) return;
         series.points.forEach(function(point) {
           if (!point || !isFinite(point.y)) return;
           if (isFinite(minX) && point.x < minX) return;
           if (isFinite(maxX) && point.x > maxX) return;
           yMin = Math.min(yMin, point.y);
           yMax = Math.max(yMax, point.y);
           hasPoints = true;
         });
       });
       if (!hasCorrSeries) return;
       if (!hasPoints || !isFinite(yMin) || !isFinite(yMax)) {
         chart.yAxis[0].setExtremes(-1, 1, false, false, {trigger: 'corrRescale'});
         return;
       }
       var range = Math.abs(yMax - yMin);
       var pad = Math.max(0.05, range * 0.05);
       if (range === 0) pad = Math.max(0.05, Math.abs(yMax) * 0.05);
       yMin = Math.max(-1, yMin - pad);
       yMax = Math.min(1, yMax + pad);
       if (yMin === yMax) {
         yMin = Math.max(-1, yMin - 0.05);
         yMax = Math.min(1, yMax + 0.05);
       }
       chart.yAxis[0].setExtremes(yMin, yMax, false, false, {trigger: 'corrRescale'});
     };
     window.tplotToggleCorrSeries = function(seriesId, el) {
       var visible = true;
       if (window.Highcharts && Highcharts.charts) {
         Highcharts.charts.forEach(function(chart) {
           if (!chart) return;
           var matched = false;
           chart.series.forEach(function(series) {
             var opts = series.options || {};
             if (opts.id === seriesId) {
               matched = true;
               visible = !series.visible;
               if (visible) series.show(false);
               else series.hide(false);
             }
           });
           if (matched) window.tplotRescaleCorrChart(chart);
           chart.redraw(false);
         });
       }
       document.querySelectorAll('[data-tplot-corr-series]').forEach(function(node) {
         if (node.getAttribute('data-tplot-corr-series') === seriesId) {
           node.style.opacity = visible ? '1' : '0.42';
         node.setAttribute('aria-pressed', visible ? 'false' : 'true');
         }
       });
     };
     window.tplotHighlightCorrSeries = function(seriesId, highlighted) {
       if (window.Highcharts && Highcharts.charts) {
         Highcharts.charts.forEach(function(chart) {
           if (!chart) return;
           chart.series.forEach(function(series) {
             var opts = series.options || {};
             if (opts.id === seriesId && series.visible) {
               series.update({
                 lineWidth: highlighted ? 4 : 2,
                 zIndex: highlighted ? 10 : 1
               }, false);
             }
           });
           chart.redraw(false);
         });
       }
     };"
  ))
}

#' Renders the tplot output in HTML format
#' @param prep The list of data prepared by .tplot_prepare.
#' @param modules The modules to include.
#' @param theme The theme list object.
#' @param output_dir The output directory.
#' @param viewer Logical, whether to open in the RStudio viewer instead of saving a file.
#' @return The path to the saved HTML file.
#' @keywords internal
.tplot_render_html <- function(prep, modules, theme = dark_theme(), output_dir = "~",
                               viewer = FALSE) {
  head_tags <- tagList(
    tags$meta(charset = "UTF-8"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$title(sprintf(
      "tplot_%s_%s",
      (prep$asset %||% prep$ativo),
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )),
    .tplot_asset_toggle_script(),
    tags$style(HTML(sprintf(
      "html,body{margin:0;padding:0;background:%s;}
       #tplot-container{margin:0 auto;padding:10px;box-sizing:border-box;width:100%%;}
       .module{margin:0 0 %spx 0;padding:0;}
       .module:last-child{margin-bottom:0;}
       .module .html-widget{margin:0;}",
      theme$colors$page_bg,
      .module_gap(theme)
    )))
  )

  sync_with_candles <- "candles" %in% modules

  linha_datas <- tags$div(
    style = sprintf(
      "text-align:right;font-family:%s;font-size:12px;font-weight:bold;
       margin:30px 30px -10px 50px;color:%s;",
      theme$font_family, theme$colors$page_txt
    )
  )
  page <- tagList()
  link_flag <- all(c("cumulative", "rolling", "period", "drawdowns") %in% modules)

  for (mod in modules) {
    ui <- switch(mod,
      stats = tagList(
        stats_module(
          prep$stats_df %||% prep$carteira_df,
          (prep$asset %||% prep$ativo),
          (prep$benchmarks %||% prep$benchs),
          theme,
          prep$asset_info %||% NULL
        ),
        linha_datas
      ),
      costs = costs_module(
        prep$costs_df,
        theme
      ),
      trade_quality = trade_quality_module(
        prep$trade_quality_df,
        prep$trade_quality_points,
        theme
      ),
      candles = candles_module(
        prep$market_data %||% prep$mktdata,
        prep$trades,
        theme,
        (prep$asset %||% prep$ativo)
      ),
      volume = volume_module(
        prep$market_data %||% prep$mktdata,
        theme
      ),
      position = position_module(
        prep$market_data %||% prep$mktdata,
        prep$trades,
        theme,
        TRUE
      ),
      cumulative = cumret_module(
        prep$cum_returns %||% prep$ret_cum,
        prep$timestamps %||% prep$datas,
        (prep$asset %||% prep$ativo),
        (prep$benchmarks %||% prep$benchs),
        theme,
        link_flag,
        sync_with_candles
      ),
      rolling = rollingret_module(
        prep$cum_returns %||% prep$ret_cum,
        prep$timestamps %||% prep$datas,
        (prep$asset %||% prep$ativo),
        (prep$benchmarks %||% prep$benchs),
        theme,
        sync_with_candles
      ),
      rolling_corr = rollingcorr_module(
        prep$rolling_corr,
        prep$rolling_corr_timestamps,
        theme,
        sync_with_candles
      ),
      period = periodret_module(
        prep$period_returns %||% prep$ret_sim,
        prep$timestamps %||% prep$datas,
        (prep$asset %||% prep$ativo),
        (prep$benchmarks %||% prep$benchs),
        theme,
        sync_with_candles
      ),
      drawdowns = drawdown_module(
        prep$drawdowns %||% prep$dds,
        prep$timestamps %||% prep$datas,
        (prep$asset %||% prep$ativo),
        (prep$benchmarks %||% prep$benchs),
        theme,
        sync_with_candles
      ),
      table = rentab_table_module(
        prep$returns_tables %||% prep$lista_tabs,
        (prep$asset %||% prep$ativo),
        (prep$benchmarks %||% prep$benchs),
        theme
      ),
      footer = footer_module(theme)
    )
    page <- tagAppendChild(
      page,
      tags$div(class = "module", `data-module` = mod, ui)
    )
  }
  container <- tags$div(
    id = "tplot-container",
    style = sprintf(
      "background:%s;color:%s;",
      theme$colors$page_bg,
      theme$colors$page_txt
    ),
    page
  )
  doc <- tags$html(
    lang = "en",
    tags$head(head_tags),
    tags$body(container)
  )
  ## 4) se for viewer, apenas abre
  if (viewer) {
    html_print(doc)
    return(invisible(NULL))
  }
  deps <- findDependencies(doc)
  deps <- lapply(deps, fix_pkg)
  htmlDependencies(doc) <- deps
  doc <- fix_pkg(doc)

  ## 6) determina destino
  if (is.null(output_dir)) output_dir <- getwd()
  dir.create(output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  nome <- sprintf(
    "tplot_%s_%s.html",
    (prep$asset %||% prep$ativo),
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  destino <- file.path(output_dir, nome)
  ## 7) salva com htmltools::save_html()
  #    selfcontained = FALSE e libdir = "files"

  save_html(
    html = doc,
    file = destino,
    libdir = "https://balboa.wiseturtle.com.br/api/hplots/files",
    background = theme$colors$page_bg
  )

  html_content <- readLines(destino, encoding = "UTF-8")
  html_content <- gsub("https%3A/", "https://", html_content, fixed = TRUE)
  html_content <- gsub("%2F", "/", html_content, fixed = TRUE)
  writeLines(html_content, destino, useBytes = TRUE)

  message(
    "  HTML generated at: ", destino,
    "\n  Libs folder at: ",
    file.path(dirname(destino), "files/")
  )
  invisible(destino)
}

#' Renders the tplot output as a static image
#' @param prep The prepared data list.
#' @param modules The modules to include.
#' @param theme The theme list object.
#' @param format The image format ("png" or "jpg").
#' @param output_dir The output directory.
#' @return The path to the saved image file.
#' @keywords internal
.tplot_render_image <- function(prep, modules, theme = dark_theme(), format, output_dir = NULL) {
  if (missing(output_dir) || is.null(output_dir)) {
    output_dir <- getwd()
  }

  # Normalize output dir
  od <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(od)) dir.create(od, recursive = TRUE, showWarnings = FALSE)

  # Ensure we at least have cumulative, otherwise static layout doesn't make sense
  if (!"cumulative" %in% modules) {
    stop("Static image requires module 'cumulative'.")
  }

  # Font fallback if theme font not installed (prevents ugly fallback issues)
  font_family <- theme$font_family
  if (is.character(font_family) && nzchar(font_family)) {
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      mf <- tryCatch(systemfonts::match_font(font_family), error = function(e) NULL)
      if (is.null(mf) || is.na(mf$path) || !nzchar(mf$path)) font_family <- "sans"
    } else {
      font_family <- font_family # let ragg/system pick fallback if present
    }
  } else {
    font_family <- "sans"
  }

  # Build long frames for ggplot
  df_cum <- data.frame(
    date = as.Date(index(prep$cum_returns %||% prep$ret_cum)),
    as.data.frame(prep$cum_returns %||% prep$ret_cum, check.names = FALSE)
  )
  df_sim <- data.frame(
    date = as.Date(index(prep$period_returns %||% prep$ret_sim)),
    as.data.frame(prep$period_returns %||% prep$ret_sim, check.names = FALSE)
  )
  df_dd <- data.frame(
    date = as.Date(index(prep$drawdowns %||% prep$dds)),
    as.data.frame(prep$drawdowns %||% prep$dds, check.names = FALSE)
  )
  df_cum_l <- pivot_longer(df_cum, -date, names_to = "series", values_to = "value")
  df_sim_l <- pivot_longer(df_sim, -date, names_to = "series", values_to = "value")
  df_dd_l <- pivot_longer(df_dd, -date, names_to = "series", values_to = "value")

  # Helper for subtle grid lines on both light/dark themes
  .is_dark <- function(hex) {
    x <- tryCatch(grDevices::col2rgb(hex) / 255, error = function(e) matrix(c(0, 0, 0), ncol = 1))
    # luminance approx
    lum <- 0.2126 * x[1, 1] + 0.7152 * x[2, 1] + 0.0722 * x[3, 1]
    lum < 0.5
  }
  .mix_col <- function(col, with = if (.is_dark(theme$colors$chart_bg)) "#FFFFFF" else "#000000", alpha = 0.15) {
    a <- tryCatch(grDevices::col2rgb(col), error = function(e) matrix(c(200, 200, 200), ncol = 1))
    b <- tryCatch(grDevices::col2rgb(with), error = function(e) matrix(c(255, 255, 255), ncol = 1))
    m <- pmax(pmin(round((1 - alpha) * a + alpha * b), 255), 0)
    grDevices::rgb(m[1, 1], m[2, 1], m[3, 1], alpha = 0.8, maxColorValue = 255)
  }
  grid_col_major <- .mix_col(theme$colors$axis_txt, alpha = if (.is_dark(theme$colors$chart_bg)) 0.25 else 0.12)
  grid_col_minor <- .mix_col(theme$colors$axis_txt, alpha = if (.is_dark(theme$colors$chart_bg)) 0.12 else 0.06)

  base_theme <- ggplot2::theme_minimal(base_family = font_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = theme$colors$page_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = theme$colors$chart_bg, color = NA),
      plot.title = ggplot2::element_text(
        size = theme$font_sizes$title,
        color = theme$colors$title_txt, face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = theme$font_sizes$axis,
        color = theme$colors$axis_txt, face = "bold"
      ),
      axis.title = ggplot2::element_text(
        size = theme$font_sizes$axis,
        color = theme$colors$axis_txt, face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = theme$font_sizes$legend,
        color = theme$colors$legend_txt
      ),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = theme$colors$chart_bg, color = NA),
      legend.box.background = ggplot2::element_rect(fill = theme$colors$chart_bg, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col_major, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = grid_col_minor, linewidth = 0.2),
      plot.margin = ggplot2::margin(8, 16, 8, 16)
    )

  # Axis helpers and limits aligned with HTML modules
  y_cum_min <- suppressWarnings(min(df_cum_l$value, na.rm = TRUE))
  y_cum_max <- suppressWarnings(max(df_cum_l$value, na.rm = TRUE))
  y_dd_min <- suppressWarnings(min(df_dd_l$value, na.rm = TRUE))

  p1 <- ggplot2::ggplot(df_cum_l, ggplot2::aes(date, value, color = series)) +
    ggplot2::geom_line(size = 0.9) +
    labs(title = "Cumulative Returns", x = NULL, y = "Percentage") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "%"), limits = c(y_cum_min, y_cum_max), expand = c(0.01, 0)) +
    ggplot2::scale_x_date(expand = c(0.01, 0)) +
    ggplot2::scale_color_manual(values = theme$palette) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    base_theme

  p2 <- ggplot2::ggplot(df_sim_l, ggplot2::aes(date, value, color = series)) +
    ggplot2::geom_line(size = 0.8) +
    labs(title = "Periodic Returns", x = NULL, y = "Percentage") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "%"), expand = c(0.01, 0)) +
    ggplot2::scale_x_date(expand = c(0.01, 0)) +
    ggplot2::scale_color_manual(values = theme$palette) +
    base_theme +
    ggplot2::theme(legend.position = "none")

  p3 <- ggplot2::ggplot(df_dd_l, ggplot2::aes(date, value, color = series)) +
    ggplot2::geom_line(size = 0.8) +
    labs(title = "Drawdowns", x = NULL, y = "Percentage") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "%"), limits = c(y_dd_min, 0), expand = c(0.01, 0)) +
    ggplot2::scale_x_date(expand = c(0.01, 0)) +
    ggplot2::scale_color_manual(values = theme$palette) +
    base_theme +
    ggplot2::theme(legend.position = "none")

  # Tables styling closer to HTML
  # Stats table styling: per-row tinted background using series palette
  stats_rows <- nrow(prep$stats_df %||% prep$carteira_df)
  stats_cols <- ncol(prep$stats_df %||% prep$carteira_df)
  series_order <- c((prep$asset %||% prep$ativo), (prep$benchmarks %||% prep$benchs))
  row_palette <- vapply(seq_len(stats_rows), function(i) {
    nm <- as.character((prep$stats_df %||% prep$carteira_df)$Asset[i])
    pal_idx <- match(nm, series_order)
    base_col <- if (!is.na(pal_idx) && pal_idx <= length(theme$palette)) theme$palette[pal_idx] else theme$palette[1]
    grDevices::adjustcolor(base_col, alpha.f = 0.20)
  }, character(1))
  # build a fill matrix repeating row colors for all body columns
  stats_fill_mat <- matrix(rep(row_palette, stats_cols), nrow = stats_rows, ncol = stats_cols, byrow = FALSE)
  tt_stats <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(
        fontfamily = font_family,
        fontsize = theme$font_sizes$table,
        col = theme$colors$table_row_txt
      ),
      bg_params = list(
        fill = stats_fill_mat,
        col = grDevices::adjustcolor("#000000", alpha.f = 0.10)
      )
    ),
    colhead = list(
      fg_params = list(
        fontfamily = font_family,
        fontsize = theme$font_sizes$table,
        col = theme$colors$table_header_txt,
        fontface = "bold"
      ),
      bg_params = list(
        fill = theme$colors$table_header_bg,
        col = grDevices::adjustcolor("#000000", alpha.f = 0.10)
      )
    )
  )

  stats_tbl <- tableGrob(prep$stats_df %||% prep$carteira_df, rows = NULL, theme = tt_stats)
  # Make table stretch to full width
  stats_tbl$widths <- grid::unit(rep(1, ncol(prep$stats_df %||% prep$carteira_df)), "null")

  date_lbl <- textGrob(
    paste0(format((prep$start_date %||% prep$init_date), "%d-%m-%Y"), " to ", format((prep$end_date %||% prep$finit_date), "%d-%m-%Y")),
    x = 1, hjust = 1,
    gp = gpar(fontfamily = font_family, fontsize = theme$font_sizes$table, col = theme$colors$page_txt)
  )

  month_names <- names(prep$returns_tables %||% prep$lista_tabs)[vapply(prep$returns_tables %||% prep$lista_tabs, function(t) NROW(t) > 0, logical(1))]
  month_grobs <- lapply(month_names, function(nm) {
    tab <- (prep$returns_tables %||% prep$lista_tabs)[[nm]]
    df2 <- cbind(Year = rownames(tab), as.data.frame(tab, check.names = FALSE))
    pal <- theme$palette[match(nm, c((prep$asset %||% prep$ativo), (prep$benchmarks %||% prep$benchs)))]
    fill <- grDevices::adjustcolor(ifelse(is.na(pal), theme$palette[1], pal), alpha.f = 0.20)
    # Build fill matrix for all rows/cols of this table
    if (NROW(df2) > 0) {
      fill_mat <- matrix(fill, nrow = NROW(df2), ncol = NCOL(df2))
    } else {
      fill_mat <- NULL
    }
    tt <- gridExtra::ttheme_minimal(
      core = list(
        fg_params = list(
          fontfamily = font_family,
          fontsize = theme$font_sizes$table,
          col = theme$colors$table_row_txt
        ),
        bg_params = list(
          fill = fill_mat,
          col = grDevices::adjustcolor("#000000", alpha.f = 0.10)
        )
      ),
      colhead = list(
        fg_params = list(
          fontfamily = font_family,
          fontsize = theme$font_sizes$table,
          col = theme$colors$table_header_txt,
          fontface = "bold"
        ),
        bg_params = list(
          fill = theme$colors$table_header_bg,
          col = grDevices::adjustcolor("#000000", alpha.f = 0.10)
        )
      )
    )
    tg <- tableGrob(df2, rows = NULL, theme = tt)
    tg$widths <- grid::unit(rep(1, ncol(df2)), "null")
    tg
  })

  footer_lbl <- textGrob(theme$footer_text,
    gp = gpar(
      fontfamily = font_family,
      fontsize = theme$font_sizes$table,
      col = theme$colors$footer_txt,
      fontface = "bold"
    ),
    x = 0.5, hjust = 0.5
  )

  # Helper: wrap grob with left/right margins to match chart margins
  wrap_with_margins <- function(g, left_pt = 16, right_pt = 16) {
    gridExtra::arrangeGrob(
      grobs = list(grid::nullGrob(), g, grid::nullGrob()),
      ncol = 3,
      widths = grid::unit.c(grid::unit(left_pt, "pt"), grid::unit(1, "null"), grid::unit(right_pt, "pt"))
    )
  }

  stats_tbl_wrapped <- wrap_with_margins(stats_tbl)
  date_lbl_wrapped <- wrap_with_margins(date_lbl)
  footer_wrapped <- wrap_with_margins(footer_lbl)
  month_wrapped <- lapply(month_grobs, wrap_with_margins)

  # Estimate pixel heights for each section
  row_h_px <- max(24, round(theme$font_sizes$table * 2.2))
  stats_body_px <- 20 + (stats_rows + 1) * row_h_px
  date_px <- 24
  cum_px <- 420
  per_px <- 300
  dd_px <- 260
  footer_px <- 32
  months_px <- vapply(month_names, function(nm) {
    tab <- (prep$returns_tables %||% prep$lista_tabs)[[nm]]
    20 + (NROW(tab) + 1) * row_h_px
  }, numeric(1))

  # Page composition with optional splitting for very tall outputs
  width_px <- 1600
  res_dpi <- 200
  max_page_px <- getOption("tplot.image_max_height", 8000)

  # Page 1 base sections
  base_grobs <- list(stats_tbl_wrapped, date_lbl_wrapped, p1, p2, p3)
  base_heights <- c(stats_body_px, date_px, cum_px, per_px, dd_px)

  # Fit as many monthly tables as possible on page 1
  available_px <- max_page_px - sum(base_heights) - footer_px
  take <- 0L
  if (length(months_px)) {
    cs <- cumsum(months_px)
    take <- max(which(cs <= available_px), na.rm = TRUE)
    if (!is.finite(take)) take <- 0L
  }
  page_grobs <- c(base_grobs, if (take > 0) month_wrapped[seq_len(take)] else list(), list(footer_wrapped))
  page_heights <- c(base_heights, if (take > 0) months_px[seq_len(take)] else numeric(0), footer_px)

  # Function to write one page
  write_page <- function(page_grobs, page_heights, suffix) {
    heights_units <- page_heights / 40 # convert px to relative weights
    composed <- gridExtra::arrangeGrob(grobs = page_grobs, ncol = 1, heights = heights_units)
    fname <- sprintf("tplot_%s_%s%s.%s", (prep$asset %||% prep$ativo), format(Sys.time(), "%Y%m%d_%H%M%S"), suffix, format)
    fpath <- file.path(od, fname)
    height_px <- max(1200, round(sum(page_heights) * 1.02))
    if (identical(format, "png")) {
      if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_png(fpath,
          width = width_px, height = height_px, units = "px", res = res_dpi,
          background = theme$colors$page_bg
        )
      } else {
        grDevices::png(fpath, width = width_px, height = height_px, res = res_dpi, bg = theme$colors$page_bg)
      }
    } else {
      if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_jpeg(fpath,
          width = width_px, height = height_px, units = "px", res = res_dpi,
          quality = 0.95, background = theme$colors$page_bg
        )
      } else {
        grDevices::jpeg(fpath, width = width_px, height = height_px, quality = 95, res = res_dpi, bg = theme$colors$page_bg)
      }
    }
    grid::grid.draw(composed)
    grDevices::dev.off()

    # RStudio plotting
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      ok <- tryCatch(isTRUE(rstudioapi::isAvailable()), error = function(e) FALSE)
      if (ok) {
        grid::grid.newpage()
        grid::grid.draw(composed)
      }
    }

    message("  Saved chart at: ", fpath)
    fpath
  }

  paths <- character(0)
  paths <- c(paths, write_page(page_grobs, page_heights, suffix = ""))

  # Remaining monthly tables go to subsequent pages
  if (take < length(month_wrapped)) {
    idx <- take + 1
    while (idx <= length(month_wrapped)) {
      cur_grobs <- list(date_lbl_wrapped)
      cur_heights <- c(date_px)
      while (idx <= length(month_wrapped)) {
        next_h <- months_px[idx]
        if (sum(cur_heights) + next_h + footer_px > max_page_px) {
          # if nothing fits yet, force at least one table on this page
          if (length(cur_heights) == 1) {
            cur_grobs <- c(cur_grobs, month_wrapped[idx])
            cur_heights <- c(cur_heights, next_h)
            idx <- idx + 1
          }
          break
        }
        cur_grobs <- c(cur_grobs, month_wrapped[idx])
        cur_heights <- c(cur_heights, next_h)
        idx <- idx + 1
      }
      # add footer to this page
      cur_grobs <- c(cur_grobs, list(footer_wrapped))
      cur_heights <- c(cur_heights, footer_px)
      paths <- c(paths, write_page(cur_grobs, cur_heights, suffix = sprintf("_p%d", length(paths) + 1)))
    }
  }

  invisible(paths)
}
