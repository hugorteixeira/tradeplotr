#' @title Generate a Performance Report for Financial Assets
#' @description
#' Creates a complete and customizable performance report for one or more financial assets,
#' comparing them against benchmarks. The output can be an interactive viewer, an HTML
#' file, a JSON string, or a static image (PNG/JPG).
#'
#' @param ... Ticker symbols (e.g., "PETR4.SA") or xts/data.frame objects to be analyzed.
#' The first item is treated as the main asset, and the rest as benchmarks.
#' @param init Start date for the analysis in "YYYY-MM-DD" format.
#' @param finit End date for the analysis in "YYYY-MM-DD" format.
#' @param rf_rate The risk-free rate for calculating metrics like the Sharpe Ratio.
#' @param auto_rets Logical. If `TRUE`, calculates geometric returns. If `FALSE`, arithmetic.
#' @param format The output format: "viewer", "html", "json", "png", or "jpg".
#' @param output_dir The directory where output files (HTML, PNG, etc.) will be saved.
#' @param modules A character vector of modules to include in the report.
#' @param theme The theme function to use (e.g., `default_theme()` or `dark_theme()`).
#'
#' @return Invisibly returns the path to the generated file (for "html", "png", "jpg"),
#' the JSON string (for "json"), or launches the interactive viewer (for "viewer").
#'
#' @export
tplot <- function(...,
                  init       = "1994-08-01",
                  finit      = Sys.Date(),
                  rf_rate    = NULL,
                  auto_rets  = FALSE,
                  format     = c("viewer","html","json","png","jpg"),
                  output_dir = "tplots",
                  modules    = c("stats","candles","volume","position","cumulative","rolling",
                                 "period","drawdowns","table","footer"),
                  theme      = default_theme(),
                  verbose    = getOption("tplot.verbose", FALSE)) {
  inicio <- Sys.time()
  format  <- match.arg(format)
  user_specified_modules <- !missing(modules)
  modules <- match.arg(modules, several.ok = TRUE,
                       choices = c("stats","candles","volume","position","cumulative","rolling",
                                   "period","drawdowns","table","footer"))

  # Parse variable tickers: first is main (ativo), others are secondary
  mc <- match.call(expand.dots = FALSE)
  dot_exprs <- mc$...
  if (is.null(dot_exprs) || length(dot_exprs) == 0)
    stop("Provide at least on ticker as first argument (string or xts/data.frame object).")

  # helper to generate unique labels for xts entries
  seen <- new.env(parent = emptyenv())
  next_label <- function(lbl) {
    if (is.null(lbl) || lbl == "") lbl <- "Serie"
    if (is.null(seen[[lbl]])) {
      seen[[lbl]] <- 1L
      return(lbl)
    } else {
      seen[[lbl]] <- seen[[lbl]] + 1L
      return(paste0(lbl, " (", seen[[lbl]], ")"))
    }
  }

  items <- list() # list of list(label, type='xts'|'char', value)
  add_xts_item <- function(obj, label) {
    conv <- .to_xts(obj)
    if (.is_xts(conv)) {
      label_u <- next_label(label)
      items[[length(items) + 1L]] <<- list(label = label_u, type = "xts", value = conv)
    }
  }
  add_char_item <- function(sym) {
    if (is.character(sym) && length(sym) == 1L && nzchar(sym)) {
      items[[length(items) + 1L]] <<- list(label = sym, type = "char", value = sym)
    }
  }
  # recursively parse expressions without fully evaluating c()/list()
  parse_one <- function(ex) {
    if (is.symbol(ex)) {
      nm <- as.character(ex)
      obj <- .get_object_if_exists(nm)
      if (!is.null(obj)) {
        if (.is_xts(obj) || is.matrix(obj) || is.data.frame(obj) || inherits(obj, "zoo")) {
          add_xts_item(obj, nm)
        } else if (is.character(obj) && length(obj) >= 1L) {
          for (s in obj) add_char_item(s)
        } else {
          add_char_item(nm)
        }
      } else {
        add_char_item(nm)
      }
      return(invisible(NULL))
    }
    if (is.character(ex)) {
      for (s in ex) add_char_item(s)
      return(invisible(NULL))
    }
    if (is.call(ex)) {
      hd <- tryCatch(as.character(ex[[1]]), error = function(e) "")
      if (hd %in% c("c", "list")) {
        args <- as.list(ex)[-1]
        for (a in args) parse_one(a)
        return(invisible(NULL))
      }
      # evaluate other calls; derive a readable label
      obj <- tryCatch(eval(ex, envir = parent.frame()), error = function(e) NULL)
      lbl <- paste0(substr(paste(deparse(ex), collapse = ""), 1, 20))
      if (.is_xts(obj) || is.matrix(obj) || is.data.frame(obj) || inherits(obj, "zoo")) {
        add_xts_item(obj, lbl)
      } else if (is.character(obj) && length(obj) >= 1L) {
        for (s in obj) add_char_item(s)
      }
      return(invisible(NULL))
    }
    val <- tryCatch(eval(ex, envir = parent.frame()), error = function(e) NULL)
    if (.is_xts(val) || is.matrix(val) || is.data.frame(val) || inherits(val, "zoo")) {
      add_xts_item(val, "Serie")
    } else if (is.character(val) && length(val) >= 1L) {
      for (s in val) add_char_item(s)
    }
    invisible(NULL)
  }
  for (ex in as.list(dot_exprs)) parse_one(ex)
  if (length(items) == 0) stop("No valid ticker identified in ...")

  # split into ativo (first) and benchs (rest)
  ativo_item  <- items[[1]]
  bench_items <- if (length(items) > 1) items[-1] else list()

  ativo_spec  <- if (ativo_item$type == "xts") ativo_item$value else ativo_item$value
  ativo_label <- if (ativo_item$type == "xts") ativo_item$label else ativo_item$value

  bench_list  <- list()
  bench_chars <- character(0)
  for (bi in bench_items) {
    if (bi$type == "xts") {
      bench_list[[bi$label]] <- bi$value
    } else {
      bench_chars <- c(bench_chars, bi$value)
    }
  }
  benchs_spec <- if (length(bench_list) == 0 && length(bench_chars) == 0) NULL else c(bench_list, as.list(bench_chars))

  # palette sizing by number of series
  n_assets <- 1L + length(bench_items)
  if (length(theme$palette) < n_assets)
    theme$palette <- colorRampPalette(theme$palette)(n_assets)

  prep <- .tplot_prepare(ativo_spec, benchs_spec, init, finit, rf_rate, auto_rets,
                         ativo_name = ativo_label,
                         verbose = verbose)
  preparo <- Sys.time()
  mods_avail <- .available_modules(prep)
  if (!user_specified_modules) {
    priority <- c("stats","candles","volume","position","cumulative","rolling","period","drawdowns","table","footer")
    mods_final <- intersect(priority, mods_avail)
  } else {
    missing_req <- setdiff(modules, mods_avail)
    if (length(missing_req)) {
      stop(sprintf("Requested modules not available: %s", paste(missing_req, collapse = ", ")))
    }
    mods_final <- modules
  }
  if (length(mods_final) == 0)
    stop("No available module for provided data.")

  if (format == "json"){
    resultado <- .tplot_render_json(prep, mods_final)
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio,2), "\n")
    cat("Rendering: ", round(termino - preparo,2), "\n")
    return(invisible(resultado))
  }
  if (format %in% c("html", "viewer")){
    resultado <- .tplot_render_html(prep,
                                    mods_final,
                                    theme,
                                    output_dir = if (format == "html") output_dir else NULL,
                                    viewer     = (format == "viewer"))
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio,2), "\n")
    cat("Rendering: ", round(termino - preparo,2), "\n")
    return(invisible(resultado))
  }

  if (format %in% c("png","jpg")) {
    resultado <- .tplot_render_image(prep, mods_final, theme,
                                     format     = format,
                                     output_dir = output_dir)
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio,2), "\n")
    cat("Rendering: ", round(termino - preparo,2), "\n")
    return(invisible(resultado))
  }
  invisible(NULL)
}

#' Plot a quantstrat/blotter Portfolio similar to chart.Posn, enriched with tplot modules
#' This is a convenience wrapper that prioritizes portfolio data (mktdata + trades)
#' while still rendering the other tplot modules (stats, cumulative, etc.).
#'
#' @param portfolio The portfolio name (as used with blotter/quantstrat).
#' @param symbol Optional symbol to focus when the portfolio contains multiple symbols.
#' @param ... Optional additional tickers/benchmarks to compare against.
#' @inheritParams tplot
#'
#' @export
tplot_posn <- function(portfolio,
                       symbol     = NULL,
                       ...,
                       init       = "1994-08-01",
                       finit      = Sys.Date(),
                       rf_rate    = NULL,
                       auto_rets  = FALSE,
                       format     = c("viewer","html","json","png","jpg"),
                       output_dir = "tplots",
                       modules    = c("stats","candles","position","cumulative","rolling",
                                      "period","drawdowns","table","footer"),
                       theme      = default_theme(),
                       verbose    = getOption("tplot.verbose", FALSE)){
  # Provide a hint so .tplot_prepare fetches portfolio-first
  hint <- list(name = as.character(portfolio))
  if (!is.null(symbol)) hint$symbol <- as.character(symbol)
  old_opt <- getOption("tplot.portfolio_hint", NULL)
  on.exit(options(tplot.portfolio_hint = old_opt), add = TRUE)
  options(tplot.portfolio_hint = hint)

  # Decide the primary symbol for returns/candles
  chosen_symbol <- NULL
  # Try to resolve quickly to have a stable label/order
  qs <- try(.quantstrat_portfolio_data(as.character(portfolio), init, finit), silent = TRUE)
  if (is.list(qs) && !is.null(qs$symbol)) chosen_symbol <- qs$symbol
  if (!is.null(symbol)) chosen_symbol <- as.character(symbol)
  if (is.null(chosen_symbol)) chosen_symbol <- as.character(portfolio)

  # Dispatch to tplot with the chosen symbol as the first asset
  tplot(chosen_symbol, ..., init = init, finit = finit, rf_rate = rf_rate,
        auto_rets = auto_rets, format = format, output_dir = output_dir,
        modules = modules, theme = theme, verbose = verbose)
}

#' @title Launch an Interactive tplot Viewer with Shiny
#' @description
#' An interactive variant of tplot that opens a Shiny application, allowing the user
#' to edit, add, and remove assets in real-time to compare performance.
#'
#' @param ... Initial ticker symbols or xts/data.frame objects.
#' @param init Start date for the analysis.
#' @param finit End date for the analysis.
#' @param rf_rate The risk-free rate.
#' @param auto_rets Logical, for geometric returns.
#' @param modules Modules to display in the app.
#' @param theme The theme function to use.
#'
#' @return Launches a Shiny application. There is no direct return value.
#'
#' @export
tplot_interactive <- function(...,
                              init = "1994-08-01",
                              finit = Sys.Date(),
                              rf_rate = NULL,
                              auto_rets = FALSE,
                              modules = c("stats","candles","volume","position", "cumulative","rolling", "period","drawdowns"),
                              theme = default_theme()) {

  mc <- match.call(expand.dots = FALSE)
  dot_exprs <- mc$...
  if (is.null(dot_exprs) || length(dot_exprs) == 0) stop("Provide at least one ticker as first argument (string or xts object).")

  seen <- new.env(parent = emptyenv())
  next_label <- function(lbl) {
    if (is.null(lbl) || lbl == "") lbl <- "Serie"
    if (is.null(seen[[lbl]])) { seen[[lbl]] <- 1L; lbl } else { seen[[lbl]] <- seen[[lbl]] + 1L; paste0(lbl, " (", seen[[lbl]], ")") }
  }
  items <- list()
  add_xts_item <- function(obj, label) {
    conv <- .to_xts(obj)
    if (.is_xts(conv)) {
      label_u <- next_label(label)
      items[[length(items) + 1L]] <<- list(label = label_u, type = "xts", value = conv)
    }
  }
  add_char_item <- function(sym) {
    if (is.character(sym)) for (s in sym) if (nzchar(s)) items[[length(items) + 1L]] <<- list(label = s, type = "char", value = s)
  }
  parse_one <- function(ex) {
    if (is.symbol(ex)) {
      nm <- as.character(ex)
      obj <- .get_object_if_exists(nm)
      if (!is.null(obj)) {
        if (.is_xts(obj) || is.matrix(obj) || is.data.frame(obj) || inherits(obj, "zoo")) {
          add_xts_item(obj, nm)
        } else if (is.character(obj) && length(obj) >= 1L) {
          add_char_item(obj)
        } else {
          add_char_item(nm)
        }
      } else {
        add_char_item(nm)
      }
      return(invisible(NULL))
    }
    if (is.character(ex)) { add_char_item(ex); return(invisible(NULL)) }
    if (is.call(ex)) {
      hd <- tryCatch(as.character(ex[[1]]), error = function(e) "")
      if (hd %in% c("c", "list")) {
        args <- as.list(ex)[-1]
        for (a in args) parse_one(a)
        return(invisible(NULL))
      }
      obj <- tryCatch(eval(ex, envir = parent.frame()), error = function(e) NULL)
      lbl <- paste0(substr(paste(deparse(ex), collapse = ""), 1, 20))
      if (.is_xts(obj) || is.matrix(obj) || is.data.frame(obj) || inherits(obj, "zoo")) {
        add_xts_item(obj, lbl)
      } else if (is.character(obj) && length(obj) >= 1L) {
        add_char_item(obj)
      }
      return(invisible(NULL))
    }
    val <- tryCatch(eval(ex, envir = parent.frame()), error = function(e) NULL)
    if (.is_xts(val) || is.matrix(val) || is.data.frame(val) || inherits(val, "zoo")) add_xts_item(val, "Serie") else if (is.character(val) && length(val) >= 1L) add_char_item(val)
    invisible(NULL)
  }
  for (ex in as.list(dot_exprs)) parse_one(ex)
  if (length(items) == 0) stop("No valid ticker identified in ...")

  # build initial series list: each element is list(type, value, label, source)
  make_series_from_items <- function(items) {
    s <- list()
    for (it in items) {
      if (it$type == "xts") {
        s[[length(s)+1]] <- list(type = "xts", value = it$value, label = it$label, source = it$label)
      } else {
        s[[length(s)+1]] <- list(type = "char", value = as.character(it$value), label = as.character(it$value), source = as.character(it$value))
      }
    }
    s
  }
  series_list <- make_series_from_items(items)

  # helpers to convert series_list -> args for .tplot_prepare
  series_to_prep_args <- function(series) {
    if (length(series) == 0) return(NULL)
    ativo_el <- series[[1]]
    ativoArg <- if (ativo_el$type == "xts") ativo_el$value else as.character(ativo_el$value)
    ativo_label <- ativo_el$label
    bench_xts <- list()
    bench_chars <- character(0)
    if (length(series) > 1) {
      for (i in seq(2, length(series))) {
        el <- series[[i]]
        if (el$type == "xts") bench_xts[[el$label]] <- el$value else bench_chars <- c(bench_chars, as.character(el$value))
      }
    }
    benchesArg <- NULL
    if (length(bench_xts) > 0 && length(bench_chars) > 0) benchesArg <- c(bench_xts, as.list(bench_chars) )
    else if (length(bench_xts) > 0) benchesArg <- bench_xts
    else if (length(bench_chars) > 0) benchesArg <- bench_chars
    list(ativoArg = ativoArg, benchesArg = benchesArg, ativo_label = ativo_label)
  }

  # helper to test .tplot_prepare with candidate series
  test_prepare <- function(series) {
    args <- series_to_prep_args(series)
    if (is.null(args)) return(NULL)
    tryCatch({
      .tplot_prepare(args$ativoArg, args$benchesArg, init, finit, rf_rate, auto_rets, ativo_name = args$ativo_label)
    }, error = function(e) { NULL })
  }

  # (duplication helper removed - duplication feature disabled)

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(HTML(sprintf(
        "html,body{background:%s;color:%s;font-family:%s} #controls{margin:6px 0 12px 0;font-size:12px} .mini-btn{cursor:pointer;padding:2px 6px;border-radius:3px;border:1px solid rgba(0,0,0,0.1);margin-left:6px} .rem-btn{color:red} .series-name{cursor:pointer;font-weight:bold}",
        theme$colors$page_bg, theme$colors$page_txt, theme$font_family
      )))
    ),
    shiny::div(id = "controls", ""),
    shiny::uiOutput("stats_ui"),
    shiny::fluidRow(
      shiny::column(12, highcharter::highchartOutput("cum_chart", height = "420px")),
      shiny::column(12, highcharter::highchartOutput("period_chart", height = "260px")),
      shiny::column(12, highcharter::highchartOutput("dd_chart", height = "260px"))
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(series = series_list)

    # reactive that prepares data using current rv$series
    prep_reactive <- shiny::reactive({
      s <- isolate(rv$series)
      args <- series_to_prep_args(s)
      if (is.null(args)) return(NULL)
      res <- tryCatch(.tplot_prepare(args$ativoArg, args$benchesArg, init, finit, rf_rate, auto_rets, ativo_name = args$ativo_label),
                      error = function(e) { shiny::showNotification(paste("Error preparing data:", e$message), type = "error"); NULL })
      # if success, align rv$series order/labels with res$carteira
      if (!is.null(res)) {
        cols <- colnames(res$carteira)
        # attempt to match and reorder
        cur <- rv$series
        new_series <- vector("list", length(cols))
        for (i in seq_along(cols)) {
          col <- cols[i]
          found <- NA
          for (j in seq_along(cur)) {
            el <- cur[[j]]
            if (!is.null(el$label) && el$label == col) { found <- j; break }
            if (el$type == 'char' && el$value == col) { found <- j; break }
          }
          if (!is.na(found)) {
            new_series[[i]] <- cur[[found]]
            new_series[[i]]$label <- col
            cur[[found]] <- NULL
          } else {
            new_series[[i]] <- list(type = 'char', value = col, label = col, source = col)
          }
        }
        rv$series <- new_series
      }
      res
    })

    # render stats table built from prepared data but using our interactive controls
    output$stats_ui <- shiny::renderUI({
      # If the user removed all tickers, still render the table headers so they
      # can add new tickers. Otherwise prepare data normally.
      cur_series <- isolate(rv$series)
      if (length(cur_series) == 0L) {
        # header-only table (same columns as stats_module)
        cols <- c("Tickers", "Total", "CAR", "MaxDD", "Std_Dev", "Sharpe", "Sortino")
        cl <- theme$colors
        header_cells <- lapply(cols, function(col) {
          if (col == 'Tickers') {
            tags$th(style = sprintf('background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);', cl$table_header_bg, cl$table_header_txt, theme$font_family, theme$font_sizes$table),
                    tags$span(col),
                    shiny::actionButton('header_add', '+', class = 'mini-btn', style = 'margin-left:8px;')
            )
          } else {
            tags$th(col, style = sprintf('background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);', cl$table_header_bg, cl$table_header_txt, theme$font_family, theme$font_sizes$table))
          }
        })
        tbl <- tags$table(style = sprintf('min-width:100%%;font-family:%s;table-layout:auto;white-space:nowrap;border-collapse:collapse;background-color:%s;', theme$font_family, cl$page_bg), tags$thead(tags$tr(header_cells)), tags$tbody())
        js <- tags$script(HTML("$(document).off('click.tplot_interactive');
          $(document).on('click', '.series-name', function(){ var idx = $(this).data('row'); Shiny.setInputValue('tplot_edit_click', {index: idx, nonce: Math.random()}); });
          $(document).on('click', '.rem-btn', function(){ var idx = $(this).data('row'); Shiny.setInputValue('tplot_remove', {index: idx, nonce: Math.random()}); });"))
        return(tagList(tbl, js))
      }

      p <- prep_reactive()
      if (is.null(p)) return(shiny::HTML("<div>No data</div>"))
      df <- p$carteira_df
      pal <- theme$palette; cl <- theme$colors

      thead <- tags$tr(lapply(colnames(df), function(col) tags$th(col, style = sprintf('background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);', cl$table_header_bg, cl$table_header_txt, theme$font_family, theme$font_sizes$table))))

      tbody_rows <- lapply(seq_len(nrow(df)), function(i) {
        lab <- as.character(df$Ativos[i])
        # find index in rv$series (1-based)
        idx <- which(sapply(isolate(rv$series), function(x) x$label) == lab)
        if (length(idx) == 0) idx <- i
        pal_idx <- match(lab, sapply(isolate(rv$series), function(x) x$label))
        pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
        rgb_col <- tryCatch(grDevices::col2rgb(pal_col), error = function(e) matrix(c(200,200,200), ncol=1))
        bg <- paste0('rgba(', paste(rgb_col[,1], collapse=','), ',0.2)')
        first_td <- tags$td(
          tags$span(lab, class = 'series-name', `data-row` = idx),
          tags$span('-', class = 'mini-btn rem-btn', `data-row` = idx, title = 'Remover'),
          style = sprintf('padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);', theme$font_family, theme$font_sizes$table, cl$table_row_txt)
        )
        other_tds <- lapply(colnames(df)[-1], function(col) tags$td(as.character(df[i, col]), style = sprintf('padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);', theme$font_family, theme$font_sizes$table, cl$table_row_txt)))
        tags$tr(`data-row` = idx, style = sprintf('background-color:%s;', bg), first_td, other_tds)
      })

      # header: add a + button in the 'Ativos' header cell
      header_cells <- lapply(colnames(df), function(col) {
        if (col == 'Ativos') {
          tags$th(style = sprintf('background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);', cl$table_header_bg, cl$table_header_txt, theme$font_family, theme$font_sizes$table),
                  tags$span(col),
                  shiny::actionButton('header_add', '+', class = 'mini-btn', style = 'margin-left:8px;')
          )
        } else {
          tags$th(col, style = sprintf('background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);', cl$table_header_bg, cl$table_header_txt, theme$font_family, theme$font_sizes$table))
        }
      })

      tbl <- tags$table(style = sprintf('min-width:100%%;font-family:%s;table-layout:auto;white-space:nowrap;border-collapse:collapse;background-color:%s;', theme$font_family, cl$page_bg), tags$thead(tags$tr(header_cells)), tags$tbody(tbody_rows))

      # JS to delegate clicks
      js <- tags$script(HTML("$(document).off('click.tplot_interactive');
        $(document).on('click', '.series-name', function(){ var idx = $(this).data('row'); Shiny.setInputValue('tplot_edit_click', {index: idx, nonce: Math.random()}); });
        $(document).on('click', '.rem-btn', function(){ var idx = $(this).data('row'); Shiny.setInputValue('tplot_remove', {index: idx, nonce: Math.random()}); });"))

      tagList(tbl, js)
    })

    # Edit flow
    shiny::observeEvent(input$tplot_edit_click, {
      idx <- input$tplot_edit_click$index
      rv$editingIndex <- idx
      cur <- isolate(rv$series[[idx]])
      cur_label <- if (!is.null(cur$label)) cur$label else as.character(cur$value)
      shiny::showModal(shiny::modalDialog(
        title = paste("Edit ticker:", cur_label),
        shiny::textInput("tplot_edit_input", "New ticker:", value = cur_label),
        shiny::tags$script(HTML("$('#tplot_edit_input').on('keydown', function(e){ if(e.key === 'Enter'){ var el=$(this); el.trigger('change'); setTimeout(function(){ $('#tplot_confirm_edit').click(); }, 50); } });")),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("tplot_confirm_edit", "Confirmar")),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$tplot_confirm_edit, {
      shiny::removeModal()
      idx <- isolate(rv$editingIndex)
      newsym <- shiny::isolate(input$tplot_edit_input)
      if (is.null(newsym) || !nzchar(newsym)) { shiny::showNotification("Empty ticker.", type = "error"); return() }
      # build candidate series list
      cand <- isolate(rv$series)
      cand[[idx]] <- list(type = 'char', value = as.character(newsym), label = as.character(newsym), source = as.character(newsym))
      test <- test_prepare(cand)
      if (is.null(test)) {
        shiny::showNotification(paste("Erro ao obter dados para", newsym), type = "error")
      } else {
        rv$series <- cand
      }
    })

    # Add from header
    shiny::observeEvent(input$header_add, {
      shiny::showModal(shiny::modalDialog(
        title = "Adicionar ativo",
        shiny::textInput("tplot_add_input", "Ticker:", value = ""),
        shiny::tags$script(HTML("$('#tplot_add_input').on('keydown', function(e){ if(e.key === 'Enter'){ var el=$(this); el.trigger('change'); setTimeout(function(){ $('#tplot_confirm_add').click(); }, 50); } });")),
        footer = shiny::tagList(shiny::modalButton("Cancelar"), shiny::actionButton("tplot_confirm_add", "Adicionar")),
        easyClose = TRUE
      ))
    })
    shiny::observeEvent(input$tplot_confirm_add, {
      shiny::removeModal()
      newsym <- shiny::isolate(input$tplot_add_input)
      if (is.null(newsym) || !nzchar(newsym)) { shiny::showNotification("Empty ticker.", type = "error"); return() }
      cand <- isolate(rv$series)
      cand[[length(cand)+1]] <- list(type = 'char', value = as.character(newsym), label = as.character(newsym), source = as.character(newsym))
      test <- test_prepare(cand)
      if (is.null(test)) {
        shiny::showNotification(paste("Erro ao obter dados para", newsym), type = "error")
      } else {
        rv$series <- cand
      }
    })

    # Remove
    shiny::observeEvent(input$tplot_remove, {
      idx <- input$tplot_remove$index
      cand <- isolate(rv$series)
      if (idx > length(cand) || idx < 1) return()
      cand <- cand[-idx]
      rv$series <- cand
    })

    # charts
    render_common_chart <- function(chart_type) {
      p <- prep_reactive()
      if (is.null(p)) return(NULL)
      syms <- colnames(p$carteira)
      pal <- theme$palette
      hc <- highcharter::highchart() %>%
        highcharter::hc_chart(type = "line", backgroundColor = theme$colors$chart_bg) %>%
        highcharter::hc_xAxis(type = "datetime") %>%
        highcharter::hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>")
      datas <- p$datas
      for (i in seq_along(syms)) {
        nm <- syms[i]
        vec <- switch(chart_type, cum = p$ret_cum[, nm], period = p$ret_sim[, nm], dd = p$dds[, nm])
        df <- data.frame(x = datas, y = as.numeric(vec))
        col <- pal[ ((i-1) %% length(pal)) + 1]
        hc <- hc %>% highcharter::hc_add_series(data = highcharter::list_parse2(df), type = "line", name = nm, id = nm, color = col)
      }
      hc
    }

    output$cum_chart <- highcharter::renderHighchart({ render_common_chart("cum") })
    output$period_chart <- highcharter::renderHighchart({ render_common_chart("period") })
    output$dd_chart <- highcharter::renderHighchart({ render_common_chart("dd") })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app)
}
