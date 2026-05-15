#' @importFrom stats setNames
#' @importFrom utils tail
#' @importFrom zoo index
#' @importFrom zoo "index<-"
#' @noRd
NULL
#' @title Generate a Performance Report for Financial Assets
#' @description
#' Creates a complete and customizable performance report for one or more financial assets,
#' comparing them against benchmarks. The output can be an interactive viewer, an HTML
#' file, a JSON string, or a static image (PNG/JPG).
#'
#' @param ... Ticker symbols (e.g., "PETR4.SA"), xts/data.frame objects, or
#' backtest-like lists with a `rets` element. The first item is treated as the
#' main asset, and the rest as benchmarks.
#' @param init Start date for the analysis in "YYYY-MM-DD" format.
#' @param finit End date for the analysis in "YYYY-MM-DD" format.
#' @param rf_rate The risk-free rate for calculating metrics like the Sharpe Ratio.
#' @param geometric Logical. If `TRUE`, calculates geometric returns. If `FALSE`, arithmetic.
#' @param normalize_risk Target annualized volatility used to scale all series before plotting. Use `NULL` to skip.
#' @param format The output format: "viewer", "html", "json", "png", or "jpg".
#' @param output_dir The directory where output files (HTML, PNG, etc.) will be saved.
#' @param modules A character vector of modules to include in the report.
#' @param theme The theme function to use (e.g., `default_theme()` or `dark_theme()`).
#' @param verbose Print more info for debugging purposes
#'
#' @return Invisibly returns the path to the generated file (for "html", "png", "jpg"),
#' the JSON string (for "json"), or launches the interactive viewer (for "viewer").
#'
#' @export
tplot <- function(...,
                  init = "1994-08-01",
                  finit = Sys.Date(),
                  rf_rate = NULL,
                  geometric = TRUE,
                  normalize_risk = NULL,
                  format = c("viewer", "html", "json", "png", "jpg"),
                  output_dir = "tplots",
                  modules = c(
                    "stats", "candles", "volume", "position", "cumulative", "rolling", "rolling_corr",
                    "costs", "trade_quality", "period", "drawdowns", "table", "footer"
                  ),
                  theme = default_theme(),
                  verbose = getOption("tplot.verbose", FALSE)) {
  inicio <- Sys.time()
  format <- match.arg(format)
  user_specified_modules <- !missing(modules)
  modules <- match.arg(modules,
    several.ok = TRUE,
    choices = c(
      "stats", "costs", "trade_quality", "candles", "volume", "position", "cumulative", "rolling", "rolling_corr",
      "period", "drawdowns", "table", "footer"
    )
  )

  # Parse variable tickers: first is main (asset), others are secondary
  mc <- match.call(expand.dots = FALSE)
  dot_exprs <- mc$...
  if (is.null(dot_exprs) || length(dot_exprs) == 0) {
    stop("Provide at least on ticker as first argument (string or xts/data.frame object).")
  }

  # helper to generate unique labels for xts entries
  seen <- new.env(parent = emptyenv())
  next_label <- function(lbl) {
    if (is.null(lbl) || lbl == "") lbl <- "Serie"
    if (is.null(seen[[lbl]])) {
      seen[[lbl]] <- 1L
      return(lbl)
    } else {
      seen[[lbl]] <- seen[[lbl]] + 1L
      return(paste0(lbl, "_", seen[[lbl]]))
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
  add_obj_item <- function(obj, label) {
    if (!is.list(obj)) {
      return(FALSE)
    }
    nested_bt <- list()
    if (length(obj) > 0) {
      nms <- names(obj)
      for (i in seq_along(obj)) {
        bt <- .as_backtest(obj[[i]])
        if (!is.null(bt)) {
          lbl <- nms[i]
          if (is.null(lbl) || !nzchar(lbl)) lbl <- paste0(label, "_", i)
          base_lbl <- lbl
          dup_idx <- 1L
          while (!is.null(nested_bt[[lbl]])) {
            dup_idx <- dup_idx + 1L
            lbl <- paste0(base_lbl, "_", dup_idx)
          }
          nested_bt[[lbl]] <- obj[[i]]
        }
      }
    }
    if (length(nested_bt) > 0) {
      for (nm in names(nested_bt)) {
        label_u <- next_label(nm)
        items[[length(items) + 1L]] <<- list(label = label_u, type = "obj", value = nested_bt[[nm]])
      }
      return(TRUE)
    }
    bt_self <- .as_backtest(obj)
    if (!is.null(bt_self)) {
      label_u <- next_label(label)
      items[[length(items) + 1L]] <<- list(label = label_u, type = "obj", value = obj)
      return(TRUE)
    }
    FALSE
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
        } else if (add_obj_item(obj, nm)) {
          return(invisible(NULL))
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
      } else if (add_obj_item(obj, lbl)) {
        return(invisible(NULL))
      }
      return(invisible(NULL))
    }
    val <- tryCatch(eval(ex, envir = parent.frame()), error = function(e) NULL)
    if (.is_xts(val) || is.matrix(val) || is.data.frame(val) || inherits(val, "zoo")) {
      add_xts_item(val, "Serie")
    } else if (is.character(val) && length(val) >= 1L) {
      for (s in val) add_char_item(s)
    } else if (add_obj_item(val, "Serie")) {
      return(invisible(NULL))
    }
    invisible(NULL)
  }
  for (ex in as.list(dot_exprs)) parse_one(ex)
  if (length(items) == 0) stop("No valid ticker identified in ...")

  # split into asset (first) and benchs (rest)
  ativo_item <- items[[1]]
  bench_items <- if (length(items) > 1) items[-1] else list()

  ativo_spec <- ativo_item$value
  ativo_label <- if (ativo_item$type %in% c("xts", "obj")) ativo_item$label else ativo_item$value

  bench_list <- list()
  bench_chars <- character(0)
  for (bi in bench_items) {
    if (bi$type %in% c("xts", "obj")) {
      bench_list[[bi$label]] <- bi$value
    } else {
      bench_chars <- c(bench_chars, bi$value)
    }
  }
  benchs_spec <- if (length(bench_list) == 0 && length(bench_chars) == 0) NULL else c(bench_list, as.list(bench_chars))

  # palette sizing by number of series
  n_assets <- 1L + length(bench_items)
  if (length(theme$palette) < n_assets) {
    theme$palette <- colorRampPalette(theme$palette)(n_assets)
  }

  prep <- .tplot_prepare(ativo_spec, benchs_spec, init, finit, rf_rate, geometric,
    normalize_risk = normalize_risk,
    ativo_name = ativo_label,
    verbose = verbose
  )
  preparo <- Sys.time()
  mods_avail <- .available_modules(prep)
  if (!user_specified_modules) {
    priority <- c("stats", "candles", "volume", "position", "cumulative", "rolling", "rolling_corr", "costs", "trade_quality", "period", "drawdowns", "table", "footer")
    mods_final <- intersect(priority, mods_avail)
  } else {
    missing_req <- setdiff(modules, mods_avail)
    if (length(missing_req)) {
      stop(sprintf("Requested modules not available: %s", paste(missing_req, collapse = ", ")))
    }
    mods_final <- modules
  }
  if (length(mods_final) == 0) {
    stop("No available module for provided data.")
  }

  if (format == "json") {
    resultado <- .tplot_render_json(prep, mods_final)
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio, 2), "\n")
    cat("Rendering: ", round(termino - preparo, 2), "\n")
    return(invisible(resultado))
  }
  if (format %in% c("html", "viewer")) {
    resultado <- .tplot_render_html(prep,
      mods_final,
      theme,
      output_dir = if (format == "html") output_dir else NULL,
      viewer     = (format == "viewer")
    )
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio, 2), "\n")
    cat("Rendering: ", round(termino - preparo, 2), "\n")
    return(invisible(resultado))
  }

  if (format %in% c("png", "jpg")) {
    resultado <- .tplot_render_image(prep, mods_final, theme,
      format     = format,
      output_dir = output_dir
    )
    termino <- Sys.time()
    cat("Preparing: ", round(preparo - inicio, 2), "\n")
    cat("Rendering: ", round(termino - preparo, 2), "\n")
    return(invisible(resultado))
  }
  invisible(NULL)
}
