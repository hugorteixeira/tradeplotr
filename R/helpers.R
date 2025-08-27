## Data detection and sourcing helpers


#' Get a function by name if it exists in the environment
#' @param name The name of the function to look for.
#' @return The function object, or NULL if not found.
#' @keywords internal
.get_function_if_exists <- function(name) {
  fn <- tryCatch(get(name, mode = "function", inherits = TRUE), error = function(e) NULL)
  if (is.function(fn)) fn else NULL
}

#' Get an object by name from common environments
#' @param name The name of the object to look for.
#' @param envs A list of environments to search in.
#' @return The object, or NULL if not found.
#' @keywords internal
.get_object_if_exists <- function(name, envs = NULL) {
  # If a concrete object was provided (e.g., xts), just return it
  if (!is.character(name) || length(name) != 1) {
    return(name)
  }
  # Build a robust search path across parent frames to handle wrappers
  if (is.null(envs)) {
    envs <- list()
    # search up to 6 parent frames, then .GlobalEnv
    for (i in 1:6) {
      ev <- try(parent.frame(i), silent = TRUE)
      if (!inherits(ev, "try-error") && is.environment(ev)) envs[[length(envs)+1]] <- ev
    }
    envs[[length(envs)+1]] <- .GlobalEnv
  }
  for (ev in envs) {
    if (is.environment(ev) && exists(name, envir = ev, inherits = TRUE)) {
      obj <- get(name, envir = ev, inherits = TRUE)
      return(obj)
    }
  }
  NULL
}

#' Check if an object is of class xts
#' @param x The object to check.
#' @return TRUE if the object is an xts object, FALSE otherwise.
#' @keywords internal
.is_xts <- function(x) inherits(x, "xts")

#' Attempt to coerce common tabular structures into xts
#' @param x The object to convert (e.g., data.frame, matrix, zoo).
#' @return An xts object, or NULL on failure.
#' @keywords internal
.to_xts <- function(x) {
  if (.is_xts(x)) return(x)
  # zoo is easy to promote
  if (inherits(x, "zoo")) return(xts::as.xts(x))
  # matrices/data.frames/tibbles
  if (is.matrix(x) || is.data.frame(x)) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    # detect index: prefer explicit date/time columns
    idx_col <- NULL
    cn <- colnames(df)
    if (!is.null(cn)) {
      idx_col <- which(tolower(cn) %in% c("date","data","datetime","time","index"))
      if (length(idx_col) > 0) idx_col <- idx_col[1] else idx_col <- NULL
    }
    if (!is.null(idx_col)) {
      idx <- df[[idx_col]]
      df[[idx_col]] <- NULL
    } else {
      # fallback: try rownames
      rn <- rownames(df)
      if (!is.null(rn) && length(rn) == nrow(df)) {
        idx <- rn
      } else {
        # cannot infer index
        return(NULL)
      }
    }
    # coerce index to POSIXct or Date
    idx_parsed <- suppressWarnings(as.POSIXct(idx, tz = "UTC"))
    if (all(is.na(idx_parsed))) idx_parsed <- suppressWarnings(as.Date(idx))
    if (all(is.na(idx_parsed))) return(NULL)
    # coerce numeric columns
    for (j in seq_along(df)) {
      if (!is.numeric(df[[j]])) {
        df[[j]] <- suppressWarnings(as.numeric(as.character(df[[j]])))
      }
    }
    # drop all-NA columns
    keep <- vapply(df, function(col) any(!is.na(col)), logical(1))
    df <- df[, keep, drop = FALSE]
    if (ncol(df) == 0) return(NULL)
    # build xts and sort by index
    x_out <- xts::xts(df, order.by = idx_parsed)
    x_out <- x_out[order(index(x_out))]
    return(x_out)
  }
  NULL
}

#' Check if an xts object has OHLC columns
#' @param x The xts object.
#' @return TRUE if it has OHLC columns, FALSE otherwise.
#' @keywords internal
.is_ohlc_xts <- function(x) {
  if (!.is_xts(x)) return(FALSE)
  cols <- tolower(colnames(x))
  # Handle plain OHLC names and quantmod-style prefixed columns (e.g., TICKER.Open)
  has_plain <- all(c("open","high","low","close") %in% cols)
  if (has_plain) return(TRUE)
  # Prefix-aware: look for ".Open", ".High", ".Low", ".Close"
  has_pref <- all(vapply(c("\\.open$","\\.high$","\\.low$","\\.close$"),
                        function(re) any(grepl(re, cols, ignore.case = TRUE)),
                        logical(1)))
  has_pref
}

#' Coerce an xts object to a standard OHLC shape expected by highcharter
#' Names the columns as Open, High, Low, Close, optionally preserving Volume.
#' @param x xts object with any OHLC-like column names
#' @return xts with 4 (or 5 with Volume) columns or NULL if cannot map
#' @keywords internal
.to_ohlc_standard <- function(x) {
  if (!.is_xts(x)) return(NULL)
  cols <- colnames(x)
  cl   <- tolower(cols)
  pick <- function(regex) {
    idx <- which(grepl(regex, cl, ignore.case = TRUE))
    if (length(idx)) idx[1] else NA_integer_
  }
  io <- pick("(^|\\.)open$")
  ih <- pick("(^|\\.)high$")
  il <- pick("(^|\\.)low$")
  ic <- pick("(^|\\.)close$")
  iv <- pick("(^|\\.)volume$|(^|\\.)vol$")
  if (any(is.na(c(io, ih, il, ic)))) return(NULL)
  ohlc <- x[, c(io, ih, il, ic), drop = FALSE]
  colnames(ohlc) <- c("Open","High","Low","Close")
  if (!is.na(iv)) {
    vol <- x[, iv, drop = FALSE]
    colnames(vol) <- "Volume"
    ohlc <- cbind(ohlc, vol)
  }
  ohlc
}

#' Try to interpret a list as a backtest-like object
#' @param obj The list to check.
#' @return A list with mktdata, trades, and stats, or NULL.
#' @keywords internal
.as_backtest <- function(obj) {
  if (is.list(obj)) {
    has_mkt <- !is.null(obj$mktdata) && .is_xts(obj$mktdata)
    has_txn <- !is.null(obj$trades)  && .is_xts(obj$trades)
    if (has_mkt && has_txn) {
      return(list(
        mktdata = obj$mktdata,
        trades  = obj$trades,
        stats   = obj$stats %||% NULL
      ))
    }
  }
  NULL
}

#' Try to resolve a blotter/quantstrat portfolio by name
#' @param name Portfolio name (character).
#' @param init Start date to subset.
#' @param finit End date to subset.
#' @return list(mktdata=xts or NULL, trades=xts or NULL, symbol=character) or NULL
#' @keywords internal
.quantstrat_portfolio_data <- function(name, init = NULL, finit = NULL){
  if (is.null(name) || !is.character(name) || length(name) != 1L) return(NULL)
  getPortfolio <- .get_function_if_exists("getPortfolio")
  getTxns      <- .get_function_if_exists("getTxns")
  if (is.null(getPortfolio)) return(NULL)
  pf <- tryCatch(getPortfolio(name), error = function(e) NULL)
  if (is.null(pf) || is.null(pf$symbols)) return(NULL)
  syms <- names(pf$symbols)
  if (length(syms) == 0) return(NULL)
  # Prefer a symbol that actually has transactions
  chosen <- syms[1]
  if (!is.null(getTxns)) {
    for (s in syms) {
      tx_try <- tryCatch(getTxns(Portfolio = name, Symbol = s), error = function(e) NULL)
      if (.is_xts(tx_try) && NROW(tx_try) > 0) { chosen <- s; break }
    }
  }
  # Get transactions for the chosen symbol if possible
  trades <- if (!is.null(getTxns)) tryCatch(getTxns(Portfolio = name, Symbol = chosen), error = function(e) NULL) else NULL
  if (.is_xts(trades)) trades <- .subset_xts(trades, init, finit)
  # Try to locate OHLC market data in the environment first
  mktdata <- .get_object_if_exists(chosen)
  if (.is_xts(mktdata)) mktdata <- .subset_xts(mktdata, init, finit) else mktdata <- NULL
  # If no suitable OHLC found in env, try user's API, then Yahoo via quantmod
  if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
    od <- .get_function_if_exists("sm_get_data")
    if (!is.null(od)) {
      fetched <- tryCatch(od(chosen, start_date = init, end_date = finit, auto_returns = FALSE), error = function(e) NULL)
      if (.is_xts(fetched)) {
        mktdata <- .subset_xts(fetched, init, finit)
      } else if (is.list(fetched) && length(fetched) > 0) {
        cand <- fetched[[1]]
        if (.is_xts(cand)) mktdata <- .subset_xts(cand, init, finit)
      }
    }
  }
  if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
    # Last resort: Yahoo
    obj <- tryCatch(
      suppressWarnings(quantmod::getSymbols(chosen, from = init, to = finit, auto.assign = FALSE)),
      error = function(e) NULL
    )
    if (.is_xts(obj)) mktdata <- .subset_xts(obj, init, finit)
  }
  list(mktdata = mktdata, trades = trades, symbol = chosen)
}

#' Safely subset an xts object by a date range
#' @param x The xts object.
#' @param init Start date.
#' @param finit End date.
#' @return The subsetted xts object.
#' @keywords internal
.subset_xts <- function(x, init, finit) {
  if (!.is_xts(x)) return(x)
  rng <- paste0(as.Date(init), "/", as.Date(finit))
  tryCatch(x[rng], error = function(e) x)
}

#' Find any xts candidates for the provided names in the environment
#' @param symbols A character vector of symbol names.
#' @param init Start date.
#' @param finit End date.
#' @return A list of found xts objects.
#' @keywords internal
.find_series_in_env <- function(symbols, init, finit) {
  out <- list()
  if (is.null(symbols) || length(symbols) == 0) return(out)
  idxs <- seq_along(symbols)
  for (i in idxs) {
    nm <- symbols[[i]]
    label <- names(symbols)[i]
    if (is.null(label) || !nzchar(label)) label <- if (is.character(nm)) nm else paste0("Serie", i)
    if (is.null(nm)) next
    # allow the user to pass an xts object directly instead of a name
    if (.is_xts(nm)) {
      out[[label]] <- .subset_xts(nm, init, finit)
      next
    }
    # allow passing data.frame/matrix directly
    if (is.matrix(nm) || is.data.frame(nm) || inherits(nm, "zoo")) {
      conv <- .to_xts(nm)
      if (.is_xts(conv)) {
        out[[label]] <- .subset_xts(conv, init, finit)
        next
      }
    }
    if (!is.character(nm) || length(nm) != 1) next
    obj <- .get_object_if_exists(nm)
    if (.is_xts(obj)) {
      out[[label]] <- .subset_xts(obj, init, finit)
    } else if (is.matrix(obj) || is.data.frame(obj) || inherits(obj, "zoo")) {
      conv <- .to_xts(obj)
      if (.is_xts(conv)) out[[label]] <- .subset_xts(conv, init, finit)
    } else if (is.list(obj) && !is.null(obj$prices) && .is_xts(obj$prices)) {
      # common wrapper structure
      out[[label]] <- .subset_xts(obj$prices, init, finit)
    }
  }
  out
}

#' Determine which modules can be rendered based on prepared data
#' @param prep The list of data prepared by .tplot_prepare.
#' @return A character vector with the names of available modules.
#' @keywords internal
.available_modules <- function(prep) {
  mods <- c()
  if (!is.null(prep$carteira) && NROW(prep$carteira) > 0) {
    mods <- c(mods, "stats", "cumulative", "rolling", "period", "drawdowns", "table", "footer")
  }
  # Candles is available if we have any OHLC mktdata
  if (!is.null(prep$mktdata) && .is_xts(prep$mktdata) && .is_ohlc_xts(prep$mktdata)) {
    mods <- c(mods, "candles")
    # Volume module if volume present
    std <- .to_ohlc_standard(prep$mktdata)
    if (!is.null(std) && "Volume" %in% colnames(std)) mods <- c(mods, "volume")
  }
  # Position requires transactions
  if (!is.null(prep$trades) && .is_xts(prep$trades) && NROW(prep$trades) > 0) {
    mods <- c(mods, "position")
  }
  unique(mods)
}

#' Force an xts index to midnight in a target timezone without shifting dates
#' @param x xts object to reindex
#' @param tz_target target timezone label (default 'America/Sao_Paulo')
#' @param tz_source assumed source timezone for index when deriving the Date (default 'UTC')
#' @return xts with index set to 00:00 in tz_target for each original Date
#' @keywords internal
.force_midnight_tz <- function(x, tz_target = "America/Sao_Paulo", tz_source = "UTC"){
  if (!.is_xts(x)) return(x)
  idx <- index(x)
  # If already Date-based, just coerce to POSIXct midnight in target TZ
  if (inherits(idx, "Date")) {
    index(x) <- as.POSIXct(idx, tz = tz_target)
    attr(index(x), "tzone") <- tz_target
    return(x)
  }
  # Derive calendar date from the original index (interpreting in tz_source),
  # then assign midnight of that date in tz_target. No time shifting of data.
  idx_date <- tryCatch(as.Date(idx, tz = tz_source), error = function(e) as.Date(idx))
  index(x) <- as.POSIXct(idx_date, tz = tz_target)
  attr(index(x), "tzone") <- tz_target
  x
}

#' Fetch quantstrat/blotter portfolio returns and normalize index timezone
#' @param portfolio character portfolio name
#' @param init start date
#' @param finit end date
#' @param tz_target timezone to force on index midnight (default 'America/Sao_Paulo')
#' @return xts with a single column of returns, or NULL on failure
#' @keywords internal
.get_portfolio_returns <- function(portfolio, init, finit, tz_target = "America/Sao_Paulo"){
  if (is.null(portfolio) || !is.character(portfolio) || length(portfolio) != 1L) return(NULL)
  PortfReturns <- .get_function_if_exists("PortfReturns")
  if (is.null(PortfReturns)) return(NULL)
  rets <- tryCatch(PortfReturns(portfolio), error = function(e) NULL)
  if (!.is_xts(rets) || NROW(rets) == 0) return(NULL)
  rets <- .subset_xts(rets, init, finit)
  # Coerce to single-column numeric xts if needed
  if (NCOL(rets) > 1) {
    # Prefer a column named 'PortfReturns' or similar; otherwise take first
    cn <- colnames(rets)
    pick <- which(grepl("portf", tolower(cn)))
    if (length(pick) == 0) pick <- 1L
    rets <- rets[, pick, drop = FALSE]
  }
  # Force index to midnight in target TZ without shifting date labels
  rets <- .force_midnight_tz(rets, tz_target = tz_target, tz_source = "UTC")
  # Name the column as 'Discrete' so .data_prepare treats it as ready returns
  colnames(rets) <- "Discrete"
  rets
}

#' Ensure unique names in a named list by appending _2, _3, ... for duplicates
#' @param x a named list
#' @return same list with unique names
#' @keywords internal
.uniquify_names <- function(x){
  nms <- names(x)
  if (is.null(nms)) return(x)
  seen <- new.env(parent = emptyenv())
  new <- character(length(nms))
  for (i in seq_along(nms)) {
    base <- nms[i]
    if (!nzchar(base)) base <- paste0("Serie", i)
    cnt <- seen[[base]]
    if (is.null(cnt)) {
      seen[[base]] <- 1L
      new[i] <- base
    } else {
      cnt <- cnt + 1L
      seen[[base]] <- cnt
      new[i] <- paste0(base, "_", cnt)
    }
  }
  names(x) <- new
  x
}

#' Null-coalescing operator
#' @description Returns the left-hand side if it's not NULL,
#' otherwise returns the right-hand side.
#' @name null-coalesce
#' @aliases %||%
#' @param a The value to test.
#' @param b The fallback value if 'a' is NULL.
#' @return 'a' if it is not NULL, otherwise 'b'.
#' @keywords internal
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Checks if market data is for brazilian DI Futures
#' @param mkt The xts object with market data.
#' @return Logical.
#' @keywords internal
isDI <- function(mkt){
  if (is.null(mkt)) return(FALSE)
  cols <- tolower(colnames(mkt))
  all(c("pu_o", "tickvalue", "ticksize") %in% cols)
}

#' Calculates the rate from the PU of a brazilian DI futures contract
#' @param price The PU (unit price) of the contract.
#' @param row_date The date of the calculation.
#' @param maturity The maturity date of the contract.
#' @return The numeric rate.
#' @keywords internal
get_DI_price <- function(price, row_date, maturity){
  fn <- .get_function_if_exists("calcular_taxa_futuro")
  if (!is.null(fn)) {
    res <- tryCatch(fn(price, maturity, row_date), error = function(e) NULL)
    if (!is.null(res) && !is.null(res$taxa)) return(as.numeric(res$taxa))
  }
  # fallback: if helper not available, assume 'price' already is a numeric rate
  as.numeric(price)
}

#' Converts the statistics data.frame to a JSON-ready list
#' @param carteira_df The statistics data.frame.
#' @return A list ready to be converted to JSON.
#' @keywords internal
stats_json <- function(carteira_df){
  lapply(seq_len(nrow(carteira_df)), function(i){
    row <- as.list(carteira_df[i, , drop=FALSE])
    row <- lapply(row, function(x){
      if(is.character(x) && !is.na(num <- suppressWarnings(as.numeric(x)))){
        num
      } else x
    })
    names(row) <- colnames(carteira_df)
    row
  })
}

#' Converts a time series matrix to a JSON-ready list
#' @param mat The data matrix.
#' @param datas The vector of dates in milliseconds.
#' @return A list ready to be converted to JSON.
#' @keywords internal
series_json <- function(mat, datas){
  dates <- as.character(
    as.Date(as.POSIXct(datas/1000, origin="1970-01-01"))
  )
  df    <- data.frame(Date = dates, as.data.frame(mat, stringsAsFactors=FALSE),
                      check.names=FALSE)
  idx   <- floor(seq(1, nrow(df), length.out=11))
  lapply(idx, function(i){
    row <- as.list(df[i, , drop=FALSE])
    row <- lapply(row, function(x){
      if(is.character(x) && !is.na(num <- suppressWarnings(as.numeric(x)))){
        num
      } else x
    })
    row
  })
}

#' Converts the list of returns tables to a JSON-ready list
#' @param lista_tabs The list of returns tables.
#' @return A list ready to be converted to JSON.
#' @keywords internal
rentab_json <- function(lista_tabs){
  out <- list()
  for(name in names(lista_tabs)){
    tab <- lista_tabs[[name]]
    df2 <- cbind(Year = rownames(tab),
                 as.data.frame(tab, stringsAsFactors=FALSE))
    rows <- lapply(seq_len(nrow(df2)), function(i){
      r <- as.list(df2[i, , drop=FALSE])
      r <- lapply(r, function(x){
        if(is.character(x) && grepl("%", x)){
          as.numeric(gsub("%","", x))
        } else if(is.character(x) && !is.na(num<-suppressWarnings(as.numeric(x)))){
          num
        } else x
      })
      r
    })
    out[[name]] <- rows
  }
  out
}

#' Fixes HTML dependencies with symbolic package names
#' @param x The HTML dependency object or list.
#' @return The corrected object.
#' @keywords internal
fix_pkg <- function(x) {
  if (inherits(x, "html_dependency")) {
    if (is.symbol(x$package)) {
      x$package <- as.character(x$package)
    }
    return(x)
  }
  if (is.list(x)) {
    at <- attributes(x)
    x  <- lapply(x, fix_pkg)
    attributes(x) <- at
  }
  x
}

#' Prepares raw data by calculating returns
#' @param data_xts A list of xts objects.
#' @return A single xts object with the calculated returns for all series.
#' @keywords internal
.data_prepare <- function(data_xts, verbose = getOption("tplot.verbose", FALSE)) {
  vmsg <- function(...) { if (isTRUE(verbose)) message(...) }
  vmsg(sprintf("[.data_prepare] Input items: %d | names=%s",
               if (is.list(data_xts)) length(data_xts) else 1L,
               paste(if (is.list(data_xts)) names(data_xts) else "<single>", collapse=",")))
  if (xts::is.xts(data_xts)) data_xts <- list(data_xts)
  if (is.null(names(data_xts))) names(data_xts) <- paste0("Serie", seq_along(data_xts))

  # Accumulate raw selected price/level columns per series,
  # then merge once to avoid edge cases with cbind on empty xts
  ativos_data <- NULL
  cols_accum  <- list()
  col_names <- character(0)
  use_discrete <- logical(0)
  per_scales <- character(0)   # coarse scale from xts::periodicity
  per_labels <- character(0)   # human-friendly bar size (e.g., 1h, 4h, 1d)

  # Infer robust periodicity without calling xts::periodicity (avoid segfaults)
  .infer_period_info <- function(x_index) {
    # x_index can be an xts object or a POSIXct/Date vector
    idx <- x_index
    if (xts::is.xts(x_index)) idx <- index(x_index)
    # Default
    out <- list(scale = "daily", label = "1d", key = 3L)
    if (inherits(idx, "Date")) return(out)
    # Try POSIXct
    if (inherits(idx, "POSIXct") || inherits(idx, "POSIXt")) {
      secs <- suppressWarnings(as.numeric(idx))
      d <- suppressWarnings(stats::median(diff(secs), na.rm = TRUE))
      if (!is.finite(d) || is.na(d) || d <= 0) return(out)
      # classify by step (seconds)
      min_s <- 60
      hour_s <- 3600
      day_s <- 86400
      week_s <- day_s * 7
      month_s <- day_s * 30
      quarter_s <- day_s * 90
      if (d < hour_s) {
        m <- max(1L, as.integer(round(d / min_s)))
        return(list(scale = "minute", label = paste0(m, "m"), key = 1L))
      } else if (d < day_s) {
        h <- max(1L, as.integer(round(d / hour_s)))
        return(list(scale = "hourly", label = paste0(h, "h"), key = 2L))
      } else if (d < week_s) {
        return(list(scale = "daily", label = "1d", key = 3L))
      } else if (d < month_s) {
        return(list(scale = "weekly", label = "1w", key = 4L))
      } else if (d < quarter_s) {
        return(list(scale = "monthly", label = "1mo", key = 5L))
      } else if (d < 365 * day_s) {
        return(list(scale = "quarterly", label = "1q", key = 6L))
      } else {
        return(list(scale = "yearly", label = "1y", key = 7L))
      }
    }
    out
  }

  find_col_idx <- function(cols, base_name) {
    idx <- grep(paste0("(^|\\.)", base_name, "$"), cols, ignore.case = TRUE)
    if (length(idx) == 0) NA_integer_ else idx[1]
  }

  for (i in seq_along(data_xts)) {
    item <- data_xts[[i]]
    item_name <- names(data_xts)[i]
    # baseline message (always): which item is being prepared
    message(sprintf("[.data_prepare] %s", item_name))

    if (is.null(colnames(item)) || length(colnames(item)) == 0) {
      warning(paste("Rowless object for", item_name))
      next
    }

    cols <- colnames(item)
    idx_close    <- find_col_idx(cols, "Close")
    idx_adjusted <- find_col_idx(cols, "Adjusted")
    idx_discrete <- find_col_idx(cols, "Discrete")

    chosen_idx <- NA_integer_
    chosen_msg <- ""
    chosen_is_discrete <- FALSE

    if (grepl("CDI", item_name, ignore.case = TRUE) && !is.na(idx_close)) {
      chosen_idx <- idx_close;    chosen_msg <- "\n        Close (calculated)\n"
    } else if (grepl("IPCA", item_name, ignore.case = TRUE) && !is.na(idx_close)) {
      chosen_idx <- idx_close;    chosen_msg <- "\n        Close (calculated)\n"
    } else if (!is.na(idx_adjusted)) {
      chosen_idx <- idx_adjusted; chosen_msg <- "\n        Adjusted\n"
    } else if (!is.na(idx_close)) {
      chosen_idx <- idx_close;    chosen_msg <- "\n        Close\n"
    } else if (!is.na(idx_discrete)) {
      chosen_idx <- idx_discrete; chosen_msg <- "\n        Discrete (no recalc)\n"; chosen_is_discrete <- TRUE
    } else if (NCOL(item) == 1) {
      # Fallback: use the single available column as price/level
      chosen_idx <- 1L; chosen_msg <- "\n        (fallback) first row\n"
    } else {
      warning(paste("No adequate row found for", item_name))
      next
    }

    if (nzchar(chosen_msg)) message(gsub("\n", " ", trimws(chosen_msg)))
    discrete_col <- item[, chosen_idx, drop = FALSE]
    # Infer periodicity robustly (avoid xts::periodicity)
    inf <- .infer_period_info(discrete_col)
    per <- inf$scale
    bar_lbl <- inf$label

    vmsg(sprintf("[.data_prepare] Added column for %s | rows=%d | periodicity=%s | label=%s",
                 item_name, NROW(discrete_col), per, bar_lbl))
    # Ensure the selected column carries the series name now
    colnames(discrete_col) <- item_name
    cols_accum[[length(cols_accum) + 1L]] <- discrete_col
    col_names    <- c(col_names, item_name)
    use_discrete <- c(use_discrete, chosen_is_discrete)
    per_scales   <- c(per_scales, per)
    per_labels   <- c(per_labels, bar_lbl)
  }

  # Merge accumulated columns (outer join) to create a unified xts
  if (length(cols_accum) > 0) {
    if (length(cols_accum) == 1L) {
      ativos_data <- cols_accum[[1L]]
    } else {
      ativos_data <- Reduce(function(a, b) merge(a, b, join = "outer"), cols_accum)
    }
  } else {
    ativos_data <- xts::xts()
  }

  if (NCOL(ativos_data) == 0L || NROW(ativos_data) == 0L) {
    warning("No valid series was added.")
    return(xts::xts())
  }

  # Do not force full-row overlap at raw level frequency across all series.
  # Series can start on different dates and have different timestamps; we will
  # align after computing per-series returns and aggregating to a common scale.

  for (i in seq_len(NCOL(ativos_data))) {
    if (use_discrete[i]) {
      first_non_na <- which(!is.na(ativos_data[, i]))[1]
      if (!is.na(first_non_na)) {
        ativos_data[first_non_na:NROW(ativos_data), i] <-
          ifelse(is.na(ativos_data[first_non_na:NROW(ativos_data), i]),
                 0, ativos_data[first_non_na:NROW(ativos_data), i])
      }
    } else {
      ativos_data[, i] <- zoo::na.locf(ativos_data[, i], na.rm = FALSE)
      ativos_data[, i] <- zoo::na.approx(ativos_data[, i], na.rm = FALSE)
    }
  }

  ativos_data_returns <- ativos_data
  for (i in seq_len(NCOL(ativos_data))) {
    if (!use_discrete[i]) {
      r_i <- PerformanceAnalytics::Return.calculate(ativos_data[, i], method = "discrete")
      # Set the very first non-NA return for this column to 0 (do NOT touch global first row)
      if (NROW(r_i) > 0) {
        fi <- which(!is.na(r_i[, 1]))
        if (length(fi)) r_i[fi[1], 1] <- 0
      }
      ativos_data_returns[, i] <- r_i
      vmsg(sprintf("[.data_prepare] Return.calculate(%s) -> rows=%d",
                   col_names[i], NROW(r_i)))
    } else {
      # For already discrete returns columns, ensure first valid is 0
      r_i <- ativos_data[, i, drop = FALSE]
      fi <- which(!is.na(r_i[, 1]))
      if (length(fi)) r_i[fi[1], 1] <- 0
      ativos_data_returns[, i] <- r_i
    }
  }

  # ---- Align returns to a common, safe periodicity ----
  # Map periodicity to an ordered rank and endpoint label
  .per_key <- function(scale) {
    s <- tolower(scale %||% "unknown")
    if (startsWith(s, "min")) return(1L)      # minute
    if (s %in% c("hourly","hours","hour")) return(2L)
    if (s %in% c("daily","day","days"))    return(3L)
    if (s %in% c("weekly","week","weeks")) return(4L)
    if (s %in% c("monthly","month","months")) return(5L)
    if (s %in% c("quarterly","quarter","quarters")) return(6L)
    if (s %in% c("yearly","annual","year","years")) return(7L)
    3L # default to daily
  }
  .on_from_key <- function(k) {
    switch(as.character(k),
           `1` = "minutes",
           `2` = "hours",
           `3` = "days",
           `4` = "weeks",
           `5` = "months",
           `6` = "quarters",
           `7` = "years",
           "days")
  }
  .pretty_scale <- function(on) {
    switch(on,
           minutes = "Minutos",
           hours   = "Horas",
           days    = "Diário",
           weeks   = "Semanal",
           months  = "Mensal",
           quarters= "Trimestral",
           years   = "Anual",
           on)
  }
  .normalize_index <- function(x, on) {
    # Normalize index to comparable Date stamps across different sources/markets.
    # This avoids mismatches due to different times-of-day when merging.
    if (on %in% c("days","day"))   return(as.Date(x))
    if (on %in% c("weeks","week")) return(as.Date(x))
    if (on %in% c("months","month")) return(as.Date(zoo::as.yearmon(x), frac = 1))
    if (on %in% c("quarters","quarter")) return(as.Date(zoo::as.yearqtr(x), frac = 1))
    if (on %in% c("years","year")) {
      yy <- format(as.POSIXct(x), "%Y")
      return(as.Date(zoo::as.yearmon(paste0(yy, "-12")), frac = 1))
    }
    as.Date(x)
  }

  .agg_returns <- function(r, on) {
    # r is an xts vector of returns; aggregate by compounding within each period
    if (is.null(r) || NROW(r) == 0) return(r)
    idx <- tryCatch(xts::endpoints(r, on = on), error = function(e) integer(0))
    if (length(idx) <= 1) return(r)
    out_vals <- vector("numeric", length(idx) - 1L)
    out_idx  <- index(r)[idx[-1]]
    for (j in seq_len(length(idx) - 1L)) {
      seg <- r[(idx[j] + 1L):idx[j + 1L], , drop = FALSE]
      rr  <- as.numeric(seg)
      rr  <- rr[is.finite(rr)]
      out_vals[j] <- if (length(rr)) exp(sum(log1p(rr))) - 1 else NA_real_
    }
    out_idx_n <- .normalize_index(out_idx, on)
    xts::xts(out_vals, order.by = out_idx_n)
  }

  # Determine the target (coarsest) periodicity among series; ensure at least 'days'
  keys <- vapply(per_scales, .per_key, integer(1))
  # coarsest = max(key); enforce minimum = 3 (days)
  target_key <- max(c(3L, keys), na.rm = TRUE)
  target_on  <- .on_from_key(target_key)

  # Build user-friendly message if alignment is needed or if intraday found
  det <- paste(sprintf("%s: %s", col_names, per_labels), collapse = ", ")
  if (any(keys < 3L)) {
    message(sprintf("[.data_prepare] Detectei série(s) intradiárias (%s). Convertendo para escala %s para compatibilidade de métricas.",
                   paste(col_names[keys < 3L], collapse = ", "), .pretty_scale("days")))
  }
  if (any(keys != target_key)) {
    message(sprintf("[.data_prepare] Alinhando periodicidade dos retornos -> %s | Detectadas: %s",
                   .pretty_scale(target_on), det))
  }

  # If alignment required, aggregate every column to target_on
  if (any(keys != target_key) || any(keys < 3L)) {
    agg_list <- list()
    for (i in seq_len(NCOL(ativos_data_returns))) {
      agg_list[[col_names[i]]] <- .agg_returns(ativos_data_returns[, i, drop = FALSE], target_on)
    }
    # Merge with inner join to keep common dates only
    if (length(agg_list) == 1L) {
      ativos_data_returns <- agg_list[[1]]
    } else {
      ativos_data_returns <- Reduce(function(a, b) merge(a, b, join = "inner"), agg_list)
    }
    colnames(ativos_data_returns) <- names(agg_list)
    # After aggregation, ensure first valid of each column is 0 again (fresh index)
    for (j in seq_len(NCOL(ativos_data_returns))) {
      fi <- which(!is.na(ativos_data_returns[, j]))
      if (length(fi)) ativos_data_returns[fi[1], j] <- 0
    }
    vmsg(sprintf("[.data_prepare] Aggregated to %s | rows=%d | cols=%d",
                 target_on, NROW(ativos_data_returns), NCOL(ativos_data_returns)))
  }

  # Ensure column names exist and match column count
  target_nms <- if (length(col_names) >= NCOL(ativos_data_returns)) col_names[seq_len(NCOL(ativos_data_returns))]
                else paste0("Serie", seq_len(NCOL(ativos_data_returns)))
  colnames(ativos_data_returns) <- target_nms
  vmsg(sprintf("[.data_prepare] Final prepared | rows=%d | cols=%d | names=%s",
               NROW(ativos_data_returns), NCOL(ativos_data_returns), paste(target_nms, collapse=",")))
  # Final safety: ensure xts type
  if (!xts::is.xts(ativos_data_returns) && inherits(ativos_data_returns, "zoo")) {
    ativos_data_returns <- xts::as.xts(ativos_data_returns)
  }
  vmsg(sprintf("[.data_prepare] Returning class=%s | rows=%d | cols=%d",
               paste(class(ativos_data_returns), collapse=","),
               NROW(ativos_data_returns), NCOL(ativos_data_returns)))
  return(ativos_data_returns)
}

#' Prepares all data and metrics for tplot rendering
#' @param ativo The main asset (name or xts object).
#' @param benchs A list of benchmarks.
#' @param init Start date.
#' @param finit End date.
#' @param rf_rate The risk-free rate.
#' @param auto_rets Logical, for geometric returns.
#' @param ativo_name An explicit name for the main asset.
#' @return A large list containing all data and metrics prepared for the modules.
#' @keywords internal
.tplot_prepare <- function(ativo, benchs, init, finit, rf_rate, auto_rets = FALSE,
                           ativo_name = NULL,
                           verbose = getOption("tplot.verbose", FALSE)) {
  vmsg <- function(...) { if (isTRUE(verbose)) message(...) }
  # 1) Guarantee Date inputs
  init  <- as.Date(init)
  finit <- as.Date(finit)

  # 2) Prefer quantstrat/blotter portfolio data by name (portfolio-first lookup)
  mktdata <- NULL; trades <- NULL; stats <- NULL
  pf_name <- NULL; port_rets <- NULL
  # Optional hint provided by wrapper to select a specific portfolio/symbol
  hint <- getOption("tplot.portfolio_hint", NULL)
  if (is.list(hint) && is.character(hint$name)) {
    qs <- .quantstrat_portfolio_data(hint$name, init, finit)
    if (is.list(qs)) {
      mktdata <- qs$mktdata %||% NULL
      trades  <- qs$trades  %||% NULL
      chosen  <- qs$symbol %||% NULL
      pf_name <- hint$name
      # If user requested a different symbol explicitly, override
      if (!is.null(hint$symbol) && is.character(hint$symbol) && length(hint$symbol) == 1) {
        getTxns <- .get_function_if_exists("getTxns")
        if (!is.null(getTxns)) {
          trades2 <- tryCatch(getTxns(Portfolio = hint$name, Symbol = hint$symbol), error = function(e) NULL)
          if (.is_xts(trades2)) trades <- .subset_xts(trades2, init, finit)
        }
        md2 <- .get_object_if_exists(hint$symbol)
        if (.is_xts(md2)) mktdata <- .subset_xts(md2, init, finit)
        if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
          # fallback to API then Yahoo
          od <- .get_function_if_exists("sm_get_data")
          if (!is.null(od)) {
            fetched <- tryCatch(od(hint$symbol, start_date = init, end_date = finit, auto_returns = FALSE), error = function(e) NULL)
            if (.is_xts(fetched)) mktdata <- .subset_xts(fetched, init, finit)
            else if (is.list(fetched) && length(fetched) > 0 && .is_xts(fetched[[1]])) mktdata <- .subset_xts(fetched[[1]], init, finit)
          }
          if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
            obj <- tryCatch(suppressWarnings(quantmod::getSymbols(hint$symbol, from = init, to = finit, auto.assign = FALSE)), error=function(e) NULL)
            if (.is_xts(obj)) mktdata <- .subset_xts(obj, init, finit)
          }
        }
        chosen <- hint$symbol
      }
      if (!is.null(chosen)) {
        if (is.null(ativo_name) || identical(ativo_name, hint$name)) {
          ativo_name <- chosen
        }
      }
    }
  } else if (is.character(ativo) && length(ativo) == 1) {
    qs <- .quantstrat_portfolio_data(ativo, init, finit)
    if (is.list(qs)) {
      mktdata <- qs$mktdata %||% NULL
      trades  <- qs$trades  %||% NULL
      pf_name <- ativo
      # If a portfolio was detected, prefer using the resolved symbol as the label
      if (!is.null(qs$symbol)) {
        if (is.null(ativo_name) || identical(ativo_name, ativo)) {
          ativo_name <- qs$symbol
        }
      }
    }
  }
  # Try to get backtest returns when a portfolio name is known
  if (!is.null(pf_name)) {
    port_rets <- .get_portfolio_returns(pf_name, init, finit, tz_target = "America/Sao_Paulo")
    if (.is_xts(port_rets) && NROW(port_rets) > 0) {
      vmsg(sprintf("[.tplot_prepare] Loaded PortfReturns for '%s' | rows=%d", pf_name, NROW(port_rets)))
    }
  }
  # If no quantstrat portfolio detected, try a generic backtest object in the env
  if (is.null(mktdata) && is.null(trades)) {
    bt_env <- .as_backtest(if (!is.character(ativo)) ativo else .get_object_if_exists(ativo))
    if (!is.null(bt_env)) {
      mktdata <- bt_env$mktdata %||% NULL
      trades  <- bt_env$trades  %||% NULL
      stats   <- bt_env$stats   %||% NULL
    }
  }

  # 3) Resolve series map for returns (env-first), with portfolio/backtest awareness
  series_map   <- list()
  requested    <- character(0)
  # ativo can be xts or string name
  if (.is_xts(ativo) || is.matrix(ativo) || is.data.frame(ativo) || inherits(ativo, "zoo")) {
    at_xts <- .to_xts(ativo)
    if (!.is_xts(at_xts)) stop("Could not convert ticker to xts.")
    if (is.null(ativo_name)) ativo_name <- "Ativo"
    series_map[[ativo_name]] <- .subset_xts(at_xts, init, finit)
  } else if (is.character(ativo) && length(ativo) == 1) {
    if (is.null(ativo_name)) ativo_name <- ativo
    if (!is.null(port_rets) && .is_xts(port_rets) && NROW(port_rets) > 0) {
      # Prefer backtest portfolio returns (already discrete)
      series_map[[ativo_name]] <- port_rets
    } else if (!is.null(mktdata) && .is_xts(mktdata)) {
      # Fallback: use price series directly
      series_map[[ativo_name]] <- .subset_xts(mktdata, init, finit)
    } else {
      requested <- c(requested, ativo)
    }
  } else if (!is.null(mktdata) && .is_xts(mktdata)) {
    # If we detected a backtest with mktdata but 'ativo' isn't an xts/character,
    # derive a synthetic series for returns from mktdata (Adjusted/Close fallback).
    # Use the provided label or a default one.
    if (is.null(ativo_name)) ativo_name <- "Asset"
    series_map[[ativo_name]] <- .subset_xts(mktdata, init, finit)
  } else {
    stop("Ticker needs to be a name (string) or xts object.")
  }
  # benchs can be NULL or character vector; (optional) accept list of xts
  benchs_names <- character(0)
  if (!is.null(benchs)) {
    if (is.character(benchs)) {
      benchs_names <- benchs
      requested <- c(requested, benchs)
    } else if (.is_xts(benchs) || is.matrix(benchs) || is.data.frame(benchs) || inherits(benchs, "zoo")) {
      # Treat single xts/data.frame object as ONE benchmark asset
      b_xts <- .to_xts(benchs)
      b_lab <- as.character(substitute(benchs))
      if (is.null(b_lab) || b_lab == "") b_lab <- "Bench"
      series_map[[b_lab]] <- .subset_xts(b_xts, init, finit)
      benchs_names <- c(benchs_names, b_lab)
    } else if (is.list(benchs)) {
      # list of xts series
      for (i in seq_along(benchs)) {
        bi <- benchs[[i]]
        nm <- names(benchs)[i]
        if (.is_xts(bi) || is.matrix(bi) || is.data.frame(bi) || inherits(bi, "zoo")) {
          conv <- .to_xts(bi)
          if (.is_xts(conv)) {
            if (is.null(nm) || nm == "") nm <- paste0("Bench", i)
            series_map[[nm]] <- .subset_xts(conv, init, finit)
            benchs_names <- c(benchs_names, nm)
          }
        } else if (is.character(bi) && length(bi) == 1) {
          requested <- c(requested, bi)
          benchs_names <- c(benchs_names, bi)
        }
      }
    } else {
      warning("Unrecognized format found, ignoring.")
    }
  }
  # rf rate optional
  rf_name <- NULL
  if (!is.null(rf_rate)) {
    if (.is_xts(rf_rate)) {
      rf_name <- "RF"
      series_map[[rf_name]] <- .subset_xts(rf_rate, init, finit)
    } else if (is.character(rf_rate) && length(rf_rate) == 1) {
      rf_name <- rf_rate
      requested <- c(requested, rf_rate)
    }
  }

  # Build unique label->symbol map for requested to preserve duplicates
  if (length(requested) > 0) {
    seen_req <- new.env(parent = emptyenv())
    lbls <- character(length(requested))
    for (i in seq_along(requested)) {
      sym <- requested[i]
      cnt <- seen_req[[sym]]
      if (is.null(cnt)) { seen_req[[sym]] <- 1L; lbls[i] <- sym } else { cnt <- cnt + 1L; seen_req[[sym]] <- cnt; lbls[i] <- paste0(sym, "_", cnt) }
    }
    req_map <- requested
    names(req_map) <- lbls
  } else {
    req_map <- requested
  }
  env_series <- .find_series_in_env(req_map, init, finit)
  # 4) sm_get_data fallback only for missing labels (if any) and if available; then try quantmod
  od <- .get_function_if_exists("sm_get_data")
  dados_raw <- c(series_map, env_series)
  missing_labels <- setdiff(names(req_map), names(env_series))
  if (length(missing_labels) > 0) {
    if (!is.null(od)) {
      message("Getting data with sm_get_data() for: ", paste(unname(req_map[missing_labels]), collapse = ", "))
      for (lab in missing_labels) {
        sym <- unname(req_map[lab])
        fetched_one <- tryCatch(od(sym,
                                   start_date   = init,
                                   end_date     = finit,
                                   auto_returns = auto_rets),
                                error = function(e) NULL)
        if (.is_xts(fetched_one)) {
          dados_raw[[lab]] <- .subset_xts(fetched_one, init, finit)
        } else if (is.list(fetched_one) && length(fetched_one) > 0) {
          pick <- NULL
          if (!is.null(names(fetched_one)) && sym %in% names(fetched_one)) pick <- fetched_one[[sym]]
          if (is.null(pick)) { for (el in fetched_one) { if (.is_xts(el)) { pick <- el; break } } }
          if (.is_xts(pick)) dados_raw[[lab]] <- .subset_xts(pick, init, finit)
        }
      }
    }
    # After sm_get_data, still missing? try quantmod
    still_missing <- setdiff(names(req_map), names(dados_raw))
    if (length(still_missing) > 0) {
      for (lab in still_missing) {
        sym <- unname(req_map[lab])
        obj <- tryCatch(
          suppressWarnings(quantmod::getSymbols(sym, from = init, to = finit, auto.assign = FALSE)),
          error = function(e) NULL
        )
        if (.is_xts(obj)) dados_raw[[lab]] <- .subset_xts(obj, init, finit)
      }
    }
  }
  # After fetching all sources, enforce unique names with _2, _3 suffixes for duplicates
  dados_raw <- .uniquify_names(dados_raw)
  if (length(dados_raw) == 0) {
    stop("Could not find data in the envir and could not find data with 'sm_get_data' or 'getSymbols()'.")
  }
  # Inform user about any symbols that could not be fetched
  have_syms <- names(dados_raw)
  if (!is.null(have_syms)) {
    ref_map <- if (exists("req_map")) req_map else setNames(requested, requested)
    missed <- setdiff(names(ref_map), have_syms)
    if (length(missed) > 0) {
      message("[.tplot_prepare] Could not fetch data for: ", paste(unname(ref_map[missed]), collapse = ", "))
    }
  }
  vmsg("[.tplot_prepare] Prepared raw series: ", paste(names(dados_raw), collapse = ", "))

  # 5) If sm_get_data tagged as backtest, try to pick extra pieces from env
  if (is.list(dados_raw) && length(dados_raw) > 0) {
    if (isTRUE(attr(dados_raw[[1]], "backtest"))) {
      message("Processing Backtest")
      # prefer previously discovered backtest pieces; otherwise try global
      if (is.null(mktdata) || is.null(trades)) {
        bt_guess <- .as_backtest(.get_object_if_exists(ativo))
        if (!is.null(bt_guess)) {
          mktdata <- bt_guess$mktdata
          trades  <- bt_guess$trades
          stats   <- bt_guess$stats %||% stats
        }
      }
      # legacy behavior removed forced dependency on global get(ativo)
    }
  }

  # 5.1) Choose an OHLC series for the candles module when possible.
  # Preference order: backtest mktdata (if valid OHLC), then first OHLC among
  # 'ativo' and 'benchs' (in the order they were provided).
  if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
    choose_first_ohlc <- function(nm) {
      if (is.null(nm)) return(NULL)
      x <- dados_raw[[nm]]
      if (.is_xts(x) && .is_ohlc_xts(x)) return(.subset_xts(x, init, finit))
      NULL
    }
    # Try the main asset name if it's a string
    if (is.character(ativo) && length(ativo) == 1) {
      cand <- choose_first_ohlc(ativo)
      if (!is.null(cand)) mktdata <- cand
    } else if (.is_xts(ativo) && .is_ohlc_xts(ativo)) {
      mktdata <- .subset_xts(.to_xts(ativo), init, finit)
    }
    # If still not found, try each benchmark (character names only, in order)
    if (is.null(mktdata) && !is.null(benchs)) {
      bench_names <- character(0)
      if (is.character(benchs)) bench_names <- benchs
      if (is.list(benchs) && length(benchs) > 0) {
        for (i in seq_along(benchs)) {
          if (is.character(benchs[[i]]) && length(benchs[[i]]) == 1) bench_names <- c(bench_names, benchs[[i]])
        }
      }
      if (length(bench_names)) {
        for (nm in bench_names) {
          cand <- choose_first_ohlc(nm)
          if (!is.null(cand)) { mktdata <- cand; break }
        }
      }
    }
  }

  # 6) Normalize/prepare returns from whatever we have
  dados_trat <- .data_prepare(dados_raw, verbose = verbose)
  vmsg(sprintf("[.tplot_prepare] .data_prepare class=%s | rows=%s | cols=%s",
               paste(class(dados_trat), collapse=","),
               tryCatch(NROW(dados_trat), error=function(e) "?"),
               tryCatch(NCOL(dados_trat), error=function(e) "?")))
  # Guarantee xts and column names for downstream operations
  if (!.is_xts(dados_trat)) {
    vmsg("[.tplot_prepare] .data_prepare did not return xts; attempting coercion")
    try_coerce <- try(.to_xts(dados_trat), silent = TRUE)
    if (.is_xts(try_coerce)) {
      dados_trat <- try_coerce
    } else {
      # Fallback: prepare returns in a minimal, robust way
      vmsg("[.tplot_prepare] Fallback path: building returns from raw series")
      build_ret <- function(x) {
        xx <- .to_xts(x)
        if (!.is_xts(xx)) return(NULL)
        # pick Adjusted/Close/first
        cn <- colnames(xx)
        pick <- if (length(cn)) {
          idxA <- grep("(^|\\.)Adjusted$", cn, ignore.case = TRUE)
          idxC <- grep("(^|\\.)Close$", cn, ignore.case = TRUE)
          if (length(idxA)) idxA[1] else if (length(idxC)) idxC[1] else 1L
        } else 1L
        px <- xx[, pick, drop = FALSE]
        ret <- try(PerformanceAnalytics::Return.calculate(px, method = "discrete"), silent = TRUE)
        if (inherits(ret, "try-error")) return(NULL)
        ret[1, ] <- 0
        ret
      }
      rets <- list()
      for (nm in names(dados_raw)) {
        rr <- build_ret(dados_raw[[nm]])
        if (.is_xts(rr)) { colnames(rr) <- nm; rets[[nm]] <- rr }
      }
      if (length(rets) == 0) stop("Prepared data is not an xts time series.")
      merged <- Reduce(function(a, b) merge(a, b, join = "inner"), rets)
      # Aggregate to daily by compounding within each day (safe baseline)
      ep <- tryCatch(xts::endpoints(merged, on = "days"), error = function(e) integer(0))
      if (length(ep) > 1) {
        agg <- lapply(seq_len(ncol(merged)), function(j){
          vals <- merged[, j, drop = FALSE]
          out  <- vector("numeric", length(ep) - 1L)
          for (k in seq_len(length(ep) - 1L)) {
            seg <- vals[(ep[k] + 1L):ep[k + 1L], , drop = FALSE]
            rr  <- as.numeric(seg)
            rr  <- rr[is.finite(rr)]
            out[k] <- if (length(rr)) exp(sum(log1p(rr))) - 1 else NA_real_
          }
          xts::xts(out, order.by = as.Date(index(merged)[ep[-1]]))
        })
        dados_trat <- Reduce(function(a, b) merge(a, b, join = "inner"), agg)
        colnames(dados_trat) <- names(rets)
      } else {
        dados_trat <- merged
      }
    }
  }
  if (is.null(colnames(dados_trat)) || length(colnames(dados_trat)) == 0) {
    # try to adopt names from dados_raw or fallback generic
    base_nms <- names(dados_raw)
    if (is.null(base_nms) || length(base_nms) == 0)
      base_nms <- paste0("Serie", seq_len(NCOL(dados_trat)))
    colnames(dados_trat) <- base_nms[seq_len(NCOL(dados_trat))]
  }
  if (NCOL(dados_trat) == 0 || NROW(dados_trat) == 0) {
    stop("Data series could no be prepared (no valid data).")
  }

  # 7) Build carteira (ativo + optional benchs) preserving insertion order
  # Prefer the prepared list order: first is the 'ativo', remainder are benchmarks.
  # Exclude risk-free (rf_name) if it exists.
  prepared_order <- names(dados_raw)
  if (!is.null(rf_name)) {
    rf_regex <- paste0("^", rf_name, "(_[0-9]+)?$")
    prepared_order <- prepared_order[!grepl(rf_regex, prepared_order)]
  }
  # Ensure these columns exist in dados_trat and preserve order
  keep_cols <- intersect(prepared_order, colnames(dados_trat))
  if (length(keep_cols) == 0) {
    message("[.tplot_prepare] No matching prepared columns found; using all prepared series order.")
    keep_cols <- colnames(dados_trat)
  }
  # Update ativo/benchs labels according to unique prepared names
  ativo_name <- keep_cols[1]
  benchs_labels <- if (length(keep_cols) > 1) keep_cols[-1] else character(0)
  # Build two views: full (outer-joined) and common (intersection of all series)
  carteira_full  <- dados_trat[, keep_cols, drop = FALSE]
  # Keep only rows where all selected series have data for fair charting/metrics
  complete_rows  <- apply(!is.na(carteira_full), 1, all)
  if (!any(complete_rows)) {
    message("[.tplot_prepare] No overlapping period across selected series; using last non-NA overlaps if available.")
    carteira <- na.omit(carteira_full)
  } else {
    carteira <- carteira_full[complete_rows, , drop = FALSE]
  }

  # 8) Risk-free handling (optional; default to 0 if not available)
  if (!is.null(rf_rate) && rf_rate %in% colnames(dados_trat)) {
    rf_final <- as.numeric(
      Return.annualized(dados_trat[, rf_rate], geometric = auto_rets)[1, 1]
    )
  } else {
    if (!is.null(rf_rate)) message("Risk free rate of '", rf_rate, "' not found. Using RF = 0.")
    rf_final <- 0
  }

  # 9) Metrics
  car_anu    <- Return.annualized(carteira, geometric = auto_rets)
  car_tot    <- Return.cumulative(carteira, geometric = auto_rets)
  car_dd     <- maxDrawdown(carteira)
  # Annualize SD using detected periodicity of 'carteira'
  # Infer return frequency without xts::periodicity to avoid segfaults
  infer_sd_scale <- function(x) {
    idx <- index(x)
    if (inherits(idx, "Date")) return("daily")
    if (inherits(idx, "POSIXct") || inherits(idx, "POSIXt")) {
      d <- suppressWarnings(stats::median(diff(as.numeric(idx)), na.rm = TRUE))
      day_s <- 86400; week_s <- day_s * 7; month_s <- day_s * 30; quarter_s <- day_s * 90
      if (!is.finite(d) || is.na(d) || d <= 0) return("daily")
      if (d < week_s) return("daily")
      if (d < month_s) return("weekly")
      if (d < quarter_s) return("monthly")
      return("quarterly")
    }
    # fallback
    "daily"
  }
  per_card   <- infer_sd_scale(carteira)
  scale_fac  <- switch(tolower(per_card),
                       "daily"   = 252,
                       "day"     = 252,
                       "weekly"  = 52,
                       "week"    = 52,
                       "monthly" = 12,
                       "month"   = 12,
                       "quarterly"= 4,
                       "quarter" = 4,
                       "yearly"  = 1,
                       "annual"  = 1,
                       252)
  car_sd     <- apply(na.omit(carteira), 2, sd) * sqrt(scale_fac)
  car_sharpe <- (car_anu - rf_final) / car_sd
  car_sort   <- SortinoRatio(carteira)
  car_sort[is.infinite(car_sort)] <- 0

  carteira_df <- data.frame(
    Ativos  = colnames(carteira),
    Total   = round(as.vector(car_tot * 100), 3),
    CAR     = round(as.vector(car_anu * 100), 2),
    MaxDD   = round(as.vector(car_dd * 100), 2),
    Std_Dev = round(as.vector(car_sd * 100), 3),
    Sharpe  = round(as.vector(car_sharpe), 2),
    Sortino = round(as.vector(car_sort), 3),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  if ("IPCA" %in% carteira_df$Ativos) {
    ip <- which(carteira_df$Ativos == "IPCA")
    carteira_df[ip, c("MaxDD", "Std_Dev", "Sharpe", "Sortino")] <- "-"
  }

  # 10) Derived series for plots
  if (isFALSE(auto_rets)) {
    ret_cum <- cumsum(carteira) * 100
  } else {
    ret_cum <- (cumprod(1 + carteira) - 1) * 100
  }
  ret_sim <- carteira * 100
  dds     <- Drawdowns(carteira) * 100
  datas   <- as.numeric(as.POSIXct(index(ret_cum))) * 1000

  # Build monthly/annual tables per series using each one's full history
  lista_tabs <- lapply(colnames(carteira_full), function(x) {
    serie_x <- carteira_full[, x, drop = FALSE]
    serie_x <- na.omit(serie_x)
    if (NROW(serie_x) < 1) return(data.frame())
    rentab_table_calc(serie_x, retornar = TRUE, geometric = auto_rets)
  })
  names(lista_tabs) <- colnames(carteira_full)

  resultado <- list(
    ativo       = ativo_name,
    benchs      = benchs_labels,
    init_date   = first(index(ret_cum)),
    finit_date  = last(index(ret_cum)),
    carteira    = carteira,
    carteira_df = carteira_df,
    ret_cum     = ret_cum,
    ret_sim     = ret_sim,
    dds         = dds,
    datas       = datas,
    lista_tabs  = lista_tabs
  )

  # 11) Conditionally attach backtest extras
  if (!is.null(mktdata)) resultado$mktdata <- mktdata
  if (!is.null(trades))  resultado$trades  <- if (NROW(trades) > 1) trades[-1] else trades
  if (!is.null(stats))   resultado$stats   <- stats

  resultado
}

#' Renders the tplot output in JSON format
#' @param prep The list of data prepared by .tplot_prepare.
#' @param modules The modules to include.
#' @return A JSON string.
#' @keywords internal
.tplot_render_json <- function(prep, modules) {
  out <- list()
  if("stats"      %in% modules) out$stats      <- stats_json(prep$carteira_df)
  if("cumulative" %in% modules) out$cumulative <- series_json(prep$ret_cum, prep$datas)
  if("period"     %in% modules) out$period     <- series_json(prep$ret_sim, prep$datas)
  if("drawdowns"  %in% modules) out$drawdowns  <- series_json(prep$dds,     prep$datas)
  if("table"      %in% modules) out$table      <- rentab_json(prep$lista_tabs)
  jsonlite::toJSON(out, auto_unbox=TRUE, pretty=TRUE, na="null")
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
    tags$meta(charset="UTF-8"),
    tags$meta(name="viewport",
              content="width=device-width, initial-scale=1"),
    tags$title(sprintf("tplot_%s_%s",
                       prep$ativo,
                       format(Sys.time(), "%Y%m%d_%H%M%S"))),
    tags$style(HTML(sprintf(
      "html,body{margin:0;padding:0;background:%s;}
       #tplot-container{margin:0 auto;padding:10px;box-sizing:border-box;width:100%%;}
       .module{margin-bottom:20px;}",
      theme$colors$page_bg
    )))
  )

  sync_with_candles <- "candles" %in% modules

  data_inicial <- format(prep$init_date, "%d-%m-%Y")
  data_final   <- format(prep$finit_date, "%d-%m-%Y")
  linha_datas  <- tags$div(
    style = sprintf(
      "text-align:right;font-family:%s;font-size:12px;font-weight:bold;
       margin:30px 30px -10px 50px;color:%s;",
      theme$font_family, theme$colors$page_txt
    ),
    # paste(data_inicial, "a", data_final)
  )
  page <- tagList()
  link_flag <- all(c("cumulative","rolling","period","drawdowns") %in% modules)

  for (mod in modules) {
    ui <- switch(mod,
                 stats      = tagList(stats_module(prep$carteira_df,
                                                   prep$ativo,
                                                   prep$benchs,
                                                   theme),
                                      linha_datas),
                 candles    = candles_module(prep$mktdata,
                                             prep$trades,
                                             theme),
                 volume     = volume_module(prep$mktdata,
                                            theme),
                 position   = position_module(prep$mktdata,
                                              prep$trades,
                                              theme,
                                              sync_with_candles),
                 cumulative = cumret_module(prep$ret_cum,
                                            prep$datas,
                                            prep$ativo,
                                            prep$benchs,
                                            theme,
                                            link_flag,
                                            sync_with_candles),
                 rolling = rollingret_module(prep$ret_cum,
                                             prep$datas,
                                             prep$ativo,
                                             prep$benchs,
                                             theme,
                                             sync_with_candles),
                 period     = periodret_module(prep$ret_sim,
                                               prep$datas,
                                               prep$ativo,
                                               prep$benchs,
                                               theme,
                                               sync_with_candles),
                 drawdowns  = drawdown_module(prep$dds,
                                              prep$datas,
                                              prep$ativo,
                                              prep$benchs,
                                              theme,
                                              sync_with_candles),
                 table      = rentab_table_module(prep$lista_tabs,
                                                  prep$ativo,
                                                  prep$benchs,
                                                  theme),
                 footer     = footer_module(theme)
    )
    page <- tagAppendChild(page,
                           tags$div(class="module", ui))
  }
  container <- tags$div(
    id    = "tplot-container",
    style = sprintf("background:%s;color:%s;",
                    theme$colors$page_bg,
                    theme$colors$page_txt),
    page
  )
  doc <- tags$html(
    lang = "pt-BR",
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
             showWarnings = FALSE)
  nome    <- sprintf("tplot_%s_%s.html",
                     prep$ativo,
                     format(Sys.time(), "%Y%m%d_%H%M%S"))
  destino <- file.path(output_dir, nome)
  ## 7) salva com htmltools::save_html()
  #    selfcontained = FALSE e libdir = "files"

  save_html(
    html   = doc,
    file   = destino,
    libdir = "https://balboa.wiseturtle.com.br/api/hplots/files",
    background = theme$colors$page_bg
  )

  html_content <- readLines(destino, encoding = "UTF-8")
  html_content <- gsub("https%3A/", "https://", html_content, fixed = TRUE)
  html_content <- gsub("%2F", "/", html_content, fixed = TRUE)
  writeLines(html_content, destino, useBytes = TRUE)

  message("  HTML generated at: ", destino,
          "\n  Libs folder at: ",
          file.path(dirname(destino), "files/"))
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
  # e, se quiser, transforme em caminho absoluto
  od <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(od)) dir.create(od, recursive = TRUE, showWarnings = FALSE)
  if(!"cumulative" %in% modules)
    stop("Module 'cumulative' rendered in %.2f seconds.")
  # prepara data.frames _long_
  df_cum <- data.frame(
    date = as.Date(index(prep$ret_cum)),
    as.data.frame(prep$ret_cum, check.names=FALSE)
  )
  df_sim <- data.frame(
    date = as.Date(index(prep$ret_sim)),
    as.data.frame(prep$ret_sim, check.names=FALSE)
  )
  df_dd <- data.frame(
    date = as.Date(index(prep$dds)),
    as.data.frame(prep$dds, check.names=FALSE)
  )
  df_cum_l <- pivot_longer(df_cum, -date, names_to="series", values_to="value")
  df_sim_l <- pivot_longer(df_sim, -date, names_to="series", values_to="value")
  df_dd_l  <- pivot_longer(df_dd,  -date, names_to="series", values_to="value")

  base_theme <- theme_minimal(base_family=theme$font_family) +
    theme(
      plot.background  = element_rect(fill=theme$colors$page_bg,  color=NA),
      panel.background = element_rect(fill=theme$colors$chart_bg, color=NA),
      plot.title       = element_text(size=theme$font_sizes$title,
                                      color=theme$colors$title_txt, face="bold"),
      axis.text        = element_text(size=theme$font_sizes$axis,
                                      color=theme$colors$axis_txt),
      axis.title       = element_text(size=theme$font_sizes$axis,
                                      color=theme$colors$axis_txt),
      legend.text      = element_text(size=theme$font_sizes$legend,
                                      color=theme$colors$legend_txt)
    )
  p1 <- ggplot(df_cum_l, aes(date,value,color=series)) +
    geom_line(size=1) +
    labs(title="Cumulative Returns", x=NULL, y="Percentual") +
    scale_y_continuous(labels=label_number(scale=1,suffix="%")) +
    scale_color_manual(values=theme$palette) +
    base_theme + theme(legend.position="bottom")
  p2 <- ggplot(df_sim_l, aes(date,value,color=series)) +
    geom_line(size=0.8) +
    labs(title="Periodic Returns", x=NULL, y="Percentual") +
    scale_y_continuous(labels=label_number(scale=1,suffix="%")) +
    scale_color_manual(values=theme$palette) +
    base_theme + theme(legend.position="none")
  p3 <- ggplot(df_dd_l, aes(date,value,color=series)) +
    geom_line(size=0.8) +
    labs(title="Drawdowns", x=NULL, y="Percentual") +
    scale_y_continuous(labels=label_number(scale=1,suffix="%")) +
    scale_color_manual(values=theme$palette) +
    base_theme + theme(legend.position="none")
  tt_stats <- gridExtra::ttheme_minimal(
    core    = list(fg_params=list(fontfamily=theme$font_family,
                                  fontsize=theme$font_sizes$table,
                                  col=theme$colors$table_row_txt)),
    colhead = list(fg_params=list(fontfamily=theme$font_family,
                                  fontsize=theme$font_sizes$table,
                                  col=theme$colors$table_header_txt,
                                  fontface="bold"),
                   bg_params=list(fill=theme$colors$table_header_bg))
  )
  stats_title <- textGrob("Performance Statistics",
                          gp=gpar(fontfamily=theme$font_family,
                                  fontsize=theme$font_sizes$title,
                                  col=theme$colors$title_txt,
                                  fontface="bold"))
  stats_tbl <- tableGrob(prep$carteira_df, rows=NULL, theme=tt_stats)
  date_lbl  <- textGrob(
    paste0(format(prep$init_date,"%d-%m-%Y"), " a ",
           format(prep$finit_date,"%d-%m-%Y")),
    x=1,hjust=1,
    gp=gpar(fontfamily=theme$font_family,
            fontsize=theme$font_sizes$table,
            col=theme$colors$page_txt)
  )
  month_grobs <- lapply(names(prep$lista_tabs), function(nm){
    tab <- prep$lista_tabs[[nm]]
    df2 <- cbind(Year=rownames(tab), as.data.frame(tab,check.names=FALSE))
    pal <- theme$palette[match(nm, c(prep$ativo, prep$benchs))]
    fill<- grDevices::adjustcolor(pal, alpha.f=0.2)
    tt  <- gridExtra::ttheme_minimal(
      core    = list(fg_params=list(fontfamily=theme$font_family,
                                    fontsize=theme$font_sizes$table,
                                    col=theme$colors$table_row_txt),
                     bg_params=list(fill=fill)),
      colhead = list(fg_params=list(fontfamily=theme$font_family,
                                    fontsize=theme$font_sizes$table,
                                    col=theme$colors$table_header_txt,
                                    fontface="bold"),
                     bg_params=list(fill=theme$colors$table_header_bg))
    )
    tableGrob(df2, rows=NULL, theme=tt)
  })
  footer_lbl <- textGrob(theme$footer_text,
                         gp=gpar(fontfamily=theme$font_family,
                                 fontsize=theme$font_sizes$table,
                                 col=theme$colors$footer_txt,
                                 fontface="bold"),
                         x=0.5,hjust=0.5)
  grobs   <- c(list(stats_title, stats_tbl, date_lbl, p1, p2, p3),
               month_grobs, list(footer_lbl))
  heights <- c(0.4,1.2,0.2,2,1,1,
               rep(0.8, length(month_grobs)), 0.3)
  # grava em disco
  od <- path.expand(output_dir)
  if(!dir.exists(od)) dir.create(od, recursive=TRUE)
  fname <- sprintf("tplot_%s_%s.%s",
                   prep$ativo,
                   format(Sys.time(),"%Y%m%d_%H%M%S"),
                   format)
  fpath <- file.path(od, fname)
  w <- 1200
  h <- 300 * sum(heights) / 1.2
  if(format=="png"){
    ragg::agg_png(fpath, width=w, height=h, units="px", res=150,
                  background=theme$colors$page_bg)
  } else {
    ragg::agg_jpeg(fpath, width=w, height=h, units="px", res=150,
                   quality=0.9, background=theme$colors$page_bg)
  }
  grid.arrange(grobs=grobs, ncol=1, heights=heights)
  dev.off()
  message("  Saved chart at: ", fpath)
  return(invisible(fpath))
}
