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
      if (!inherits(ev, "try-error") && is.environment(ev)) envs[[length(envs) + 1]] <- ev
    }
    envs[[length(envs) + 1]] <- .GlobalEnv
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
  if (.is_xts(x)) {
    return(x)
  }
  # zoo is easy to promote
  if (inherits(x, "zoo")) {
    return(xts::as.xts(x))
  }
  # matrices/data.frames/tibbles
  if (is.matrix(x) || is.data.frame(x)) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    # detect index: prefer explicit date/time columns
    idx_col <- NULL
    cn <- colnames(df)
    if (!is.null(cn)) {
      idx_col <- which(tolower(cn) %in% c("date", "data", "datetime", "time", "index"))
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
    if (all(is.na(idx_parsed))) {
      return(NULL)
    }
    # coerce numeric columns
    for (j in seq_along(df)) {
      if (!is.numeric(df[[j]])) {
        df[[j]] <- suppressWarnings(as.numeric(as.character(df[[j]])))
      }
    }
    # drop all-NA columns
    keep <- vapply(df, function(col) any(!is.na(col)), logical(1))
    df <- df[, keep, drop = FALSE]
    if (ncol(df) == 0) {
      return(NULL)
    }
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
  if (!.is_xts(x)) {
    return(FALSE)
  }
  cols <- tolower(colnames(x))
  # Handle plain OHLC names and quantmod-style prefixed columns (e.g., TICKER.Open)
  has_plain <- all(c("open", "high", "low", "close") %in% cols)
  if (has_plain) {
    return(TRUE)
  }
  # Prefix-aware: look for ".Open", ".High", ".Low", ".Close"
  has_pref <- all(vapply(
    c("\\.open$", "\\.high$", "\\.low$", "\\.close$"),
    function(re) any(grepl(re, cols, ignore.case = TRUE)),
    logical(1)
  ))
  has_pref
}

#' Coerce an xts object to a standard OHLC shape expected by highcharter
#' Names the columns as Open, High, Low, Close, optionally preserving Volume.
#' @param x xts object with any OHLC-like column names
#' @return xts with 4 (or 5 with Volume) columns or NULL if cannot map
#' @keywords internal
.to_ohlc_standard <- function(x) {
  if (!.is_xts(x)) {
    return(NULL)
  }
  cols <- colnames(x)
  cl <- tolower(cols)
  pick <- function(regex) {
    idx <- which(grepl(regex, cl, ignore.case = TRUE))
    if (length(idx)) idx[1] else NA_integer_
  }
  io <- pick("(^|\\.)open$")
  ih <- pick("(^|\\.)high$")
  il <- pick("(^|\\.)low$")
  ic <- pick("(^|\\.)close$")
  ia <- pick("(^|\\.)adjust(ed)?$")
  iv <- pick("(^|\\.)volume$|(^|\\.)vol$")
  # If full OHLC exists, just map
  if (!any(is.na(c(io, ih, il, ic)))) {
    ohlc <- x[, c(io, ih, il, ic), drop = FALSE]
    colnames(ohlc) <- c("Open", "High", "Low", "Close")
  } else {
    # Fallback: synthesize OHLC from a single price column (Close/Adjusted/first)
    base_idx <- if (!is.na(ic)) ic else if (!is.na(ia)) ia else 1L
    if (is.na(base_idx) || base_idx < 1L || base_idx > NCOL(x)) {
      return(NULL)
    }
    px <- x[, base_idx, drop = FALSE]
    colnames(px) <- "Close"
    ohlc <- cbind(px, px, px, px)
    colnames(ohlc) <- c("Open", "High", "Low", "Close")
  }
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
.bt_first_text <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[[1]] else NULL
}

#' @keywords internal
.bt_block_value <- function(block, key) {
  if (is.null(block) || !is.data.frame(block) || !all(c("stat_name", "value") %in% colnames(block))) {
    return(NULL)
  }
  hit <- which(tolower(as.character(block$stat_name)) == tolower(key))
  if (!length(hit)) {
    return(NULL)
  }
  .bt_first_text(block$value[hit[1]])
}

#' @keywords internal
.bt_stats_value <- function(stats, key) {
  if (is.null(stats)) {
    return(NULL)
  }
  if (is.data.frame(stats) && key %in% colnames(stats)) {
    return(.bt_first_text(stats[[key]]))
  }
  if (is.list(stats) && !is.null(stats[[key]])) {
    return(.bt_first_text(stats[[key]]))
  }
  NULL
}

#' @keywords internal
.bt_extract_symbol <- function(obj, trades = NULL) {
  sym <- .bt_first_text(obj$symbol)
  if (!is.null(sym)) {
    return(sym)
  }
  sym <- .bt_stats_value(obj$stats, "Symbol")
  if (!is.null(sym)) {
    return(sym)
  }
  sym <- .bt_first_text(attr(obj$info_blocks, "label", exact = TRUE))
  if (!is.null(sym)) {
    return(sym)
  }
  if (is.list(obj$info_blocks) && !is.null(obj$info_blocks$strategy)) {
    sym <- .bt_block_value(obj$info_blocks$strategy, "Symbol")
    if (!is.null(sym)) {
      return(sym)
    }
  }
  if (!is.null(trades)) {
    if (is.data.frame(obj$trades) && "symbol" %in% tolower(colnames(obj$trades))) {
      idx <- which(tolower(colnames(obj$trades)) == "symbol")[1]
      sym <- .bt_first_text(obj$trades[[idx]])
      if (!is.null(sym)) {
        return(sym)
      }
    }
    if (.is_xts(trades) && "symbol" %in% tolower(colnames(trades))) {
      idx <- which(tolower(colnames(trades)) == "symbol")[1]
      sym <- .bt_first_text(trades[, idx])
      if (!is.null(sym)) {
        return(sym)
      }
    }
  }
  NULL
}

#' @keywords internal
.bt_select_returns <- function(obj) {
  for (source_name in c("rets", "rets_acct", "raw_rets")) {
    cand <- obj[[source_name]]
    if (is.null(cand)) {
      next
    }
    cand_xts <- .to_xts(cand)
    if (!.is_xts(cand_xts) || NCOL(cand_xts) < 1L) {
      next
    }
    cn <- colnames(cand_xts)
    lc <- tolower(cn %||% character(NCOL(cand_xts)))
    discrete_idx <- which(lc == "discrete")
    log_idx <- which(lc == "log")
    if (length(discrete_idx)) {
      out <- cand_xts[, discrete_idx[1], drop = FALSE]
      colnames(out) <- "Discrete"
      return(list(rets = out, source = source_name, type = "Discrete"))
    }
    if (length(log_idx)) {
      out <- exp(cand_xts[, log_idx[1], drop = FALSE]) - 1
      colnames(out) <- "Discrete"
      return(list(rets = out, source = source_name, type = "Log"))
    }
    if (NCOL(cand_xts) == 1L) {
      out <- cand_xts[, 1, drop = FALSE]
      colnames(out) <- "Discrete"
      type_name <- if (!is.null(cn) && length(cn) >= 1L && nzchar(cn[[1]])) cn[[1]] else "single"
      return(list(rets = out, source = source_name, type = type_name))
    }
  }
  NULL
}

#' @keywords internal
.bt_add_trade_aliases <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  cn <- colnames(x)
  lc <- tolower(cn %||% character(0))
  add_alias <- function(out_name, in_name) {
    if (out_name %in% colnames(x)) {
      return(invisible(NULL))
    }
    idx <- which(lc == tolower(in_name))
    if (!length(idx)) {
      return(invisible(NULL))
    }
    x[[out_name]] <<- x[[idx[1]]]
    invisible(NULL)
  }
  add_alias("Txn.Qty", "qty_delta")
  add_alias("Txn.Price", "price")
  add_alias("Pos.Qty", "qty")
  x
}

#' @keywords internal
.bt_normalize_trades <- function(trades) {
  if (is.null(trades)) {
    return(NULL)
  }
  if (.is_xts(trades)) {
    return(.bt_add_trade_aliases(trades))
  }
  if (!is.data.frame(trades)) {
    return(NULL)
  }
  df <- .bt_add_trade_aliases(trades)
  cn <- tolower(colnames(df))
  time_col <- which(cn %in% c("time", "date", "datetime", "txn.date", "timestamp"))
  if (!length(time_col)) {
    return(NULL)
  }
  idx <- df[[time_col[1]]]
  df[[time_col[1]]] <- NULL
  idx_parsed <- suppressWarnings(as.POSIXct(idx, tz = "UTC"))
  if (all(is.na(idx_parsed))) {
    idx_parsed <- suppressWarnings(as.Date(idx))
  }
  if (all(is.na(idx_parsed))) {
    return(NULL)
  }
  for (j in seq_along(df)) {
    if (!is.numeric(df[[j]])) {
      df[[j]] <- suppressWarnings(as.numeric(as.character(df[[j]])))
    }
  }
  keep <- vapply(df, function(col) any(!is.na(col)), logical(1))
  df <- df[, keep, drop = FALSE]
  if (ncol(df) == 0) {
    return(NULL)
  }
  tx <- xts::xts(df, order.by = idx_parsed)
  tx[order(index(tx))]
}

#' @keywords internal
.bt_extract_info <- function(obj, symbol = NULL, return_meta = NULL) {
  lines <- character(0)
  add <- function(label, value) {
    value <- .bt_first_text(value)
    if (!is.null(value)) {
      lines <<- c(lines, paste0(label, ": ", value))
    }
  }
  add("Symbol", symbol)
  if (!is.null(return_meta)) {
    add("Returns", paste0(return_meta$source, "$", return_meta$type))
  }
  st <- obj$stats %||% obj$performance_stats %||% obj$raw_stats
  for (pair in list(
    c("Total Return", "total_return"),
    c("Annualized Return", "annualized_return"),
    c("Annualized Vol", "annualized_vol"),
    c("Sharpe", "sharpe"),
    c("Max Drawdown", "max_drawdown"),
    c("Trades", "num_trades"),
    c("Net Profit", "net_profit"),
    c("Return Source", "ReturnSource"),
    c("Risk Target", "RiskTarget"),
    c("Risk Scale", "RiskScale"),
    c("Execution", "Execution"),
    c("Indicator", "Indicator"),
    c("Position Sizing", "PosSiz"),
    c("Risk Pct", "RiskPct"),
    c("Fee", "FeeValue"),
    c("Slippage", "SlipValue")
  )) {
    add(pair[1], .bt_stats_value(st, pair[2]))
  }
  if (is.list(obj$info_blocks)) {
    add("Strategy", .bt_block_value(obj$info_blocks$strategy, "Strategy"))
    add("Sizing", .bt_block_value(obj$info_blocks$sizing, "Position Sizing"))
    add("Performance Source", .bt_block_value(obj$info_blocks$risk_normalization, "Performance Source"))
    add("Instrument", .bt_block_value(obj$info_blocks$instrument, "Root"))
  }
  lines <- unique(lines[nzchar(lines)])
  if (!length(lines)) {
    return(NULL)
  }
  paste(lines, collapse = "\n")
}

#' @keywords internal
.as_backtest <- function(obj) {
  if (!is.list(obj)) {
    return(NULL)
  }
  # Normalize mktdata
  md <- NULL
  if (!is.null(obj$mktdata)) {
    md <- .to_xts(obj$mktdata)
    if (!.is_xts(md)) md <- NULL
  }
  # Normalize trades to xts if possible
  tx <- .bt_normalize_trades(obj$trades)
  # Normalize returns (prefer 'rets' over 'rets_acct', and Discrete over Log)
  ret_meta <- .bt_select_returns(obj)
  rt <- if (!is.null(ret_meta)) ret_meta$rets else NULL
  symbol <- .bt_extract_symbol(obj, trades = tx)
  info <- .bt_extract_info(obj, symbol = symbol, return_meta = ret_meta)
  extras <- .bt_extract_extras(obj, symbol = symbol)
  if (is.null(md) && is.null(tx) && is.null(rt) && is.null(obj$stats)) {
    return(NULL)
  }
  list(
    mktdata = md,
    trades  = tx,
    stats   = obj$stats %||% NULL,
    rets    = rt,
    symbol  = symbol,
    info    = info,
    extras  = extras,
    return_source = if (!is.null(ret_meta)) ret_meta$source else NULL,
    return_type = if (!is.null(ret_meta)) ret_meta$type else NULL
  )
}

#' Try to resolve a blotter/quantstrat portfolio by name
#' @param name Portfolio name (character).
#' @param init Start date to subset.
#' @param finit End date to subset.
#' @return list(mktdata=xts or NULL, trades=xts or NULL, symbol=character) or NULL
#' @keywords internal
.quantstrat_portfolio_data <- function(name, init = NULL, finit = NULL) {
  if (is.null(name) || !is.character(name) || length(name) != 1L) {
    return(NULL)
  }
  getPortfolio <- .get_function_if_exists("getPortfolio")
  getTxns <- .get_function_if_exists("getTxns")
  if (is.null(getPortfolio)) {
    return(NULL)
  }
  pf <- tryCatch(getPortfolio(name), error = function(e) NULL)
  if (is.null(pf) || is.null(pf$symbols)) {
    return(NULL)
  }
  syms <- names(pf$symbols)
  if (length(syms) == 0) {
    return(NULL)
  }
  # Prefer a symbol that actually has transactions
  chosen <- syms[1]
  if (!is.null(getTxns)) {
    for (s in syms) {
      tx_try <- tryCatch(getTxns(Portfolio = name, Symbol = s), error = function(e) NULL)
      if (.is_xts(tx_try) && NROW(tx_try) > 0) {
        chosen <- s
        break
      }
    }
  }
  # Get transactions: prefer portfolio-stored txn (has Pos.Qty), fallback to getTxns()
  trades <- NULL
  if (!is.null(pf$symbols[[chosen]]$txn)) {
    trades <- tryCatch(pf$symbols[[chosen]]$txn, error = function(e) NULL)
  }
  if (is.null(trades) && !is.null(getTxns)) {
    trades <- tryCatch(getTxns(Portfolio = name, Symbol = chosen), error = function(e) NULL)
  }
  if (.is_xts(trades)) trades <- .subset_xts(trades, init, finit)
  # Try to locate OHLC market data in the environment first
  raw_obj <- .get_object_if_exists(chosen)
  mktdata <- NULL
  if (!is.null(raw_obj)) {
    # Try direct xts conversion first
    conv <- .to_xts(raw_obj)
    if (.is_xts(conv)) mktdata <- .subset_xts(conv, init, finit)
    # If still not xts, try quantmod::getPrice
    if (is.null(mktdata)) {
      gp <- .get_function_if_exists("getPrice")
      if (!is.null(gp)) {
        px <- tryCatch(gp(raw_obj), error = function(e) NULL)
        if (.is_xts(px)) mktdata <- .subset_xts(px, init, finit)
      }
    }
  }
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
  if (!.is_xts(x)) {
    return(x)
  }
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
  if (is.null(symbols) || length(symbols) == 0) {
    return(out)
  }
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
