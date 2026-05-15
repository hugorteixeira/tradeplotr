#' Prepares all data and metrics for tplot rendering
#' @param ativo The main asset (name or xts object).
#' @param benchs A list of benchmarks.
#' @param init Start date.
#' @param finit End date.
#' @param rf_rate The risk-free rate.
#' @param geometric Logical, for geometric returns.
#' @param normalize_risk Optional annualized volatility target for regular ticker
#' series.
#' @param group_lines Optional group-return line selector.
#' @param normalize_group_risk Optional annualized volatility target for the
#' grouped return line.
#' @param ativo_name An explicit name for the main asset.
#' @return A large list containing all data and metrics prepared for the modules.
#' @keywords internal
.tplot_prepare <- function(ativo, benchs, init, finit, rf_rate, geometric = TRUE,
                           normalize_risk = NULL,
                           group_lines = NULL,
                           normalize_group_risk = NULL,
                           ativo_name = NULL,
                           verbose = getOption("tplot.verbose", FALSE)) {
  vmsg <- function(...) {
    if (isTRUE(verbose)) message(...)
  }
  # 1) Guarantee Date inputs
  init <- as.Date(init)
  finit <- as.Date(finit)

  # 2) Prefer quantstrat/blotter portfolio data by name (portfolio-first lookup)
  mktdata <- NULL
  trades <- NULL
  stats <- NULL
  pf_name <- NULL
  port_rets <- NULL
  bt_rets <- NULL
  from_backtest <- FALSE
  asset_info_map <- list()
  bt_extras_map <- list()
  # Optional hint provided by wrapper to select a specific portfolio/symbol
  hint <- getOption("tplot.portfolio_hint", NULL)
  if (is.list(hint) && is.character(hint$name)) {
    qs <- .quantstrat_portfolio_data(hint$name, init, finit)
    if (is.list(qs)) {
      mktdata <- qs$mktdata %||% NULL
      trades <- qs$trades %||% NULL
      chosen <- qs$symbol %||% NULL
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
            if (.is_xts(fetched)) {
              mktdata <- .subset_xts(fetched, init, finit)
            } else if (is.list(fetched) && length(fetched) > 0 && .is_xts(fetched[[1]])) mktdata <- .subset_xts(fetched[[1]], init, finit)
          }
          if (is.null(mktdata) || !.is_ohlc_xts(mktdata)) {
            obj <- tryCatch(suppressWarnings(quantmod::getSymbols(hint$symbol, from = init, to = finit, auto.assign = FALSE)), error = function(e) NULL)
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
      trades <- qs$trades %||% NULL
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
  ativo_obj <- if (!is.character(ativo)) ativo else .get_object_if_exists(ativo)
  ativo_source_obj <- ativo_obj
  # If no quantstrat portfolio detected, try a generic backtest object in the env
  if (is.null(mktdata) && is.null(trades)) {
    bt_env <- .as_backtest(ativo_obj)
    if (is.null(bt_env) && is.list(ativo_obj) && length(ativo_obj) > 0) {
      nested_bt <- lapply(ativo_obj, .as_backtest)
      valid_idx <- which(!vapply(nested_bt, is.null, logical(1), USE.NAMES = FALSE))
      if (length(valid_idx) > 0) {
        idx_use <- valid_idx[1]
        bt_env <- nested_bt[[idx_use]]
        if (is.list(ativo_obj[[idx_use]])) {
          ativo_source_obj <- ativo_obj[[idx_use]]
        }
        nm_use <- names(ativo_obj)
        if (!is.null(nm_use) && length(nm_use) >= idx_use) {
          chosen_label <- nm_use[[idx_use]]
          if (!is.null(chosen_label) && nzchar(chosen_label)) {
            if (is.null(ativo_name) || identical(ativo_name, ativo)) {
              ativo_name <- chosen_label
            }
          }
        } else if (is.null(ativo_name) || identical(ativo_name, ativo)) {
          ativo_name <- paste0("Asset", idx_use)
        }
      }
    }
    if (!is.null(bt_env)) {
      from_backtest <- TRUE
      mktdata <- bt_env$mktdata %||% NULL
      trades <- bt_env$trades %||% NULL
      stats <- bt_env$stats %||% NULL
      bt_rets <- bt_env$rets %||% NULL
      # Prefer asset label from stats$Symbol when present
      extract_sym <- function(st) {
        if (is.null(st)) {
          return(NULL)
        }
        if (is.data.frame(st) && "Symbol" %in% colnames(st)) {
          vv <- as.character(st$Symbol)
          vv <- vv[!is.na(vv) & nzchar(vv)]
          if (length(vv) > 0) {
            return(vv[1])
          }
        }
        if (is.list(st) && !is.null(st$Symbol)) {
          vv <- as.character(st$Symbol)
          vv <- vv[!is.na(vv) & nzchar(vv)]
          if (length(vv) > 0) {
            return(vv[1])
          }
        }
        NULL
      }
      sym_from_bt <- bt_env$symbol %||% extract_sym(stats)
      if (!is.null(sym_from_bt)) {
        # Always prefer the symbol carried by a native backtest object.
        ativo_name <- sym_from_bt
      } else if (is.null(ativo_name)) {
        # Fallback: if original object is a list, try first element name
        if (is.list(ativo_source_obj)) {
          nms <- names(ativo_source_obj)
          if (!is.null(nms) && length(nms) > 0 && nzchar(nms[1])) ativo_name <- nms[1]
        }
      }
      if (!is.null(bt_env$info) && !is.null(ativo_name) && nzchar(ativo_name)) {
        asset_info_map[[ativo_name]] <- bt_env$info
      }
      if (is.list(bt_env$extras) && !is.null(ativo_name) && nzchar(ativo_name)) {
        bt_extras_map[[ativo_name]] <- bt_env$extras
      }
    }
  }

  # 3) Resolve series map for returns (env-first), with portfolio/backtest awareness
  series_map <- list()
  requested <- character(0)
  # ativo can be xts or string name
  if (.is_xts(ativo) || is.matrix(ativo) || is.data.frame(ativo) || inherits(ativo, "zoo")) {
    at_xts <- .to_xts(ativo)
    if (!.is_xts(at_xts)) stop("Could not convert ticker to xts.")
    if (is.null(ativo_name)) ativo_name <- "Assets"
    series_map[[ativo_name]] <- .subset_xts(at_xts, init, finit)
  } else if (is.character(ativo) && length(ativo) == 1) {
    if (is.null(ativo_name)) ativo_name <- ativo
    if (!is.null(port_rets) && .is_xts(port_rets) && NROW(port_rets) > 0) {
      # Prefer backtest portfolio returns (already discrete)
      series_map[[ativo_name]] <- port_rets
    } else if (!is.null(bt_rets) && .is_xts(bt_rets)) {
      series_map[[ativo_name]] <- .subset_xts(bt_rets, init, finit)
    } else if (!is.null(mktdata) && .is_xts(mktdata)) {
      # Fallback: use price series directly
      series_map[[ativo_name]] <- .subset_xts(mktdata, init, finit)
    } else {
      requested <- c(requested, ativo)
    }
  } else if (!is.null(bt_rets) && .is_xts(bt_rets)) {
    if (is.null(ativo_name)) ativo_name <- "Asset"
    series_map[[ativo_name]] <- .subset_xts(bt_rets, init, finit)
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
  label_from_stats <- function(st, default_label) {
    if (!is.null(st)) {
      if (is.data.frame(st) && "Symbol" %in% colnames(st)) {
        vv <- as.character(st$Symbol)
        vv <- vv[!is.na(vv) & nzchar(vv)]
        if (length(vv) > 0) {
          return(vv[1])
        }
      }
      if (is.list(st) && !is.null(st$Symbol)) {
        vv <- as.character(st$Symbol)
        vv <- vv[!is.na(vv) & nzchar(vv)]
        if (length(vv) > 0) {
          return(vv[1])
        }
      }
    }
    default_label
  }
  if (!is.null(benchs)) {
    if (is.character(benchs)) {
      # iterate and try to treat each as backtest object or symbol
      for (sym in benchs) {
        added <- FALSE
        if (is.character(sym) && length(sym) == 1) {
          obj <- .get_object_if_exists(sym)
          bt <- .as_backtest(obj)
          if (!is.null(bt) && .is_xts(bt$rets)) {
            lab <- bt$symbol %||% label_from_stats(bt$stats, sym)
            series_map[[lab]] <- .subset_xts(bt$rets, init, finit)
            benchs_names <- c(benchs_names, lab)
            if (!is.null(bt$info)) asset_info_map[[lab]] <- bt$info
            if (is.list(bt$extras)) bt_extras_map[[lab]] <- bt$extras
            added <- TRUE
          }
        }
        if (!added) {
          requested <- c(requested, sym)
          benchs_names <- c(benchs_names, sym)
        }
      }
    } else if (.is_xts(benchs) || is.matrix(benchs) || is.data.frame(benchs) || inherits(benchs, "zoo")) {
      # Treat single xts/data.frame object as ONE benchmark asset
      b_xts <- .to_xts(benchs)
      b_lab <- as.character(substitute(benchs))
      if (is.null(b_lab) || b_lab == "") b_lab <- "Bench"
      series_map[[b_lab]] <- .subset_xts(b_xts, init, finit)
      benchs_names <- c(benchs_names, b_lab)
    } else if (is.list(benchs)) {
      # list of mixed items: xts, backtest list, or character names
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
        } else if (is.list(bi)) {
          bt <- .as_backtest(bi)
          if (!is.null(bt) && .is_xts(bt$rets)) {
            def_nm <- if (!is.null(nm) && nzchar(nm)) nm else paste0("Bench", i)
            lab <- bt$symbol %||% label_from_stats(bt$stats, def_nm)
            series_map[[lab]] <- .subset_xts(bt$rets, init, finit)
            benchs_names <- c(benchs_names, lab)
            if (!is.null(bt$info)) asset_info_map[[lab]] <- bt$info
            if (is.list(bt$extras)) bt_extras_map[[lab]] <- bt$extras
          }
        } else if (is.character(bi) && length(bi) == 1) {
          # try resolve by name as backtest first
          obj <- .get_object_if_exists(bi)
          bt <- .as_backtest(obj)
          if (!is.null(bt) && .is_xts(bt$rets)) {
            lab <- bt$symbol %||% label_from_stats(bt$stats, bi)
            series_map[[lab]] <- .subset_xts(bt$rets, init, finit)
            benchs_names <- c(benchs_names, lab)
            if (!is.null(bt$info)) asset_info_map[[lab]] <- bt$info
            if (is.list(bt$extras)) bt_extras_map[[lab]] <- bt$extras
          } else {
            requested <- c(requested, bi)
            benchs_names <- c(benchs_names, bi)
          }
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
      if (is.null(cnt)) {
        seen_req[[sym]] <- 1L
        lbls[i] <- sym
      } else {
        cnt <- cnt + 1L
        seen_req[[sym]] <- cnt
        lbls[i] <- paste0(sym, "_", cnt)
      }
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
        fetched_one <- tryCatch(
          od(sym,
            start_date   = init,
            end_date     = finit,
            auto_returns = geometric
          ),
          error = function(e) NULL
        )
        if (.is_xts(fetched_one)) {
          dados_raw[[lab]] <- .subset_xts(fetched_one, init, finit)
        } else if (is.list(fetched_one) && length(fetched_one) > 0) {
          pick <- NULL
          if (!is.null(names(fetched_one)) && sym %in% names(fetched_one)) pick <- fetched_one[[sym]]
          if (is.null(pick)) {
            for (el in fetched_one) {
              if (.is_xts(el)) {
                pick <- el
                break
              }
            }
          }
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
  # (legacy attribute-based detection dropped; backtest handled above)

  # 5.1) Choose an OHLC series for the candles module when possible.
  # Preference order: backtest mktdata (if valid OHLC), then first OHLC among
  # 'ativo' and 'benchs' (in the order they were provided).
  if ((is.null(mktdata) || !.is_ohlc_xts(mktdata)) && !from_backtest && is.null(pf_name)) {
    choose_first_ohlc <- function(nm) {
      if (is.null(nm)) {
        return(NULL)
      }
      x <- dados_raw[[nm]]
      if (.is_xts(x) && .is_ohlc_xts(x)) {
        return(.subset_xts(x, init, finit))
      }
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
          if (!is.null(cand)) {
            mktdata <- cand
            break
          }
        }
      }
    }
  }

  # 6) Normalize/prepare returns from whatever we have
  dados_trat <- .data_prepare(dados_raw, verbose = verbose)
  vmsg(sprintf(
    "[.tplot_prepare] .data_prepare class=%s | rows=%s | cols=%s",
    paste(class(dados_trat), collapse = ","),
    tryCatch(NROW(dados_trat), error = function(e) "?"),
    tryCatch(NCOL(dados_trat), error = function(e) "?")
  ))
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
        if (!.is_xts(xx)) {
          return(NULL)
        }
        # pick Adjusted/Close/first
        cn <- colnames(xx)
        pick <- if (length(cn)) {
          idxA <- grep("(^|\\.)Adjusted$", cn, ignore.case = TRUE)
          idxC <- grep("(^|\\.)Close$", cn, ignore.case = TRUE)
          if (length(idxA)) idxA[1] else if (length(idxC)) idxC[1] else 1L
        } else {
          1L
        }
        px <- xx[, pick, drop = FALSE]
        ret <- try(PerformanceAnalytics::Return.calculate(px, method = "discrete"), silent = TRUE)
        if (inherits(ret, "try-error")) {
          return(NULL)
        }
        ret[1, ] <- 0
        ret
      }
      rets <- list()
      for (nm in names(dados_raw)) {
        rr <- build_ret(dados_raw[[nm]])
        if (.is_xts(rr)) {
          colnames(rr) <- nm
          rets[[nm]] <- rr
        }
      }
      if (length(rets) == 0) stop("Prepared data is not an xts time series.")
      merged <- Reduce(function(a, b) merge(a, b, join = "inner"), rets)
      # Aggregate to daily by compounding within each day (safe baseline)
      ep <- tryCatch(xts::endpoints(merged, on = "days"), error = function(e) integer(0))
      if (length(ep) > 1) {
        agg <- lapply(seq_len(ncol(merged)), function(j) {
          vals <- merged[, j, drop = FALSE]
          out <- vector("numeric", length(ep) - 1L)
          for (k in seq_len(length(ep) - 1L)) {
            seg <- vals[(ep[k] + 1L):ep[k + 1L], , drop = FALSE]
            rr <- as.numeric(seg)
            rr <- rr[is.finite(rr)]
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
    if (is.null(base_nms) || length(base_nms) == 0) {
      base_nms <- paste0("Serie", seq_len(NCOL(dados_trat)))
    }
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
  asset_info <- list()
  if (length(asset_info_map)) {
    for (nm in keep_cols) {
      if (!is.null(asset_info_map[[nm]])) {
        asset_info[[nm]] <- asset_info_map[[nm]]
      }
    }
    if (length(asset_info) == 0 && length(asset_info_map) == 1L && length(keep_cols) >= 1L) {
      asset_info[[keep_cols[1]]] <- asset_info_map[[1]]
    }
  }
  bt_extras <- list()
  if (length(bt_extras_map)) {
    for (nm in keep_cols) {
      if (!is.null(bt_extras_map[[nm]])) {
        bt_extras[[nm]] <- bt_extras_map[[nm]]
      }
    }
    if (length(bt_extras) == 0 && length(bt_extras_map) == 1L && length(keep_cols) >= 1L) {
      bt_extras[[keep_cols[1]]] <- bt_extras_map[[1]]
      for (field in c("costs", "trade_quality", "trade_quality_points")) {
        if (!is.null(bt_extras[[keep_cols[1]]][[field]]) && "Asset" %in% colnames(bt_extras[[keep_cols[1]]][[field]])) {
          bt_extras[[keep_cols[1]]][[field]]$Asset <- keep_cols[1]
        }
      }
    }
  }
  # Build two views: full (outer-joined) and common (intersection of all series)
  carteira_full <- dados_trat[, keep_cols, drop = FALSE]

  risk_applied <- NULL
  if (!is.null(normalize_risk)) {
    risk_target <- suppressWarnings(as.numeric(normalize_risk[1]))
    if (!is.finite(risk_target) || risk_target <= 0) {
      warning("'normalize_risk' must be a positive numeric value. Ignoring request.")
    } else if (length(keep_cols) > 0) {
      normalized_cols <- list()
      for (col in keep_cols) {
        series_orig <- carteira_full[, col, drop = FALSE]
        if (NROW(series_orig) == 0 || all(is.na(series_orig))) next
        base_xts <- xts::xts(series_orig[, 1], order.by = index(series_orig))
        colnames(base_xts) <- "Discrete"
        scaled <- tryCatch(
          suppressWarnings(.normalize_risk(base_xts, risk = risk_target, type = "Discrete")),
          error = function(e) {
            warning(sprintf("Could not normalize risk for '%s': %s", col, conditionMessage(e)))
            NULL
          }
        )
        if (!is.null(scaled) && xts::is.xts(scaled)) {
          scaled_values <- as.numeric(scaled)
          if (length(scaled_values) != NROW(carteira_full)) {
            warning(sprintf(
              "Could not normalize risk for '%s': normalized series length (%d) differs from prepared series length (%d).",
              col,
              length(scaled_values),
              NROW(carteira_full)
            ))
            next
          }
          normalized_cols[[col]] <- scaled_values
        }
      }
      if (length(normalized_cols) > 0) {
        for (col in names(normalized_cols)) {
          carteira_full[, col] <- normalized_cols[[col]]
          if (col %in% colnames(dados_trat) &&
            NROW(dados_trat) == length(normalized_cols[[col]]) &&
            identical(index(dados_trat), index(carteira_full))) {
            dados_trat[, col] <- normalized_cols[[col]]
          }
        }
        risk_applied <- risk_target
        vmsg(sprintf("[.tplot_prepare] Applied risk normalization target: %.4f%%", risk_target))
      }
    }
  }

  group_risk_applied <- NULL
  group_line <- .build_group_line(carteira_full, group_lines, normalize_group_risk = normalize_group_risk)
  if (!is.null(group_line)) {
    group_name <- colnames(group_line)[1]
    group_risk_applied <- attr(group_line, "normalize_group_risk", exact = TRUE)
    carteira_full <- merge(carteira_full, group_line, join = "left")
    colnames(carteira_full)[NCOL(carteira_full)] <- group_name
    keep_cols <- c(keep_cols, group_name)
    benchs_labels <- c(benchs_labels, group_name)
    vmsg(sprintf("[.tplot_prepare] Added grouped return line: %s", group_name))
  }

  # Keep only rows where all selected series have data for fair charting/metrics
  complete_rows <- apply(!is.na(carteira_full), 1, all)
  if (!any(complete_rows)) {
    message("[.tplot_prepare] No overlapping period across selected series; using last non-NA overlaps if available.")
    carteira <- na.omit(carteira_full)
  } else {
    carteira <- carteira_full[complete_rows, , drop = FALSE]
  }

  # 8) Risk-free handling (optional; default to 0 if not available)
  if (!is.null(rf_rate) && rf_rate %in% colnames(dados_trat)) {
    rf_final <- as.numeric(
      Return.annualized(dados_trat[, rf_rate], geometric = geometric)[1, 1]
    )
  } else {
    if (!is.null(rf_rate)) message("Risk free rate of '", rf_rate, "' not found. Using RF = 0.")
    rf_final <- 0
  }

  # 9) Metrics
  car_anu <- Return.annualized(carteira, geometric = geometric)
  car_tot <- Return.cumulative(carteira, geometric = geometric)
  car_dd <- maxDrawdown(carteira)
  # Annualize SD using detected periodicity of 'carteira'
  # Infer return frequency without xts::periodicity to avoid segfaults
  infer_sd_scale <- function(x) {
    idx <- index(x)
    if (inherits(idx, "Date")) {
      return("daily")
    }
    if (inherits(idx, "POSIXct") || inherits(idx, "POSIXt")) {
      d <- suppressWarnings(stats::median(diff(as.numeric(idx)), na.rm = TRUE))
      day_s <- 86400
      week_s <- day_s * 7
      month_s <- day_s * 30
      quarter_s <- day_s * 90
      if (!is.finite(d) || is.na(d) || d <= 0) {
        return("daily")
      }
      if (d < week_s) {
        return("daily")
      }
      if (d < month_s) {
        return("weekly")
      }
      if (d < quarter_s) {
        return("monthly")
      }
      return("quarterly")
    }
    # fallback
    "daily"
  }
  per_card <- infer_sd_scale(carteira)
  scale_fac <- switch(tolower(per_card),
    "daily" = 252,
    "day" = 252,
    "weekly" = 52,
    "week" = 52,
    "monthly" = 12,
    "month" = 12,
    "quarterly" = 4,
    "quarter" = 4,
    "yearly" = 1,
    "annual" = 1,
    252
  )
  car_sd <- apply(na.omit(carteira), 2, sd) * sqrt(scale_fac)
  car_sharpe <- (car_anu - rf_final) / car_sd
  car_sort <- SortinoRatio(carteira)
  car_sort[is.infinite(car_sort)] <- 0

  carteira_df <- data.frame(
    Asset = colnames(carteira),
    Total = round(as.vector(car_tot * 100), 3),
    CAR = round(as.vector(car_anu * 100), 2),
    MaxDD = round(as.vector(car_dd * 100), 2),
    Std_Dev = round(as.vector(car_sd * 100), 3),
    Sharpe = round(as.vector(car_sharpe), 2),
    Sortino = round(as.vector(car_sort), 3),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  if ("IPCA" %in% carteira_df$Asset) {
    ip <- which(carteira_df$Asset == "IPCA")
    carteira_df[ip, c("MaxDD", "Std_Dev", "Sharpe", "Sortino")] <- "-"
  }

  # 10) Derived series for plots
  if (isFALSE(geometric)) {
    ret_cum <- cumsum(carteira) * 100
  } else {
    ret_cum <- (cumprod(1 + carteira) - 1) * 100
  }
  ret_sim <- carteira * 100
  dds <- Drawdowns(carteira) * 100
  datas <- as.numeric(as.POSIXct(index(ret_cum))) * 1000
  # English aliases
  cum_returns <- ret_cum
  period_returns <- ret_sim
  drawdowns <- dds
  timestamps <- datas

  # Build monthly/annual tables per series using each one's full history
  lista_tabs <- lapply(colnames(carteira_full), function(x) {
    series_x <- carteira_full[, x, drop = FALSE]
    series_x <- na.omit(series_x)
    if (NROW(series_x) < 1) {
      return(data.frame())
    }
    rentab_table_calc(series_x, retornar = TRUE, geometric = geometric)
  })
  names(lista_tabs) <- colnames(carteira_full)
  returns_tables <- lista_tabs
  rolling_corr <- .rolling_corr_calc(carteira)
  rolling_corr_timestamps <- if (!is.null(rolling_corr)) as.numeric(as.POSIXct(index(rolling_corr))) * 1000 else NULL
  costs_df <- .bt_bind_extra(bt_extras, "costs")
  trade_quality_df <- .bt_bind_extra(bt_extras, "trade_quality")
  trade_quality_points <- .bt_bind_extra(bt_extras, "trade_quality_points")

  resultado <- list(
    # Preferred English fields
    asset = ativo_name,
    benchmarks = benchs_labels,
    start_date = first(index(cum_returns)),
    end_date = last(index(cum_returns)),
    portfolio = carteira,
    stats_df = carteira_df,
    asset_info = asset_info,
    costs_df = costs_df,
    trade_quality_df = trade_quality_df,
    trade_quality_points = trade_quality_points,
    rolling_corr = rolling_corr,
    rolling_corr_timestamps = rolling_corr_timestamps,
    cum_returns = cum_returns,
    period_returns = period_returns,
    drawdowns = drawdowns,
    timestamps = timestamps,
    returns_tables = returns_tables,
    # Legacy fields for backward-compatibility
    ativo = ativo_name,
    benchs = benchs_labels,
    init_date = first(index(ret_cum)),
    finit_date = last(index(ret_cum)),
    carteira = carteira,
    carteira_df = carteira_df,
    ret_cum = ret_cum,
    ret_sim = ret_sim,
    dds = dds,
    datas = datas,
    lista_tabs = lista_tabs
  )

  if (!is.null(risk_applied)) resultado$normalize_risk <- risk_applied
  if (!is.null(group_risk_applied)) resultado$normalize_group_risk <- group_risk_applied

  # 11) Conditionally attach backtest extras
  if (!is.null(mktdata)) resultado$mktdata <- mktdata
  if (!is.null(trades)) resultado$trades <- if (NROW(trades) > 1) trades[-1] else trades
  if (!is.null(stats)) resultado$stats <- stats

  resultado
}
