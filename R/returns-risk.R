#' Normalize the volatility of a returns series to a target annualized risk in percent.
#' @keywords internal
.normalize_risk <- function(xts, risk = 10, type = c("Discrete", "Log")) {
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("Package 'xts' is required.")
  }

  type <- match.arg(type)

  find_returns_in_xts <- function(x) {
    if (!xts::is.xts(x)) {
      return(list(log = NULL, discrete = NULL))
    }
    cn <- colnames(x) %||% character(0)
    lc <- tolower(cn)
    log_idx <- which(lc == "log")
    disc_idx <- which(lc == "discrete")
    list(
      log = if (length(log_idx)) x[, log_idx[1], drop = FALSE] else NULL,
      discrete = if (length(disc_idx)) x[, disc_idx[1], drop = FALSE] else NULL
    )
  }

  search_returns <- function(obj, depth = 0, max_depth = 4) {
    if (depth > max_depth) {
      return(NULL)
    }
    if (xts::is.xts(obj)) {
      fr <- find_returns_in_xts(obj)
      if (!is.null(fr$log) || !is.null(fr$discrete)) {
        return(list(host = obj, log = fr$log, discrete = fr$discrete))
      }
      ra <- attr(obj, "rets")
      if (!is.null(ra)) {
        res <- search_returns(ra, depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      if (NCOL(obj) == 1) {
        colnames(obj) <- "Discrete"
        return(search_returns(obj, depth + 1, max_depth))
      }
      return(NULL)
    }
    if (is.list(obj)) {
      nm <- names(obj)
      if (!is.null(nm) && "rets" %in% tolower(nm)) {
        rets_name <- nm[match("rets", tolower(nm))]
        res <- search_returns(obj[[rets_name]], depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      for (i in seq_along(obj)) {
        res <- search_returns(obj[[i]], depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      return(NULL)
    }
    ra <- attr(obj, "rets")
    if (!is.null(ra)) {
      res <- search_returns(ra, depth + 1, max_depth)
      if (!is.null(res)) {
        return(res)
      }
    }
    NULL
  }

  periods_per_year <- function(x) {
    if (!xts::is.xts(x)) stop("periods_per_year requires an xts object.")
    idx <- index(x)
    if (length(idx) < 2) {
      return(NA_real_)
    }
    p <- tryCatch(xts::periodicity(x)$scale, error = function(e) NA_character_)
    if (!is.na(p)) {
      p <- tolower(p)
      if (p == "daily") {
        w <- weekdays(as.Date(idx))
        frac_weekend <- mean(w %in% c("Saturday", "Sunday"))
        return(if (is.na(frac_weekend) || frac_weekend > 0.05) 365.25 else 252)
      } else if (p == "weekly") {
        return(52)
      } else if (p == "monthly") {
        return(12)
      } else if (p == "quarterly") {
        return(4)
      } else if (p == "yearly") {
        return(1)
      }
    }
    dt <- stats::median(diff(as.numeric(idx)))
    if (is.na(dt) || dt <= 0) {
      return(NA_real_)
    }
    if (inherits(idx, "Date")) {
      secs_per_period <- dt * 86400
    } else {
      secs_per_period <- dt
    }
    as.numeric((365.25 * 24 * 3600) / secs_per_period)
  }

  annualized_vol <- function(r, ppy) {
    rnum <- as.numeric(r)
    rnum <- rnum[is.finite(rnum)]
    if (length(rnum) < 2) {
      return(NA_real_)
    }
    stats::sd(rnum) * sqrt(ppy)
  }

  found <- search_returns(xts)
  if (is.null(found)) {
    stop("Could not locate 'Log' or 'Discrete' returns in the provided object.")
  }

  if (!is.null(found$log)) {
    r_log <- found$log
  } else if (!is.null(found$discrete)) {
    disc <- as.numeric(found$discrete)
    if (any(!is.finite(disc))) {
      warning("Non-finite values found in 'Discrete' returns; they will be kept as NA.")
    }
    if (any(disc <= -1, na.rm = TRUE)) {
      stop("Discrete returns contain values <= -1, cannot convert to log returns.")
    }
    r_log <- xts::xts(log1p(disc), order.by = index(found$discrete))
    colnames(r_log) <- "Log"
  } else {
    stop("Internal error: neither 'Log' nor 'Discrete' returns found after search.")
  }

  ppy <- periods_per_year(r_log)
  if (!is.finite(ppy) || ppy <= 0) {
    stop("Could not determine periods-per-year (annualization factor).")
  }

  vol_ann <- annualized_vol(r_log, ppy)
  if (!is.finite(vol_ann) || vol_ann <= 0) {
    stop("Realized volatility is zero or undefined; cannot normalize risk.")
  }

  target_vol_ann <- risk / 100
  scale_factor <- as.numeric(target_vol_ann / vol_ann)

  r_log_scaled <- r_log * scale_factor

  if (identical(type, "Log")) {
    out <- r_log_scaled
    colnames(out) <- "Log"
  } else {
    out <- xts::xts(exp(as.numeric(r_log_scaled)) - 1, order.by = index(r_log_scaled))
    colnames(out) <- "Discrete"
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
    if (!is.null(prep$rolling_corr) && .is_xts(prep$rolling_corr) && NCOL(prep$rolling_corr) > 0) {
      mods <- c(mods, "rolling_corr")
    }
  }
  if (!is.null(prep$costs_df) && is.data.frame(prep$costs_df) && NROW(prep$costs_df) > 0) {
    mods <- c(mods, "costs")
  }
  if (!is.null(prep$trade_quality_df) && is.data.frame(prep$trade_quality_df) && NROW(prep$trade_quality_df) > 0) {
    mods <- c(mods, "trade_quality")
  }
  # Candles/Volume available if we can standardize to OHLC (even from Close-only)
  if (!is.null(prep$mktdata) && .is_xts(prep$mktdata)) {
    std <- .to_ohlc_standard(prep$mktdata)
    if (!is.null(std)) {
      mods <- c(mods, "candles")
      if ("Volume" %in% colnames(std)) mods <- c(mods, "volume")
    }
  }
  # Position requires transactions
  if (!is.null(prep$trades) && .is_xts(prep$trades) && NROW(prep$trades) > 0) {
    mods <- c(mods, "position")
  }
  unique(mods)
}

#' Calculate rolling correlations for all selected return series
#' @param x An xts matrix of aligned discrete returns.
#' @param window Rolling window length. If NULL, a dynamic window is used.
#' @return An xts object with one column per pair, or NULL.
#' @keywords internal
.rolling_corr_calc <- function(x, window = NULL) {
  if (!.is_xts(x) || NCOL(x) < 2L || NROW(x) < 5L) {
    return(NULL)
  }
  vals <- coredata(x)
  finite_rows <- apply(is.finite(vals), 1, all)
  x <- x[finite_rows, , drop = FALSE]
  if (NROW(x) < 5L) {
    return(NULL)
  }
  vals <- coredata(x)
  n <- NROW(vals)
  k <- if (is.null(window)) max(20L, as.integer(round(n * 0.10))) else as.integer(window[1])
  k <- min(max(3L, k), max(3L, floor(n / 2)))
  if (n <= k) {
    return(NULL)
  }
  nms <- colnames(x)
  pairs <- utils::combn(nms, 2, simplify = FALSE)
  max_pairs <- getOption("tplot.rolling_corr.max_pairs", Inf)
  max_pairs <- suppressWarnings(as.numeric(max_pairs[1]))
  if (is.finite(max_pairs) && max_pairs > 0 && length(pairs) > max_pairs) {
    pairs <- pairs[seq_len(as.integer(max_pairs))]
  }
  roll_sum <- function(v) {
    cs <- c(0, cumsum(v))
    out <- rep(NA_real_, n)
    out[k:n] <- cs[(k + 1):(n + 1)] - cs[1:(n - k + 1)]
    out
  }
  out <- matrix(NA_real_, n, length(pairs))
  pair_names <- character(length(pairs))
  for (i in seq_along(pairs)) {
    pa <- pairs[[i]]
    a <- as.numeric(vals[, pa[1]])
    b <- as.numeric(vals[, pa[2]])
    sa <- roll_sum(a)
    sb <- roll_sum(b)
    saa <- roll_sum(a * a)
    sbb <- roll_sum(b * b)
    sab <- roll_sum(a * b)
    cov_ab <- (sab - (sa * sb / k)) / (k - 1)
    var_a <- (saa - (sa * sa / k)) / (k - 1)
    var_b <- (sbb - (sb * sb / k)) / (k - 1)
    corr <- cov_ab / sqrt(var_a * var_b)
    corr[!is.finite(corr)] <- NA_real_
    out[, i] <- pmax(pmin(corr, 1), -1)
    pair_names[i] <- paste(pa, collapse = " x ")
  }
  ans <- xts::xts(out, order.by = index(x))
  colnames(ans) <- pair_names
  attr(ans, "window") <- k
  ans
}

#' Force an xts index to midnight in a target timezone without shifting dates
#' @param x xts object to reindex
#' @param tz_target target timezone label (default 'America/Sao_Paulo')
#' @param tz_source assumed source timezone for index when deriving the Date (default 'UTC')
#' @return xts with index set to 00:00 in tz_target for each original Date
#' @keywords internal
.force_midnight_tz <- function(x, tz_target = "America/Sao_Paulo", tz_source = "UTC") {
  if (!.is_xts(x)) {
    return(x)
  }
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
.get_portfolio_returns <- function(portfolio, init, finit, tz_target = "America/Sao_Paulo") {
  if (is.null(portfolio) || !is.character(portfolio) || length(portfolio) != 1L) {
    return(NULL)
  }
  PortfReturns <- .get_function_if_exists("PortfReturns")
  if (is.null(PortfReturns)) {
    return(NULL)
  }
  rets <- tryCatch(PortfReturns(portfolio), error = function(e) NULL)
  if (!.is_xts(rets) || NROW(rets) == 0) {
    return(NULL)
  }
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
.uniquify_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(x)
  }
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
