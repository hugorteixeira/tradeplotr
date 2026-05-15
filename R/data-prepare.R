#' Prepares raw data by calculating returns
#' @param data_xts A list of xts objects.
#' @return A single xts object with the calculated returns for all series.
#' @keywords internal
.data_prepare <- function(data_xts, verbose = getOption("tplot.verbose", FALSE)) {
  vmsg <- function(...) {
    if (isTRUE(verbose)) message(...)
  }
  vmsg(sprintf(
    "[.data_prepare] Input items: %d | names=%s",
    if (is.list(data_xts)) length(data_xts) else 1L,
    paste(if (is.list(data_xts)) names(data_xts) else "<single>", collapse = ",")
  ))
  if (xts::is.xts(data_xts)) data_xts <- list(data_xts)
  if (is.null(names(data_xts))) names(data_xts) <- paste0("Serie", seq_along(data_xts))

  # Accumulate raw selected price/level columns per series,
  # then merge once to avoid edge cases with cbind on empty xts
  ativos_data <- NULL
  cols_accum <- list()
  col_names <- character(0)
  use_discrete <- logical(0)
  per_scales <- character(0) # coarse scale from xts::periodicity
  per_labels <- character(0) # human-friendly bar size (e.g., 1h, 4h, 1d)

  # Infer robust periodicity without calling xts::periodicity (avoid segfaults)
  .infer_period_info <- function(x_index) {
    # x_index can be an xts object or a POSIXct/Date vector
    idx <- x_index
    if (xts::is.xts(x_index)) idx <- index(x_index)
    # Default
    out <- list(scale = "daily", label = "1d", key = 3L)
    if (inherits(idx, "Date")) {
      return(out)
    }
    # Try POSIXct
    if (inherits(idx, "POSIXct") || inherits(idx, "POSIXt")) {
      secs <- suppressWarnings(as.numeric(idx))
      d <- suppressWarnings(stats::median(diff(secs), na.rm = TRUE))
      if (!is.finite(d) || is.na(d) || d <= 0) {
        return(out)
      }
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

  find_col_idx <- function(cols, base_names) {
    cols_l <- tolower(cols)
    for (nm in base_names) {
      idx <- grep(paste0("(^|\\.)", tolower(nm), "$"), cols_l)
      if (length(idx) > 0) {
        return(idx[1])
      }
    }
    NA_integer_
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
    idx_close <- find_col_idx(cols, "Close")
    # Se a função aceita um vetor de possíveis nomes
    idx_pu_close <- find_col_idx(cols, c("PU_close", "PU_c"))
    idx_adjusted <- find_col_idx(cols, "Adjusted")
    idx_discrete <- find_col_idx(cols, "Discrete")

    chosen_idx <- NA_integer_
    chosen_msg <- ""
    chosen_is_discrete <- FALSE

    if (grepl("CDI", item_name, ignore.case = TRUE) && !is.na(idx_close)) {
      chosen_idx <- idx_close
      chosen_msg <- "\n        Close (calculated)\n"
    } else if (grepl("IPCA", item_name, ignore.case = TRUE) && !is.na(idx_close)) {
      chosen_idx <- idx_close
      chosen_msg <- "\n        Close (calculated)\n"
    } else if (grepl("DI1", item_name, ignore.case = TRUE) && !is.na(idx_pu_close)) {
      chosen_idx <- idx_pu_close
      chosen_msg <- "\n        Close DI (calculated)\n"
    } else if (!is.na(idx_adjusted)) {
      chosen_idx <- idx_adjusted
      chosen_msg <- "\n        Adjusted\n"
    } else if (!is.na(idx_close)) {
      chosen_idx <- idx_close
      chosen_msg <- "\n        Close\n"
    } else if (!is.na(idx_discrete)) {
      chosen_idx <- idx_discrete
      chosen_msg <- "\n        Discrete (no recalc)\n"
      chosen_is_discrete <- TRUE
    } else if (NCOL(item) == 1) {
      # Fallback: use the single available column as price/level
      chosen_idx <- 1L
      chosen_msg <- "\n        (fallback) first row\n"
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

    vmsg(sprintf(
      "[.data_prepare] Added column for %s | rows=%d | periodicity=%s | label=%s",
      item_name, NROW(discrete_col), per, bar_lbl
    ))
    # Ensure the selected column carries the series name now
    colnames(discrete_col) <- item_name
    cols_accum[[length(cols_accum) + 1L]] <- discrete_col
    col_names <- c(col_names, item_name)
    use_discrete <- c(use_discrete, chosen_is_discrete)
    per_scales <- c(per_scales, per)
    per_labels <- c(per_labels, bar_lbl)
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
            0, ativos_data[first_non_na:NROW(ativos_data), i]
          )
      }
    } else {
      ativos_data[, i] <- zoo::na.locf(ativos_data[, i], na.rm = FALSE)
      ativos_data[, i] <- zoo::na.approx(ativos_data[, i], na.rm = FALSE)
    }
  }

  ativos_data_returns <- ativos_data
  if (!exists("col_names")) col_names <- colnames(ativos_data)

  for (i in seq_len(NCOL(ativos_data))) {
    # Extract the single-column series (preserve xts/data.frame structure)
    x_col <- ativos_data[, i, drop = FALSE]
    # Flatten values to numeric vector for validation checks (ignoring index)
    x_vals <- as.numeric(x_col)
    # Finite mask to ignore NA/NaN/Inf in the checks
    fin <- is.finite(x_vals)

    # Content checks (ignore NAs/Inf):
    # - have at least one finite value
    have_data <- any(fin)
    # - all finite values are strictly positive (> 0) to avoid 1/0 issues
    positive_ok <- have_data && all(x_vals[fin] > 0)
    # - at least 5 digits before the decimal point for all finite values
    #   (e.g., 67890.00 => trunc(67890.00) = 67890 >= 10000)
    five_digits_ok <- have_data && all(trunc(x_vals[fin]) >= 10000)

    if (!use_discrete[i]) {
      # Only consider inversion when the column is NOT already a discrete return
      prefix_ok <- startsWith(col_names[i], "DI1")
      invert_for_DI1 <- prefix_ok && positive_ok && five_digits_ok

      # Use 1/x for DI1 columns meeting the criteria; otherwise use x as-is
      r_input <- if (invert_for_DI1) 1 / x_col else x_col

      # Compute discrete returns
      r_i <- PerformanceAnalytics::Return.calculate(r_input, method = "discrete")

      # Set the first non-NA return in this column to 0 (do not touch other columns)
      if (NROW(r_i) > 0) {
        fi <- which(!is.na(r_i[, 1]))
        if (length(fi)) r_i[fi[1], 1] <- 0
      }
      ativos_data_returns[, i] <- r_i

      # Optional log
      vmsg(sprintf(
        "[.data_prepare] Return.calculate(%s%s) -> rows=%d",
        col_names[i],
        if (invert_for_DI1) " [1/x]" else "",
        NROW(r_i)
      ))
    } else {
      # Column already represents discrete returns: only set first valid to 0
      r_i <- x_col
      fi <- which(!is.na(r_i[, 1]))
      if (length(fi)) r_i[fi[1], 1] <- 0
      ativos_data_returns[, i] <- r_i
    }
  }
  # ---- Align returns to a common, safe periodicity ----
  # Map periodicity to an ordered rank and endpoint label
  .per_key <- function(scale) {
    s <- tolower(scale %||% "unknown")
    if (startsWith(s, "min")) {
      return(1L)
    } # minute
    if (s %in% c("hourly", "hours", "hour")) {
      return(2L)
    }
    if (s %in% c("daily", "day", "days")) {
      return(3L)
    }
    if (s %in% c("weekly", "week", "weeks")) {
      return(4L)
    }
    if (s %in% c("monthly", "month", "months")) {
      return(5L)
    }
    if (s %in% c("quarterly", "quarter", "quarters")) {
      return(6L)
    }
    if (s %in% c("yearly", "annual", "year", "years")) {
      return(7L)
    }
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
      "days"
    )
  }
  .pretty_scale <- function(on) {
    switch(on,
      minutes = "Minutes",
      hours = "Hours",
      days = "Daily",
      weeks = "Weekly",
      months = "Monthly",
      quarters = "Quarterly",
      years = "Annual",
      on
    )
  }
  .normalize_index <- function(x, on) {
    # Normalize index to comparable Date stamps across different sources/markets.
    # This avoids mismatches due to different times-of-day when merging.
    if (on %in% c("days", "day")) {
      return(as.Date(x))
    }
    if (on %in% c("weeks", "week")) {
      return(as.Date(x))
    }
    if (on %in% c("months", "month")) {
      return(as.Date(zoo::as.yearmon(x), frac = 1))
    }
    if (on %in% c("quarters", "quarter")) {
      return(as.Date(zoo::as.yearqtr(x), frac = 1))
    }
    if (on %in% c("years", "year")) {
      yy <- format(as.POSIXct(x), "%Y")
      return(as.Date(zoo::as.yearmon(paste0(yy, "-12")), frac = 1))
    }
    as.Date(x)
  }

  .agg_returns <- function(r, on) {
    # r is an xts vector of returns; aggregate by compounding within each period
    if (is.null(r) || NROW(r) == 0) {
      return(r)
    }
    idx <- tryCatch(xts::endpoints(r, on = on), error = function(e) integer(0))
    if (length(idx) <= 1) {
      return(r)
    }
    out_vals <- vector("numeric", length(idx) - 1L)
    out_idx <- index(r)[idx[-1]]
    for (j in seq_len(length(idx) - 1L)) {
      seg <- r[(idx[j] + 1L):idx[j + 1L], , drop = FALSE]
      rr <- as.numeric(seg)
      rr <- rr[is.finite(rr)]
      out_vals[j] <- if (length(rr)) exp(sum(log1p(rr))) - 1 else NA_real_
    }
    out_idx_n <- .normalize_index(out_idx, on)
    xts::xts(out_vals, order.by = out_idx_n)
  }

  # Determine the target (coarsest) periodicity among series; ensure at least 'days'
  keys <- vapply(per_scales, .per_key, integer(1))
  # coarsest = max(key); enforce minimum = 3 (days)
  target_key <- max(c(3L, keys), na.rm = TRUE)
  target_on <- .on_from_key(target_key)

  # Build user-friendly message if alignment is needed or if intraday found
  det <- paste(sprintf("%s: %s", col_names, per_labels), collapse = ", ")
  if (any(keys < 3L)) {
    message(sprintf(
      "[.data_prepare] Detected (s) with intraday data (%s). Changing timeframe %s for compatibility.",
      paste(col_names[keys < 3L], collapse = ", "), .pretty_scale("days")
    ))
  }
  if (any(keys != target_key)) {
    message(sprintf(
      "[.data_prepare] Fixing periodicity of returns -> %s | Detected: %s",
      .pretty_scale(target_on), det
    ))
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
    vmsg(sprintf(
      "[.data_prepare] Aggregated to %s | rows=%d | cols=%d",
      target_on, NROW(ativos_data_returns), NCOL(ativos_data_returns)
    ))
  }

  # Ensure column names exist and match column count
  target_nms <- if (length(col_names) >= NCOL(ativos_data_returns)) {
    col_names[seq_len(NCOL(ativos_data_returns))]
  } else {
    paste0("Serie", seq_len(NCOL(ativos_data_returns)))
  }
  colnames(ativos_data_returns) <- target_nms
  vmsg(sprintf(
    "[.data_prepare] Final prepared | rows=%d | cols=%d | names=%s",
    NROW(ativos_data_returns), NCOL(ativos_data_returns), paste(target_nms, collapse = ",")
  ))
  # Final safety: ensure xts type
  if (!xts::is.xts(ativos_data_returns) && inherits(ativos_data_returns, "zoo")) {
    ativos_data_returns <- xts::as.xts(ativos_data_returns)
  }
  vmsg(sprintf(
    "[.data_prepare] Returning class=%s | rows=%d | cols=%d",
    paste(class(ativos_data_returns), collapse = ","),
    NROW(ativos_data_returns), NCOL(ativos_data_returns)
  ))
  return(ativos_data_returns)
}
