## Backtest extra-data extraction helpers

#' @keywords internal
.bt_parse_number <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (is.numeric(x)) {
    v <- as.numeric(x[1])
    return(if (is.finite(v)) v else NA_real_)
  }
  txt <- as.character(x[1])
  if (is.na(txt) || !nzchar(txt)) {
    return(NA_real_)
  }
  txt <- trimws(txt)
  txt <- gsub("%", "", txt, fixed = TRUE)
  txt <- gsub("\\s+", "", txt)
  if (grepl(",", txt, fixed = TRUE)) {
    txt <- gsub(".", "", txt, fixed = TRUE)
    txt <- sub(",", ".", txt, fixed = TRUE)
  } else {
    txt <- gsub(",", "", txt, fixed = TRUE)
  }
  out <- suppressWarnings(as.numeric(txt))
  if (is.finite(out)) out else NA_real_
}

#' @keywords internal
.bt_first_number <- function(...) {
  vals <- list(...)
  for (v in vals) {
    x <- .bt_parse_number(v)
    if (is.finite(x)) {
      return(x)
    }
  }
  NA_real_
}

#' @keywords internal
.bt_extract_costs <- function(obj, symbol = NULL) {
  st <- obj$stats %||% obj$performance_stats %||% obj$raw_stats
  info_costs <- if (is.list(obj$info_blocks)) obj$info_blocks$costs else NULL
  info_value <- function(key) .bt_block_value(info_costs, key)

  fees <- .bt_first_number(.bt_stats_value(st, "fees"), info_value("Fees"))
  slippage <- .bt_first_number(.bt_stats_value(st, "slippage"), info_value("Slippage"))
  total_cost <- .bt_first_number(.bt_stats_value(st, "total_cost"), info_value("Total Cost"))
  if (!is.finite(total_cost) && is.finite(fees) && is.finite(slippage)) {
    total_cost <- fees + slippage
  }
  net_profit <- .bt_first_number(.bt_stats_value(st, "net_profit"), info_value("Net P/L"))
  gross_pnl <- .bt_first_number(info_value("Gross P/L"))
  if (!is.finite(gross_pnl) && is.finite(net_profit) && is.finite(total_cost)) {
    gross_pnl <- net_profit + total_cost
  }
  cost_impact <- .bt_first_number(info_value("Total Cost Impact"))
  if (!is.finite(cost_impact) && is.finite(total_cost) && is.finite(gross_pnl) && gross_pnl != 0) {
    cost_impact <- 100 * total_cost / abs(gross_pnl)
  }
  fees_impact <- .bt_first_number(info_value("Fees Impact"))
  slippage_impact <- .bt_first_number(info_value("Slippage Impact"))
  trades <- .bt_first_number(.bt_stats_value(st, "num_trades"), info_value("Trades"))
  contracts <- .bt_first_number(.bt_stats_value(st, "contracts_traded"), info_value("Contracts"))

  nums <- c(fees, slippage, total_cost, net_profit, gross_pnl, cost_impact, trades, contracts)
  if (!any(is.finite(nums))) {
    return(NULL)
  }
  data.frame(
    Asset = symbol %||% "Asset",
    Trades = trades,
    Contracts = contracts,
    GrossPnL = gross_pnl,
    NetPnL = net_profit,
    Fees = fees,
    Slippage = slippage,
    TotalCost = total_cost,
    FeesImpactPct = fees_impact,
    SlippageImpactPct = slippage_impact,
    CostImpactPct = cost_impact,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.bt_numeric_col <- function(df, name) {
  if (is.null(df) || !is.data.frame(df) || !(name %in% colnames(df))) {
    return(NULL)
  }
  suppressWarnings(as.numeric(df[[name]]))
}

#' @keywords internal
.bt_extract_trade_quality <- function(obj, symbol = NULL) {
  episodes <- obj$trade_episodes
  excursions <- obj$trade_excursions
  if (!is.data.frame(episodes) || NROW(episodes) == 0) {
    return(NULL)
  }
  asset <- symbol %||% "Asset"
  net <- .bt_numeric_col(episodes, "net_pnl")
  gross <- .bt_numeric_col(episodes, "gross_pnl")
  final_r <- .bt_numeric_col(episodes, "final_R")
  bars <- .bt_numeric_col(episodes, "bars_held")
  if (!is.null(excursions) && is.data.frame(excursions) && "final_R" %in% colnames(excursions)) {
    final_r_exc <- .bt_numeric_col(excursions, "final_R")
    if (!is.null(final_r_exc) && length(final_r_exc) == NROW(episodes)) {
      final_r <- final_r_exc
    }
  }
  score <- if (!is.null(net)) net else final_r
  wins <- if (!is.null(score)) score > 0 else rep(NA, NROW(episodes))
  gp <- if (!is.null(net)) sum(net[net > 0], na.rm = TRUE) else NA_real_
  gl <- if (!is.null(net)) abs(sum(net[net < 0], na.rm = TRUE)) else NA_real_
  profit_factor <- if (is.finite(gp) && is.finite(gl) && gl > 0) gp / gl else NA_real_
  mfe_r <- if (is.data.frame(excursions)) .bt_numeric_col(excursions, "mfe_R") else NULL
  mae_r <- if (is.data.frame(excursions)) .bt_numeric_col(excursions, "mae_R") else NULL

  summary <- data.frame(
    Asset = asset,
    Trades = NROW(episodes),
    WinRate = mean(wins, na.rm = TRUE) * 100,
    ProfitFactor = profit_factor,
    AvgR = mean(final_r, na.rm = TRUE),
    MedianR = stats::median(final_r, na.rm = TRUE),
    BestR = max(final_r, na.rm = TRUE),
    WorstR = min(final_r, na.rm = TRUE),
    MedianBars = stats::median(bars, na.rm = TRUE),
    AvgNetPnL = mean(net, na.rm = TRUE),
    MedianMFE_R = stats::median(mfe_r, na.rm = TRUE),
    MedianMAE_R = stats::median(mae_r, na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  for (nm in setdiff(colnames(summary), "Asset")) {
    bad <- !is.finite(summary[[nm]])
    summary[[nm]][bad] <- NA_real_
  }

  points <- NULL
  if (!is.null(mfe_r) && !is.null(final_r)) {
    n <- min(length(mfe_r), length(final_r), NROW(episodes))
    side <- if ("side" %in% colnames(episodes)) as.character(episodes$side[seq_len(n)]) else rep(NA_character_, n)
    tid <- if ("trade_id" %in% colnames(episodes)) episodes$trade_id[seq_len(n)] else seq_len(n)
    points <- data.frame(
      Asset = asset,
      TradeID = tid,
      Side = side,
      MFE_R = mfe_r[seq_len(n)],
      MAE_R = if (!is.null(mae_r)) mae_r[seq_len(n)] else NA_real_,
      FinalR = final_r[seq_len(n)],
      Bars = if (!is.null(bars)) bars[seq_len(n)] else NA_real_,
      NetPnL = if (!is.null(net)) net[seq_len(n)] else NA_real_,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    keep <- is.finite(points$MFE_R) & is.finite(points$FinalR)
    points <- points[keep, , drop = FALSE]
    if (NROW(points) == 0) {
      points <- NULL
    }
  }

  list(summary = summary, points = points)
}

#' @keywords internal
.bt_extract_extras <- function(obj, symbol = NULL) {
  tq <- .bt_extract_trade_quality(obj, symbol = symbol)
  list(
    costs = .bt_extract_costs(obj, symbol = symbol),
    trade_quality = if (!is.null(tq)) tq$summary else NULL,
    trade_quality_points = if (!is.null(tq)) tq$points else NULL
  )
}

#' @keywords internal
.bt_bind_extra <- function(extras, field) {
  parts <- lapply(extras, function(x) {
    if (is.list(x)) x[[field]] else NULL
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    return(NULL)
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}
