#' Checks if market data is for brazilian DI Futures
#' @param mkt The xts object with market data.
#' @return Logical.
#' @keywords internal
.isDI <- function(mkt) {
  if (is.null(mkt)) {
    return(FALSE)
  }
  cols <- tolower(colnames(mkt))
  all(c("tickvalue", "ticksize") %in% cols) &&
    (any(c("pu_o", "pu_open") %in% cols))
}

.get_di_tick_size <- function(mm, basis_date, rule_change_date = as.Date("2025-08-01")) {
  positionsizer::ps_di_tick_size(mm, basis_date = basis_date, rule_change_date = rule_change_date)
}

.calculate_futures_di_rates <- function(
  pu,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-01")
) {
  positionsizer::ps_di_pu_to_rate(
    pu = pu,
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    snap_to_tick = FALSE,
    rule_change_date = rule_change_date
  )
}

.calculate_futures_di_notional <- function(
  rates,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-01")
) {
  positionsizer::ps_di_rate_to_pu(
    rates = rates,
    maturity_date = maturity_date,
    basis_date = basis_date,
    cal = cal,
    snap_to_tick = FALSE,
    round_pu = FALSE,
    rule_change_date = rule_change_date
  )
}
#' Calculates the rate from the PU of a brazilian DI futures contract
#' @param price The PU (unit price) of the contract.
#' @param row_date The date of the calculation.
#' @param maturity The maturity date of the contract.
#' @return The numeric rate.
#' @importFrom bizdays bizdays create.calendar holidays
#' @keywords internal
get_DI_price <- function(price, row_date, maturity) {
  res <- tryCatch(.calculate_futures_di_rates(pu = price, maturity, row_date), error = function(e) NULL)
  if (!is.null(res) && !is.null(res$rates)) {
    return(as.numeric(res$rates))
  }
  # fallback: if helper not available, assume 'price' already is a numeric rate
  # as.numeric(price)
}
