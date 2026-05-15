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
  basis_date <- as.Date(basis_date)
  if (basis_date < rule_change_date) {
    # Old rule (pre-change): 0–3m: 0.001; 3–60m: 0.005; >60m: 0.010
    if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
  } else {
    # New rule (post-change): 0–3m: 0.001; >3m: 0.005 (no 0.010 tier)
    if (mm <= 3) 0.001 else 0.005
  }
}

.calculate_futures_di_rates <- function(
  pu,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-01")
) {
  # 0) Calendar
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::time_length(lubridate::interval(basis_date, md), "month")
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    md <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(md, "try-error") || is.na(md)) {
      stop("'maturity_date' must be Date, a number of business days, or coercible to Date.")
    }
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::time_length(lubridate::interval(basis_date, md), "month")
  }

  if (n <= 0) stop("Number of business days to maturity (n) must be positive.")

  # 2) Tick-size (depends on regime at basis_date)
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # 3) Rate from PU
  pu <- as.numeric(pu)
  if (any(!is.finite(pu) | pu <= 0)) stop("'pu' must be positive and finite.")

  rates <- 100 * ((1e5 / pu)^(252 / n) - 1) # percent

  # 4) Tick-value (magnitude), using dPU/d(rate in percentage points)
  # dPU/d(r%) = -(n/252) * PU / (100 * (1 + r%/100))
  deriv_pp <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    rates      = as.numeric(rates), # percent
    tick_size  = tick_size, # percent points per tick
    tick_value = as.numeric(tick_value) # PU points per tick
  )
}

.calculate_futures_di_notional <- function(
  rates,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-01")
) {
  # 0) Calendar
  if (is.null(cal)) {
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::time_length(lubridate::interval(basis_date, md), "month")
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    stop("'maturity_date' must be a number of business days or Date.")
  }

  if (n <= 0) stop("Number of business days to maturity (n) must be positive.")

  # 2) Tick-size (depends on regime at basis_date)
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # 3) PU from rate
  rates <- as.numeric(rates)
  if (any(!is.finite(rates) | rates <= -100)) stop("'rates' must be finite and > -100%.")
  pu <- 1e5 / (1 + rates / 100)^(n / 252)

  # 4) Tick-value (magnitude)
  deriv_pp <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    pu         = as.numeric(pu), # PU
    tick_size  = tick_size, # percent points per tick
    tick_value = as.numeric(tick_value) # PU points per tick
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
