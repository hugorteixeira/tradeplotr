#' Converts the statistics data.frame to a JSON-ready list
#' @param carteira_df The statistics data.frame.
#' @return A list ready to be converted to JSON.
#' @keywords internal
stats_json <- function(carteira_df) {
  lapply(seq_len(nrow(carteira_df)), function(i) {
    row <- as.list(carteira_df[i, , drop = FALSE])
    row <- lapply(row, function(x) {
      if (is.character(x) && !is.na(num <- suppressWarnings(as.numeric(x)))) {
        num
      } else {
        x
      }
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
series_json <- function(mat, datas) {
  dates <- as.character(
    as.Date(as.POSIXct(datas / 1000, origin = "1970-01-01"))
  )
  df <- data.frame(
    Date = dates, as.data.frame(mat, stringsAsFactors = FALSE),
    check.names = FALSE
  )
  idx <- unique(as.integer(round(seq(1, nrow(df), length.out = min(11, nrow(df))))))
  lapply(idx, function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) {
      if (is.character(x) && !is.na(num <- suppressWarnings(as.numeric(x)))) {
        num
      } else {
        x
      }
    })
    row
  })
}

#' Converts the list of returns tables to a JSON-ready list
#' @param lista_tabs The list of returns tables.
#' @return A list ready to be converted to JSON.
#' @keywords internal
rentab_json <- function(lista_tabs) {
  out <- list()
  for (name in names(lista_tabs)) {
    tab <- lista_tabs[[name]]
    df2 <- cbind(
      Year = rownames(tab),
      as.data.frame(tab, stringsAsFactors = FALSE)
    )
    rows <- lapply(seq_len(nrow(df2)), function(i) {
      r <- as.list(df2[i, , drop = FALSE])
      r <- lapply(r, function(x) {
        if (is.character(x) && grepl("%", x)) {
          as.numeric(gsub("%", "", x))
        } else if (is.character(x) && !is.na(num <- suppressWarnings(as.numeric(x)))) {
          num
        } else {
          x
        }
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
    x <- lapply(x, fix_pkg)
    attributes(x) <- at
  }
  x
}
