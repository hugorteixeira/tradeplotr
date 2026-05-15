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
