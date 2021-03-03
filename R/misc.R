require_suggested_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install the ", pkg, " package.", call. = FALSE)
  }
}

is.family <- function(x) {
  inherits(x, "family")
}

#' Check if family is binomial/bernoulli
#' @noRd
#' @param x User's specified `family` argument.
#' @return `TRUE` or `FALSE` if no error.
family_is_binomial <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (!is.character(x) && !is.family(x)) {
    stop("Model family must be a string or family object", call. = FALSE)
  }
  if (is.family(x)) {
    x <- x$family
  }
  x %in% c("binomial", "bernoulli")
}
