require_suggested_package <- function(pkg, ver = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE) ||
      (!is.null(ver) && utils::packageVersion(pkg) < ver)) {
    stop("Please install ",
         if (!is.null(ver)) paste("at least version", ver, "of "),
         "the ", pkg, " package.", call. = FALSE)
  }
}

is_family <- function(x) {
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
  if (!is.character(x) && !is_family(x)) {
    stop("Model family must be a string or family object", call. = FALSE)
  }
  if (is_family(x)) {
    x <- x$family
  }
  x %in% c("binomial", "bernoulli")
}



#' Generate approximate samples of predicted probabilities from a binomial
#' `glmerMod` object.
#'
#' @export
#' @keywords internal
#' @param object A binomial model fit using `lme4::glmer()`.
#' @param newdata The data to use.
#' @param nsamples Number of samples to generate.
#' @return A matrix with `nrow(newdata)` rows and `nsamples` columns.
#'
sim_posterior_probs  <- function(object, newdata, nsamples = 4000) {
  require_suggested_package("merTools")
  if (!inherits(object, "glmerMod")) {
    stop("Object must have class 'glmerMod'.", call. = FALSE)
  }
  if (stats::family(object)$family != "binomial") {
    stop("Model family must be binomial.", call. = FALSE)
  }
  predict_nd <-
    merTools::predictInterval(
      merMod = object,
      newdata = as.data.frame(newdata),
      level = 0.95,
      n.sims = nsamples,
      stat = "median",
      type = "probability",
      include.resid.var = TRUE,
      returnSims = TRUE,
      .parallel = FALSE
    )
  stats::plogis(attr(predict_nd, "sim.results"))
}

