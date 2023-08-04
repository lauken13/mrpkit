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
family_is_binomial_or_bernoulli <- function(x) {
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

#' Figure out which package was used to fit the model
#' @noRd
#' @param fit Fitted model object (not the SurveyFit object)
detect_fitting_package <- function(fit) {
  switch(
    class(fit)[1],
    "brmsfit" = "brms",
    "stanreg" = "rstanarm",
    "glmerMod" = "lme4",
    "custom"
  )
}

is_brms_binomial <- function(fit) {
  detect_fitting_package(fit) == "brms" && family(fit)$family == "binomial"
}
is_rstanarm_binomial <- function(fit) {
  detect_fitting_package(fit) == "rstanarm" &&
    family(fit)$family == "binomial" &&
    grepl("^cbind\\(", as.character(formula(fit))[[2]])
}
is_lme4_binomial <- function(fit) {
  detect_fitting_package(fit) == "lme4" &&
    family(fit)$family == "binomial" &&
    grepl("^cbind\\(", as.character(formula(fit))[[2]])
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

