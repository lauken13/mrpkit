#' Generate approximate samples of predicted probabilities from a binomial
#' `glmerMod` object.
#'
#' @export
#' @param object A binomial model fit using `lme4::glmer()`.
#' @param newdata The data to use.
#' @param nsamples Number of samples to generate.
#' @return A matrix with `nrow(newdata)` rows and `nsamples` columns.
#'
sim_posterior_epred  <- function(object, newdata, nsamples = 4000) {
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
      newdata = newdata,
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
