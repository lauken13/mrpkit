#' SurveyFit
#'
#' @name SurveyFit
#' @export
#' @description An [R6][R6::R6Class] SurveyFit object stores a fitted model
#'   object and provides methods for generating predicted probabilities for all
#'   poststrat cells, generating population and group estimates, and visualizing
#'   results.
#' @inherit SurveyMap examples
SurveyFit <- R6::R6Class(
  classname = "SurveyFit",
  private = list(
    map_ = NULL,
    fit_ = NULL
  ),
  public = list(
    initialize = function(fit, map) {
      private$fit_ <- fit
      private$map_ <- map
      invisible(self)
    },

    #' @description Access the fitted model object
    #' @return The fitted model object.
    fit = function() {
      private$fit_
    },

    #' @description Access the SurveyMap object
    #' @return The [SurveyMap] object.
    map = function() {
      private$map_
    },

    #' @description Call the fitted model object's print method
    #' @param ... Optional arguments to pass the print method.
    print = function(...) {
      print(private$fit_, ...)
      invisible(self)
    },

    #' @description Use fitted model to add predicted probabilities to post-stratification dataset.
    #' @param fun The function to use to generate the predicted probabilities.
    #'   This should only be specified if using a custom function, otherwise for
    #'   \pkg{rstanarm} and \pkg{brms} models `posterior_epred()` is
    #'   automatically used (with the result transposed) and for \pkg{lme4}
    #'   models the [sim_posterior_epred()] is used. If `fun` is a custom
    #'   function then the first argument should take in the fitted model object
    #'   and the second argument should take in the poststratification
    #'   (`newdata`) data frame. The function must return a matrix with rows
    #'   corresponding to the columns of the poststratification data and columns
    #'   corresponding to simulations.
    #' @param ... Arguments other than the fitted model and `newdata` data frame
    #'   to pass to `fun`.
    #' @return A matrix with rows corresponding to poststrat cells and columns
    #'   corresponding to posterior samples.
    #'
    predictify = function(fun = NULL, ...) {
      args <- list(...)
      if (!is.null(args$newdata)) {
        stop("The 'newdata' argument should not be specified.",
             call. = FALSE)
      }
      if (is.null(private$map_$poststrat_data())) {
        stop("Post-stratification data not found. ",
             "Please call the tabulate() method before fitting a model.",
             call. = FALSE)
      }
      poststrat <- private$map_$poststrat_data()

      if (is.null(args$fun)) {
        if ("stanreg" %in% class(private$fit_)){
          require_suggested_package("rstanarm", "2.21.0")
          return(
            t(rstanarm::posterior_epred(
              object = private$fit_,
              newdata = poststrat,
              ...
            ))
          )
        }
        if ("brmsfit" %in% class(private$fit_)){
          require_suggested_package("brms")
          return(
            t(brms::posterior_epred(
              object = private$fit_,
              newdata = poststrat,
              allow_new_levels = TRUE,
              sample_new_levels =
                if (!is.null(args$sample_new_levels)) args$sample_new_levels
              else "gaussian",
              ...
            ))
          )
        }
        if ("glmerMod" %in% class(private$fit_)) {
          require_suggested_package("lme4")
          return(
            sim_posterior_epred(
              object = private$fit_,
              newdata = poststrat,
              ...
            )
          )
        }
      } else {
        fun <- match.fun(fun)
        fun(fitted_model, poststrat, ...)
      }
    },
    aggregate = function(poststrat_fit, by = NULL) {
      poststrat <- private$map_$poststrat_data()
      if (!is.null(by)) {
        if (length(by) != 1) {
          stop("Currently only one variable can be named in 'by'.", call. = FALSE)
        }
        rotate_levels <- levels(private$map_$samp_obj()$mapped_data()[, by])
        posterior_preds <- expand.grid(by = rotate_levels, draw = 1:ncol(poststrat_fit), value = NA)
        colnames(posterior_preds)[1] <- by
        for (focus_level in rotate_levels){
          level_loc <- poststrat[by] == focus_level
          posterior_preds[posterior_preds[by] == focus_level, "value"] <-
            apply(poststrat_fit[level_loc, ], 2, function(x) sum(poststrat$N_j[level_loc]*x)/sum(poststrat$N_j[level_loc]))
        }
      } else {
        posterior_preds <-
          data.frame(value = apply(poststrat_fit, 2, function(x) sum(poststrat$N_j*x)/sum(poststrat$N_j)))
      }
      return(posterior_preds)
    },
    visify = function(sae_preds, weights = TRUE) {
      if (dim(sae_preds)[2] > 2){
        focus_var <- colnames(sae_preds)[1]
        which_q <- private$map_$item_map()[[focus_var]]$col_names()[1]
        svy_q <- private$map_$samp_obj()$questions()[[which_q]]
        gg <- ggplot2::ggplot(sae_preds) +
          ggplot2::aes(x = .data[[focus_var]], y = .data[["value"]]) +
          ggplot2::geom_violin(fill = "darkblue", alpha = .3) +
          ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
          ggplot2::xlab(svy_q)
      } else {
        model_fit <- private$fit_
        lhs_var <- as.character(formula(model_fit))[[2]]
        svy_q <- private$map_$samp_obj()$questions()[[lhs_var]]
        gg <- ggplot2::ggplot(sae_preds) +
          ggplot2::aes(x = .data[["value"]], y = ggplot2::after_stat(scaled)) +
          ggplot2::geom_density(fill = "darkblue", alpha = .3, ) +
          ggplot2::scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
          ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
          ggplot2::xlab(svy_q)
      }

      if (weights) {
        model_fit <- private$fit_
        lhs_var <- as.character(formula(model_fit))[[2]]
        if (dim(sae_preds)[2] > 2) {
          by_var <- colnames(sae_preds)[1]
          wtd_ests <- create_wtd_ests(private, lhs_var, by=by_var)
          gg <- gg +
            ggplot2::geom_point(data = wtd_ests, ggplot2::aes(x= .data[[by_var]], y = .data[["mean"]])) +
            ggplot2::geom_errorbar(
              data = wtd_ests,
              ggplot2::aes(x = .data[[by_var]],
                           ymin = .data[["mean"]] - 1.96*.data[["std"]],
                           ymax = .data[["mean"]] + 1.96*.data[["std"]]),
              inherit.aes = FALSE, alpha = .5)
        } else {
          wtd_ests <- create_wtd_ests(private, lhs_var)
          gg <- gg +
            ggplot2::geom_vline(data = wtd_ests, ggplot2::aes(xintercept = .data[["mean"]])) +
            ggplot2::annotate("rect",
              xmin = wtd_ests$mean - 1.96*wtd_ests$std, xmax = wtd_ests$mean + 1.96*wtd_ests$std,
              ymin = 0, ymax = 1,
              alpha = .5, fill = "grey"
            )
        }
      }
      return(gg)
    }
  )
)
