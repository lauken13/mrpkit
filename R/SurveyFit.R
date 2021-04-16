#' SurveyFit
#'
#' @name SurveyFit
#' @export
#' @description An [R6][R6::R6Class] SurveyFit object stores a fitted model
#'   object and provides methods for generating predicted probabilities for all
#'   poststrat cells, generating population and group estimates, and visualizing
#'   results.
#'
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
      if (!is.null(by)){
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
    visify = function(sae_preds) {
      if (dim(sae_preds)[2]>1){
        svy_q <- private$map_$samp_obj()$questions()[colnames(private$map_$samp_obj()$survey_data()) == private$map_$item_map()[[colnames(sae_preds)[1]]]$col_names()[1]]
        focus_var <- dplyr::sym(colnames(sae_preds)[1])
        ggplot2::ggplot(sae_preds, ggplot2::aes(x = !!focus_var, y = value))+
          ggplot2::geom_violin(fill = "darkblue", alpha = .3) +
          ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0, 0))+
          ggplot2::xlab(svy_q)
      } else {
        ggplot2::ggplot(sae_preds, ggplot2::aes(x = value))+
          ggplot2::geom_density(fill = "darkblue", alpha = .3) +
          ggplot2::scale_x_continuous(limits = c(0,1), expand = c(0, 0))
      }
    }
  )
)
