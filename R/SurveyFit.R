#' SurveyFit
#'
#' @name SurveyFit
#' @export
#' @description An [R6][R6::R6Class] `SurveyFit` object stores a fitted model
#'   object and provides methods for generating predicted probabilities for all
#'   poststratification cells, generating population and group estimates, and
#'   visualizing results.
#' @inherit SurveyMap examples
#'
SurveyFit <- R6::R6Class(
  classname = "SurveyFit",
  private = list(
    map_ = NULL,
    fit_ = NULL,
    formula_ = NULL
  ),
  public = list(

    #' @description Create a new `SurveyFit` object. This method is called
    #'   internally by the `fit` method of the [`SurveyMap`] object and does
    #'   not need to be called directly by the user.
    #' @param fit A fitted model object.
    #' @param map A [`SurveyMap`] object.
    #' @param formula A formula object for the model that was fit.
    #' @return A `SurveyFit` object.
    initialize = function(fit, map, formula) {
      private$fit_ <- fit
      private$map_ <- map
      private$formula_ <- formula
      invisible(self)
    },

    #' @description Access the fitted model object.
    #' @return The fitted model object created by the modeling function called
    #'   by the `fit` method of the [`SurveyMap`] object. For example, if using
    #'   `rstanarm::stan_glmer()` then a `stanreg` object from \pkg{rstanarm} is
    #'   returned.
    fit = function() {
      private$fit_
    },

    #' @description Access the [`SurveyMap`] object.
    #' @return The [`SurveyMap`] associated with the `SurveyFit` object.
    map = function() {
      private$map_
    },

    #' @description Access the model formula.
    #' @return The model formula used when fitting the model.
    formula = function() {
      private$formula_
    },

    #' @description Call the fitted model object's print method. The console
    #'   output from this method depends on the model fitting function used.
    #' @param ... Optional arguments to pass the print method.
    #' @return The `SurveyFit` object, invisibly.
    print = function(...) {
      print(private$fit_, ...)
      invisible(self)
    },

    #' @description Use fitted model to add predicted probabilities to
    #'   post-stratification dataset.
    #' @param fun The function to use to generate the predicted probabilities.
    #'   This should only be specified if you used a model fitting function
    #'   not natively supported by \pkg{mrpkit}.
    #'   For models fit using \pkg{rstanarm}, \pkg{brms}, or \pkg{lme4}, `fun`
    #'   is handled automatically. If `fun` is specified then:
    #'   * the first argument should be the fitted model object
    #'   * the second argument should be the poststratification data frame
    #'   * it can take an arbitrary number of other arguments
    #'   * the returned object should match the specifications in the 'Returns'
    #'    section below in order to be compatible with subsequent methods
    #' @param ... Arguments other than the fitted model object and
    #'   poststratification data frame to pass to `fun`.
    #' @return A matrix with rows corresponding to poststratification cells and
    #'   columns corresponding to posterior samples (or approximate ones
    #'   in the case of \pkg{lme4} models).
    population_predict = function(..., fun = NULL) {
      args <- list(...)
      if (!is.null(args$newdata) && is.null(fun)) {
        stop("The 'newdata' argument should not be specified.",
             call. = FALSE)
      }
      poststrat <- private$map_$poststrat_data()

      if (is.null(fun)) {
        if ("stanreg" %in% class(private$fit_)) {
          require_suggested_package("rstanarm", "2.21.0")
          return(
            t(suppressMessages(rstanarm::posterior_linpred(
              object = private$fit_,
              newdata = poststrat,
              transform = TRUE,
              ...
            )))
          )
        } else if ("brmsfit" %in% class(private$fit_)) {
          require_suggested_package("brms")
          return(
            t(brms::posterior_epred(
              object = private$fit_,
              newdata = poststrat,
              dpar = "mu",
              allow_new_levels = TRUE,
              sample_new_levels =
                if (!is.null(args$sample_new_levels)) args$sample_new_levels
                else "gaussian",
              ...
            ))
          )
        } else if ("glmerMod" %in% class(private$fit_)) {
          require_suggested_package("lme4")
          return(
            sim_posterior_probs(
              object = private$fit_,
              newdata = poststrat,
              ...
            )
          )
        } else {
          stop("Custom population_predict method required. Please specifiy 'fun'.",
               call. = FALSE)
        }
      } else {
        fun <- match.fun(fun)
        fun(private$fit_, poststrat, ...)
      }
    },

    #' @description Aggregate estimates to the population level or by level of a
    #'   grouping variable.
    #' @param poststrat_estimates The object returned by the `population_predict` method.
    #' @param by Optionally a string specifying a grouping variable. If
    #'   specified the aggregation will happen by level of the named variable.
    #'   If not specified population-level estimates will be computed.
    #' @return A data frame. If `by` is not specified then the data frame will
    #'   have number of rows equal to the number of posterior draws. If `by` is
    #'   specified the data frame will have number of rows equal to the number
    #'   of posterior draws times the number of levels of the `by` variable,
    #'   and there will be an extra column indicating which level of the `by`
    #'   variable each row corresponds to.
    aggregate = function(poststrat_estimates, by = NULL) {
      poststrat_data <- private$map_$poststrat_data()
      if (!is.null(by)) {
        if (length(by) != 1) {
          stop("Currently only one variable can be named in 'by'.", call. = FALSE)
        }
        ps_and_posterior <- cbind(poststrat_data, poststrat_estimates)
        out <- ps_and_posterior %>%
          tidyr::pivot_longer(
            cols = -colnames(poststrat_data),
            names_to = "draws",
            values_to = "posterior_sample"
          ) %>%
          dplyr::group_by(.data[[by]], .data$draws) %>%
          dplyr::summarise(value = sum(.data$posterior_sample * .data$N_j) / sum(.data$N_j)) %>%
          dplyr::ungroup()
        out <- out %>%
          dplyr::select(-dplyr::contains("draws"))
      } else {
        out <- data.frame(value = apply(poststrat_estimates, 2, function(x) sum(poststrat_data$N_j*x)/sum(poststrat_data$N_j)))
      }
      rownames(out) <- NULL
      out
    },
    #' @description Creates a set of summary statistics of the mrp estimate,
    #' and corresponding weighted and raw data estimates
    #' @param aggregated_estimates The data frame returned by the `aggregate` method.
    #' @return A data frame that consists of a minimum three rows with the raw, MRP
    #' and weighted estimates, plus an estimate of standard error. If the aggregated estimates
    #' were specified with a `by` argument (indicating sub population or small area estimates),
    #' then produces a dataframe with number of rows equal to three times the number of small areas.
    summary = function(aggregated_estimates) {
      if (!is.data.frame(aggregated_estimates)) {
        stop("'aggregated_estimates' must be a data frame ",
             "returned by the aggregate() method.", call. = FALSE)
      }
      lhs_var <- as.character(self$formula()[[2]])
      if (ncol(aggregated_estimates) > 1) {
        by_var <- colnames(aggregated_estimates)[1]
        wtd_ests <- data.frame(
          create_wtd_ests(self, lhs_var, by = by_var),
          method = "wtd"
        )
        mrp_ests <- aggregated_estimates %>%
          dplyr::group_by(.data[[by_var]])%>%
          dplyr::summarise(mean = mean(value), sd = sd(value))%>%
          dplyr::relocate(by_var, .after="sd")%>%
          dplyr::mutate(method = "mrp")

          lhs_binary <- self$map()$sample()$survey_data() %>%
          dplyr::select(dplyr::all_of(lhs_var), .key)%>%
          dplyr::mutate(lhs_binary = force_factor(.data[[lhs_var]]))

        raw_data <- self$map()$mapped_sample_data() %>%
          dplyr::left_join(lhs_binary, by = ".key")

        raw_ests <- raw_data %>%
          dplyr::group_by(.data[[by_var]])%>%
          dplyr::summarise(
            mean = mean(.data[["lhs_binary"]]),
            sd = sqrt(mean(.data[["lhs_binary"]]) * (1 - mean(.data[["lhs_binary"]])) /
                        length(.data[["lhs_binary"]]))
          ) %>%
          dplyr::relocate(by_var, .after="sd")%>%
          dplyr::mutate(method = "raw")

       } else {
        wtd_ests <- data.frame(create_wtd_ests(self, lhs_var), method = "wtd")
        mrp_ests <- data.frame(
          mean = mean(aggregated_estimates$value),
          sd = sd(aggregated_estimates$value),
          method = "mrp"
        )
        lhs_binary <- force_factor(self$map()$sample()$survey_data()[, lhs_var])
        raw_ests <- data.frame(
          mean = mean(lhs_binary),
          sd = sqrt(mean(lhs_binary) * (1 - mean(lhs_binary)) / length(lhs_binary)),
          method = "raw"
        )
      }
      out <- rbind(mrp_ests, raw_ests, wtd_ests)
      rownames(out) <- NULL
      as.data.frame(out)
    },

    #' @description Visualize population or sub-population estimates.
    #'
    #'   When passed the data frame containing the posterior distribution of the
    #'   population MRP estimate a density plot is generated.  If visualizing
    #'   sub-populations it generates a violin plot of the posterior
    #'   distribution of the aggregated MRP estimates for each level of the
    #'   grouping variable. The `additional_stats` argument controls which
    #'   other information is overlaid on the plot.
    #'
    #' @param aggregated_estimates The data frame returned by the `aggregate` method.
    #' @param additional_stats A vector that specifies which of three additional
    #'   stats (`"wtd"`, `"raw"`, `"mrp"`, `"none"`) should be overlaid on the
    #'   plot. The default is to overlay intervals for the weighted and raw
    #'   estimates on top of the density plot representing the MRP estimates.
    #'   The weighted estimates are computed by passing the optional survey
    #'   weights and design specified in the [`SurveyData`] to the \pkg{survey}
    #'   package. The raw estimate is a direct mean and binomial sd of the
    #'   binary responses. Uncertainty estimates for the `additional_stats` are
    #'   included on violin plots but not on density plots. Intervals are 95%
    #'   CI.
    #' @return A ggplot object that is either a violin plot if showing small
    #'   area level (sub-population) estimates, or a density plot if showing
    #'   population estimates.
    plot = function(aggregated_estimates, additional_stats = c("wtd","raw")) {
      if (!is.character(additional_stats)) {
        stop("'additional_stats' must be a character vector.", call. = FALSE)
      }
      if (!all(additional_stats %in% c("wtd", "raw", "mrp", "none"))) {
        stop("Valid 'additional_stats' arguments are either 'none' or a combination of ",
             "'wtd', 'raw', and 'mrp'.", call. = FALSE)
      }
      if ("none" %in% additional_stats && length(additional_stats) > 1) {
        stop("When choosing no additional statistics, only supply 'none'.",
             call. = FALSE)
      }
      if ("wtd" %in% additional_stats && all(self$map()$sample()$weights() == 1)) {
        warning("Weights are all equal to 1 or no weights provided. ",
                  "Raw estimate and weighted estimate will be equivalent.",
                  call. = FALSE)
      }

      model_fit <- private$fit_
      lhs_var <- as.character(self$formula()[[2]])
      svy_q <- private$map_$sample()$questions()[[lhs_var]]
      svy_ans <- private$map_$sample()$responses()[[lhs_var]][2]
      if (ncol(aggregated_estimates) > 1) {
        focus_var <- colnames(aggregated_estimates)[1]
        focus_var_which_q <- private$map_$item_map()[[focus_var]]$col_names()[1]
        focus_var_svy_q <- private$map_$sample()$questions()[[focus_var_which_q]]
        focus_var_responses <- private$map_$sample()$responses()[[focus_var_which_q]]
        if (is.data.frame(focus_var_responses)){
          num_modelled_levels <- length(levels(aggregated_estimates[[focus_var]]))
          combined_levels_asked <- rep(NA, num_modelled_levels)
          for (i in 1:num_modelled_levels){
            combined_levels_data <- unlist(strsplit(levels(aggregated_estimates[[focus_var]])[i], split = " \\+ "))
            combined_levels_asked[i] <- paste0(focus_var_responses$asked[focus_var_responses$data %in% combined_levels_data], collapse = " + ")
          }
          levels(aggregated_estimates[[focus_var]]) <- combined_levels_asked
        }
        gg <- ggplot2::ggplot(aggregated_estimates) +
          ggplot2::aes(x = .data[[focus_var]], y = .data[["value"]]) +
          ggplot2::geom_violin(fill = "#636363",alpha = 0.4, position = ggplot2::position_dodge(0.25)) +
          ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
          ggplot2::xlab(focus_var_svy_q)+
          ggplot2::ylab(paste0('Proportion of "', svy_ans, '"'))+
          ggplot2::ggtitle(svy_q)

        additional_ests <- self$summary(aggregated_estimates)
        additional_ests_filtered <- additional_ests[additional_ests$method %in% additional_stats, ]
        additional_ests_filtered$method <- ordered(additional_ests_filtered$method, levels = c("raw","mrp","wtd"))

        gg <- gg +
          ggplot2::geom_point(
            data = additional_ests_filtered,
            ggplot2::aes(x= .data[[focus_var]], y = mean, colour = method),
            position = ggplot2::position_dodge(0.25)
          ) +
          ggplot2::geom_errorbar(
            data = additional_ests_filtered,
            ggplot2::aes(x = .data[[focus_var]],
                         ymin = mean - 1.96 * sd,
                         ymax = mean + 1.96 * sd,
                         colour = method),
            inherit.aes = FALSE,
            alpha = 0.8,
            width = 0.25,
            position = ggplot2::position_dodge(0.25)
          )+
          ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::scale_colour_manual(values = c("wtd" = "#008837" , "raw" = "#7b3294", "mrp" = "#252525"))

      } else {
        gg <- ggplot2::ggplot(aggregated_estimates) +
          ggplot2::aes(x = .data[["value"]], y = ggplot2::after_stat(scaled)) +
          ggplot2::geom_density(fill = "#636363", alpha = 0.4) +
          ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
          ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
          ggplot2::xlab(svy_q)+
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        additional_ests <- self$summary(aggregated_estimates)
        additional_ests_filtered <- additional_ests[additional_ests$method %in% additional_stats,]
        gg <- gg +
          ggplot2::geom_vline(
            data = additional_ests_filtered,
            ggplot2::aes(xintercept = .data[["mean"]], colour = method)
          ) +
          ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::scale_colour_manual(values = c("wtd" = "#008837" , "raw" = "#7b3294", "mrp" = "#252525"))

      }
      gg
    }
  )
)


# internal ----------------------------------------------------------------

#' Create weighted estimates using the survey package
#' @noRd
#' @param fit_obj  An [R6][R6::R6Class] SurveyFit object
#' @param outcome The variable we are estimating
#' @param by The grouping variables
#' @return A table of size nlevels by 3 (level, estimate, sd).
create_wtd_ests <- function(fit_obj, outcome, by = NULL) {
  weights <- fit_obj$map()$sample()$weights()
  if (is.null(weights)) {
    stop("Sample weights must be present", call. = FALSE)
  }
  design <- fit_obj$map()$sample()$design()
  merged_data <- merge(fit_obj$map()$mapped_sample_data(),
                       fit_obj$map()$sample()$survey_data()[c(as.character(outcome),".key")],
                       by = ".key")
  svy_dsn <- do.call(survey::svydesign, c(design, list(weights = weights, data = merged_data)))
  if (is.null(by)) {
    wtd_ests <- survey::svymean(stats::as.formula(paste0(c('~',outcome), collapse = "")), design = svy_dsn)
    wtd_ests <- data.frame(wtd_ests)
    rownames(wtd_ests) <- levels(merged_data[[outcome]])
    wtd_ests <- wtd_ests[seq(2, dim(wtd_ests)[1], 2),]
    colnames(wtd_ests) <- c("mean", "sd")
  } else {
    wtd_ests <- survey::svyby(
      formula = stats::as.formula(paste0("~",outcome)),
      by = stats::as.formula(paste0("~", by)),
      design = svy_dsn,
      survey::svymean
    )
    wtd_ests <- data.frame(wtd_ests)[, c(1,3,5)]
    colnames(wtd_ests) <- c(by, "mean", "sd")
  }
  wtd_ests
}


#' Force a variable to a factor variable
#' @noRd
#' @param x A variable of length n.
#' @return A variable of length n that is binary (0,1) values. The higher level
#'   in the factor is the default 1.
force_factor <- function(x) {
  if (length(x) == 0 && is.null(dim(x))) {
    stop("x must have length n.", call. = FALSE)
  } else if (!is.null(dim(x))) {
    stop("x must be a vector of length n.", call. = FALSE)
  } else if (is.factor(x)) {
    if (length(levels(x)) != 2){
      stop("x cannot have more than 2 levels.", call. = FALSE)
    }
    if (length(levels(x)) == 2) {
      x_binary <- as.numeric(x) - 1
    }
  } else if (is.numeric(x)) {
    if (length(unique(stats::na.omit(x))) > 2){
      stop("x must have only two unique numeric values.", call. = FALSE)
    } else if (length(unique(stats::na.omit(x))) <= 2 && sum(x %in% c(1, 0, NA)) != length(x)) {
      stop("x must only contain 1, 0 and missing values", call. = FALSE)
    } else if (length(unique(stats::na.omit(x))) <= 2 && sum(x %in% c(1, 0, NA)) == length(x)) {
      x_binary <- x
    }
  }
  x_binary
}

