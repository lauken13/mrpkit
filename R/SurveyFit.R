#' SurveyFit
#'
#' @name SurveyFit
#' @export
#' @description An [R6][R6::R6Class] `SurveyFit` object stores a fitted model
#'   object and provides methods for generating predicted probabilities for all
#'   poststrat cells, generating population and group estimates, and visualizing
#'   results.
#' @inherit SurveyMap examples
#'
SurveyFit <- R6::R6Class(
  classname = "SurveyFit",
  private = list(
    map_ = NULL,
    fit_ = NULL
  ),
  public = list(

    #' @description Create a new `SurveyFit` object. This method is called
    #'   internally by the `$fit()` method of the [`SurveyMap`] object and does
    #'   not need to be called directly by the user.
    #' @param fit A fitted model object.
    #' @param map A [`SurveyMap`] object.
    initialize = function(fit, map) {
      private$fit_ <- fit
      private$map_ <- map
      invisible(self)
    },

    #' @description Access the fitted model object
    fit = function() {
      private$fit_
    },

    #' @description Access the SurveyMap object
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
        fun(fitted_model, poststrat, ...)
      }
    },

    #' @description Aggregate estimates to the population level or by level of a grouping variable
    #' @param poststrat_estimates The object returned by `population_predict`.
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
        rotate_levels <- levels(private$map_$mapped_sample_data()[, by])
        out <- expand.grid(by = rotate_levels, draw = 1:ncol(poststrat_estimates), value = NA)
        colnames(out)[1] <- by
        for (focus_level in rotate_levels){
          level_loc <- poststrat_data[by] == focus_level
          out[out[by] == focus_level, "value"] <-
            apply(poststrat_estimates[level_loc, ], 2, function(x) sum(poststrat_data$N_j[level_loc]*x)/sum(poststrat_data$N_j[level_loc]))
        }
        out <- out %>%
          dplyr::select(-"draw")
      } else {
        out <- data.frame(value = apply(poststrat_estimates, 2, function(x) sum(poststrat_data$N_j*x)/sum(poststrat_data$N_j)))
      }
      out
    },
    #' @description Creates a set of summary statistics of the mrp estimate,
    #' and corresponding weighted and raw data estimates
    #' @param aggregated_estimates The object returned by `aggregate`.
    #' @return A data frame. If `by` is not specified then the data frame will
    #'   have number of rows equal to the number of posterior draws. If `by` is
    #'   specified the data frame will have number of rows equal to the number
    #'   of posterior draws times the number of levels of the `by` variable,
    #'   and there will be an extra column indicating which level of the `by`
    #'   variable each row corresponds to.
    summary = function(aggregated_estimates) {
      if(is.null(aggregated_estimates)){
        stop("Must pass aggregated MRP estimates produced by aggregate function.")
      }
      if(length(dim(aggregated_estimates))!=2 | is.null(dim(aggregated_estimates))){
        stop("dimensions of aggregated estimates must be two")
      }
      model_fit <- private$fit_
      lhs_var <- as.character(formula(model_fit))[[2]]
      if (ncol(aggregated_estimates) > 1) {
        by_var <- colnames(aggregated_estimates)[1]
        wtd_ests <- data.frame(create_wtd_ests(self, lhs_var, by=by_var), method = "wtd")
        mrp_ests_tmp <- by(aggregated_estimates[,"value"], aggregated_estimates[,by_var],function(x)c(mean = mean(x),sd = sd(x)))
        mrp_ests_df <- sapply(mrp_ests_tmp, function(x) x)
        mrp_ests <- data.frame(t(mrp_ests_df),by_var = colnames(mrp_ests_df), method = "mrp")
        colnames(mrp_ests)[3] = by_var
        lhs_binary <- force_factor(private$fit_$data[,lhs_var])
        raw_ests_tmp <- by(lhs_binary, private$fit_$data[,by_var],function(x)c(mean = mean(x),sd = sqrt(mean(x)*(1-mean(x))/length(x))))
        raw_ests_df <- sapply(raw_ests_tmp, function(x) x)
        raw_ests <- data.frame(t(raw_ests_df),by_var = colnames(raw_ests_df), method = "raw")
        colnames(raw_ests)[3] = by_var
      } else{
        wtd_ests <- data.frame(create_wtd_ests(self, lhs_var), method = "wtd")
        mrp_ests <- data.frame(mean=mean(aggregated_estimates$value),sd = sd(aggregated_estimates$value), method = "mrp")
        lhs_binary <- force_factor(private$fit_$data[,lhs_var])
        raw_ests <- data.frame(mean = mean(lhs_binary),sd = sqrt(mean(lhs_binary)*(1-mean(lhs_binary))/length(lhs_binary)), method = "raw")
      }
      out <- rbind(mrp_ests, raw_ests,wtd_ests)
      rownames(out) <- NULL
      out
    },
    #' @description Plot takes the aggregated estimates and produces a quick visualization total and sub-population estimates.
    #' @param aggregated_estimates The object returned by `aggregate`
    #' @param additional_stats A vector the specifies which of three additional stats (wtd, raw, mrp) should be included on the plots.
    #' The default value is to include the weighted estimate (wtd) and raw data mean (raw), but an analogous bar for mrp (mrp) can be
    #' added using the posterior mean and sd. The sd for the weighted estimate uses the survey design and the survey package, whilst the
    #' raw is a direct mean and binomial sd of the binary responses. Intervals are by default 95% CI
    plot = function(aggregated_estimates, additional_stats = c("wtd","raw")) {
      model_fit <- private$fit_
      lhs_var <- as.character(formula(model_fit))[[2]]
      svy_q <- private$map_$sample()$questions()[[lhs_var]]
      svy_ans <- private$map_$sample()$responses()[[lhs_var]][2]
      if (ncol(aggregated_estimates) > 1) {
        focus_var <- colnames(aggregated_estimates)[1]
        focus_var_which_q <- private$map_$item_map()[[focus_var]]$col_names()[1]
        focus_var_svy_q <- private$map_$sample()$questions()[[focus_var_which_q]]
        gg <- ggplot2::ggplot(aggregated_estimates) +
          ggplot2::aes(x = .data[[focus_var]], y = .data[["value"]]) +
          ggplot2::geom_violin(fill = "#636363",alpha = .4, position = ggplot2::position_dodge(.25)) +
          ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
          ggplot2::xlab(focus_var_svy_q)+
          ggplot2::ylab(paste0('Proportion of "',svy_ans,'"'))+
          ggplot2::ggtitle(svy_q)

        additional_ests <- self$summary(aggregated_estimates)
        if(sum(additional_stats %in% c("wtd","raw","mrp", "none")) != length(additional_stats)){
          stop("only supply arguments `wtd`, `raw`, `mrp` or `none` as additional statistics.")
        }
        if(sum(additional_stats %in% "none")==1 & length(additional_stats)>1){
          stop("when choosing no additional statistics, only supply `none`")
        }
        if(is.null(additional_stats)){
          stop("when choosing no additional statistics, only supply `none`")
        }
        additional_ests_filtered <- additional_ests[additional_ests$method %in% additional_stats,]
        additional_ests_filtered$method <- ordered(additional_ests_filtered$method, levels = c("raw","mrp","wtd"))
        if(sum(self$map()$sample()$weights() ==1)==length(self$map()$sample()$weights())){
          warning("weights are all equal to 1 or no weights provided. Raw estimate and wtd estimate will be equivalent.")
        }
        gg <- gg +
          ggplot2::geom_point(data = additional_ests_filtered, ggplot2::aes(x= .data[[focus_var]], y = mean, colour = method),
                              position = ggplot2::position_dodge(.25)) +
          ggplot2::geom_errorbar(
            data = additional_ests_filtered,
            ggplot2::aes(x = .data[[focus_var]],
                         ymin = mean - 1.96*sd,
                         ymax = mean + 1.96*sd,
                         colour = method),
            inherit.aes = FALSE, alpha = .8, width = .25, position = ggplot2::position_dodge(.25))+
          ggplot2::theme(legend.position = "bottom",
                         legend.title = ggplot2::element_blank()) +
          ggplot2::scale_colour_manual(values = c("wtd" = "#008837" , "raw" = "#7b3294", "mrp" = "#252525"))

      } else {
        gg <- ggplot2::ggplot(aggregated_estimates) +
          ggplot2::aes(x = .data[["value"]], y = ggplot2::after_stat(scaled)) +
          ggplot2::geom_density(fill = "#636363", alpha = .4) +
          ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
          ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
          ggplot2::xlab(svy_q)+
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

      additional_ests <- self$summary(aggregated_estimates)
      if(length(additional_stats %in% c("wtd","raw")) != length(additional_stats)){
        stop("additional statistics can only be weighted (wtd) or raw (raw)")
      }
      additional_ests_filtered <- additional_ests[additional_ests$method %in% additional_stats,]
      gg <- gg +
        ggplot2::geom_vline(data = additional_ests_filtered, ggplot2::aes(xintercept = .data[["mean"]], colour = method)) +
        ggplot2::theme(legend.position = "bottom",
              legend.title = ggplot2::element_blank()) +
        ggplot2::scale_colour_manual(values = c("wtd" = "#008837" , "raw" = "#7b3294", "mrp" = "#252525"))

      }
      gg
    }
  )
)


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
                       fit_obj$map()$sample()$survey_data()[c(outcome,".key")],
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


#' Create weighted estimates using the survey package
#' @noRd
#' @param x A variable of length n
#' @return A variable of length n that is binary (0,1) values. The higher level in the factor
#' is the default 1
force_factor <- function(x) {
  if(length(x) ==0 & is.null(dim(x))){
    stop("x must have length n")
  }
  else if(!is.null(dim(x))){
    stop("x must be a vector of length n")
  }
  else if(is.factor(x) ){
    if(length(levels(x))!=2){
      stop("x cannot have more than 2 levels")
    }
    if(length(levels(x))==2){
    x_binary <- as.numeric(x)-1
    }
  }
  else if(is.numeric(x)){
    if(length(unique(na.omit(x)))>2){
      stop("x must have only two unique numeric values")
    } else if(length(unique(na.omit(x)))<=2 & sum(x %in% c(1,0,NA))!=length(x)){
      stop("x must only contain 1, 0 and missing values")
    }
    else if(length(unique(na.omit(x)))<=2& sum(x %in% c(1,0,NA))==length(x)){
      x_binary <- x
    }
  }
  x_binary
}

