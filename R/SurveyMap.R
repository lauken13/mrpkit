#' SurveyMap
#'
#' @name SurveyMap
#' @export
#'
#' @description A `SurveyMap` object holds the mapping
#' between a set of items in a survey and a population dataset.
#' The label is the item label in each dataset and the values
#' is a list of all possible values.  The values for the survey
#' and population must be aligned, i.e., the lists must have the
#' same number of elements and the values at index i in each list
#' are equivalent.  If there is a meaningful ordering over the values,
#' they should be listed in that order, either descending or ascending.
#'
#' @examples
#' feline_prefs = SurveyObj$new(
#'   feline_survey[,c("age1","gender","pet_own","y")],
#'   questions = c("Please identify your age group","Please select your gender","Which pet do you own?", "Response"),
#'   responses = list(levels(feline_survey$age1),levels(feline_survey$gender),levels(feline_survey$pet_own),c("no","yes")),
#'   weights = feline_survey$wt,
#'   design = formula("~.")
#' )
#' popn_obj = SurveyObj$new(
#'   approx_popn[,c("age2","gender","pet_pref")],
#'   questions = c("Which age group are you?","Gender?","Which pet would you like to own?"),
#'   responses = list(levels(approx_popn$age2),levels(approx_popn$gender),levels(approx_popn$pet_pref)),
#'   weights = approx_popn$wt,
#'   design = formula("~.")
#' )
#' q1 <- question$new(
#'   name = "age",
#'   col_names = c("age1","age2"),
#'   values_map = list(
#'     "18-25" = "18-35", "26-35" = "18-35","36-45" = "36-55",
#'     "46-55" = "36-55", "56-65" = "56-65", "66-75" = "66+", "76-90" = "66+"
#'   )
#' )
#' q2 <- question$new(
#'   name = "pet",
#'   col_names = c("pet_own","pet_pref"),
#'   values_map = list("cat" = "cat", "kitten" = "cat","dog" = "dog","puppy" = "dog")
#' )
#' q3 <- question$new(
#'   name = "gender",
#'   col_names = c("gender","gender"),
#'   values_map = data.frame("male" = "m","female" = "f", "nonbinary" = "nb")
#' )
#'
#' tmp_map <- SurveyMap$new(samp_obj = feline_prefs, popn_obj = popn_obj, q1)
#' print(tmp_map)
#' tmp_map$validate()
#' tmp_map$add(q3)
#' print(tmp_map)
#' tmp_map$delete(q3)
#' print(tmp_map)
#' tmp_map$add(q3)
#' tmp_map$delete("gender")
#' print(tmp_map)
#' tmp_map$add(q2)
#' print(tmp_map)
#' tmp_map$replace(q1,q3)
#' print(tmp_map)
#' tmp_map$add(q1)
#' print(tmp_map)
#' tmp_map$validate() # TODO: figure out why this errors
#' tmp_map$mapping()
#' tmp_map$tabulate("age") #Just use age in the poststrat matrix
#' tmp_map$tabulate() #Use all variables in the map
#'
#'
#' mod_fit_1 <- tmp_map$fit(
#'   fun = rstanarm::stan_glmer,
#'   formula = y ~ (1|age) + (1|gender),
#'   family = "binomial",
#'   refresh = 100,
#'   cores = 2
#' )
#'
#' mod_fit_2 <- tmp_map$fit(
#'   fun = brms::brm,
#'   formula = y ~ (1|age) + (1|gender),
#'   family = "bernoulli",
#'   refresh = 100,
#'   cores = 2
#' )
#'
#' class(mod_fit_1)
#'
#' # predict in postrat matrix - returns a matrix
#' # with rows as poststrat rows, cols as posterior samples.
#' poststrat_fit <- tmp_map$predictify(mod_fit_1)
#'
#' tmp_map$poststratify(poststrat_fit, variable = "age") # get an estimate for a particular level
#' plot1 <- tmp_map$visualize()
#'
#' @importFrom dplyr %>%
#'
SurveyMap <- R6::R6Class(
  classname = "SurveyMap",
  public = list(
    item_map = list(),
    samp_obj = NULL,
    popn_obj = NULL,

    initialize = function(samp_obj, popn_obj, ...) {
      if (!inherits(samp_obj, "SurveyObj")) {
        stop("samp_obj must be a SurveyObj object.", call. = FALSE)
      }
      if (!inherits(popn_obj, "SurveyObj")) {
        stop("popn_obj must be a SurveyObj object.", call. = FALSE)
      }

      self$item_map <- list(...)
      for (i in 1:length(self$item_map)) {
        names(self$item_map)[i] <- self$item_map[[i]]$name
      }

      self$samp_obj <- samp_obj
      self$popn_obj <- popn_obj
      invisible(self)
    },

    print = function(...) {
      if (length(self$item_map) > 0) {
        for (i in 1:length(self$item_map)) {
          cat("==============",'\n')
          cat(self$item_map[[i]]$col_names[1], "=",
              self$item_map[[i]]$col_names[2], '\n')
          cat("--------------",'\n')
          for (j in 1:nrow(self$item_map[[i]]$values)) {
            cat(as.character(self$item_map[[i]]$values[j,1]), "=",
                as.character(self$item_map[[i]]$values[j,2]), '\n')
          }
        }
      } else {
        cat("==============",'\n')
        cat("empty mapping",'\n')
      }
      invisible(self)
    },

    add = function(...) {
      for (i in 1:length(list(...))) {
        ll_length <- length(self$item_map)
        if (list(...)[[i]]$name %in% names(self$item_map)) {
          stop("Survey label: ", list(...)[[i]]$name, " already defined.  ",
               "Use function 'replace' instead. ", call. = FALSE)
        }
        self$item_map[[ll_length + 1]] <- list(...)[[i]]
        names(self$item_map)[ll_length + 1] <- self$item_map[[ll_length + 1]]$name
      }
      invisible(self)
    },

    delete = function(...) {
      tmp_list <- list(...)
      for (i in length(tmp_list)) {
        if (inherits(tmp_list[[i]], "SurveyQuestion")) {
          loc_id <- names(self$item_map) %in% tmp_list[[i]]$name
          loc_name <- tmp_list$name[[i]]
        } else {
          loc_id <- names(self$item_map) %in% tmp_list[[i]]
          loc_name <- tmp_list[[i]]
        }
        if (sum(loc_id) == 0) {
          stop("Survey label: ", loc_name, " not defined.  ",
               call. = FALSE)
        } else{
          self$item_map[[which(loc_id)]] <- NULL
        }
      }
      invisible(self)
    },

    #' @description Replace one survey question with another
    #' @param old_question The [SurveyQuestion] object to replace.
    #' @param new_question The [SurveyQuestion] object to use instead.
    #'
    replace = function(old_question, new_question) {
      self$delete(old_question)
      self$add(new_question)
    },

    validate = function() {
      samp_dfnames <- colnames(self$samp_obj$survey_data)
      popn_dfnames <-colnames(self$popn_obj$survey_data)
      samp_mapnames <- character(length(self$item_map))
      popn_mapnames <- character(length(self$item_map))
      for (j in 1:length(self$item_map)) {
        samp_mapnames[j] <- self$item_map[[j]]$col_names[1]
        popn_mapnames[j] <- self$item_map[[j]]$col_names[2]
        if (!is.factor(self$samp_obj$survey_data[,samp_mapnames[j]])) {
          self$samp_obj$survey_data[,samp_mapnames[j]] <- as.factor(self$samp_obj$survey_data[,samp_mapnames[j]])
          warning("Converting ", popn_mapnames[j], "into a factor with levels ", paste(levels(self$samp_obj$survey_data[,samp_mapnames[j]]), collapse = ", "))
        }
        if (!is.factor(self$popn_obj$survey_data[,popn_mapnames[j]])) {
          self$popn_obj$survey_data[,popn_mapnames[j]] <- as.factor(self$popn_obj$survey_data[,popn_mapnames[j]])
          warning("Converting ", popn_mapnames[j], "into a factor with levels ", paste(levels(self$popn_obj$survey_data[,popn_mapnames[j]]), collapse = ", "))
        }
        levels_map_samp <- levels(self$item_map[[j]]$values[, 1])
        levels_map_popn <- levels(self$item_map[[j]]$values[, 2])
        levels_data_samp <- levels(self$samp_obj$survey_data[, samp_mapnames[j]])
        levels_data_popn <- levels(self$popn_obj$survey_data[, popn_mapnames[j]])
        if (!samp_mapnames[j] %in% samp_dfnames) {
          stop("Variable ", samp_mapnames[j], " not in sample", call. = FALSE)
        }
        if (!popn_mapnames[j] %in% popn_dfnames) {
          stop("Variable ", popn_mapnames[j], " not in population", call. = FALSE)
        }
        if (sum(!levels_map_samp %in% levels_data_samp) > 0) {
          stop("Levels in ",samp_mapnames[j]," ", paste(levels_map_samp[!levels_map_samp %in% levels_data_samp], collapse = ", "),
               " are included in the map but are not in the sample", call. = FALSE)
        }
        if (sum(!levels_data_samp %in% levels_map_samp) > 0) {
          stop("Levels in ",samp_mapnames[j]," ", paste(levels_data_samp[!levels_data_samp %in% levels_map_samp], collapse = ", "),
               " are included in the sample but are not in the map", call. = FALSE)
        }
        if (sum(!levels_map_popn %in% levels_data_popn) > 0) {
          stop("Levels in ",popn_mapnames[j]," ",paste(levels_map_popn[!levels_map_popn %in% levels_data_popn], collapse = ", "),
               " are included in the map but are not in the population data",call. = FALSE)
        }
        if (sum(!levels_data_popn %in% levels_map_popn)>0) {
          stop("Levels in ",popn_mapnames[j]," ", paste(levels_data_popn[!levels_data_popn %in% levels_map_popn], collapse = ", "),
               " are included in the population data but are not in the map", call. = FALSE)
        }
      }
      if (sum(popn_mapnames %in% popn_dfnames) < length(popn_dfnames)) {
        warning("Variable(s) ", paste(popn_dfnames[!popn_dfnames %in% popn_mapnames], collapse = ", "),
                " are available in the population but won't be used in the model ", call. = FALSE)
      }
      if (sum(!samp_dfnames %in% c(samp_mapnames,popn_dfnames)) == 0) {
        warning("At least one variable in the survey needs to be unknown in the population.",call. = FALSE)
      }
    },
    mapping  = function() {
      ####Set up keys####
      self$samp_obj$mapped_data <- data.frame(key = 1:nrow(self$samp_obj$survey_data))
      self$samp_obj$survey_data$key <- 1:nrow(self$samp_obj$survey_data)
      self$popn_obj$mapped_data <- data.frame(key = 1:nrow(self$popn_obj$survey_data))
      self$popn_obj$survey_data$key <- 1:nrow(self$popn_obj$survey_data)
      for (j in 1:length(self$item_map)) {
        #### set up names ####
        samp_mapnames <- self$item_map[[j]]$col_names[1]
        popn_mapnames <- self$item_map[[j]]$col_names[2]
        levels_map_samp <- self$item_map[[j]]$values[, 1]
        levels_map_popn <- self$item_map[[j]]$values[, 2]
        new_varname <- self$item_map[[j]]$name
        new_levels_samp <- character(length(levels_map_samp))
        new_levels_popn <- character(length(levels_map_popn))
        for (k in 1:length(levels_map_samp)) {
          is_samp_unique <- sum(levels_map_samp %in% levels_map_samp[k]) == 1
          is_popn_unique <- sum(levels_map_popn %in% levels_map_popn[k]) == 1
          new_levels_samp[k] <- as.character(levels_map_samp[k])
          new_levels_popn[k] <- as.character(levels_map_popn[k])
          if (is_samp_unique & !is_popn_unique) {
            names(new_levels_samp)[k] <- as.character(levels_map_popn[k])
            names(new_levels_popn)[k] <- as.character(levels_map_popn[k])
          } else if (is_samp_unique & is_popn_unique) {
            names(new_levels_samp)[k] <- as.character(levels_map_samp[k])
            names(new_levels_popn)[k] <- as.character(levels_map_samp[k])
          } else if (!is_samp_unique & is_popn_unique) {
            names(new_levels_samp)[k] <- as.character(levels_map_samp[k])
            names(new_levels_popn)[k] <- as.character(levels_map_samp[k])
          } else if (!is_samp_unique & !is_popn_unique) {
            stop("Mapping can only handle many to one mappings.", call. = FALSE)
          }
        }
        self$samp_obj$mapped_data[[new_varname]] <- forcats::fct_recode(self$samp_obj$survey_data[[samp_mapnames]], !!!new_levels_samp)
        self$popn_obj$mapped_data[[new_varname]] <- forcats::fct_recode(self$popn_obj$survey_data[[popn_mapnames]], !!!new_levels_popn)
      }
    },
    tabulate  = function(...) {
      grouping_vars <- c(...)
      if (length(grouping_vars) == 0) {
        grouping_vars <- names(self$item_map)
      }
      if (sum(!grouping_vars %in% names(self$item_map)) > 0) {
        stop("At least one poststratification variable doesn't correspond to the map.", call. = FALSE)
      }
      self$popn_obj$poststrat <- self$popn_obj$mapped_data %>%
        dplyr::mutate(wts = self$popn_obj$weights) %>%
        dplyr::group_by_at(dplyr::all_of(grouping_vars)) %>%
        dplyr::summarize(N_j = sum(wts), .groups = 'drop')
      invisible(self)
    },

    #' @description Fit a model
    #' @param fun The model fitting function to use. For example,
    #'   `fun=rstanarm::stan_glmer` or `fun=brms::brm`.
    #' @param formula The model formula. Can be either a string or a formula
    #'   object.
    #' @param ... Arguments other than `formula` and `data` to pass to the model
    #'   fitting function.
    #'
    fit = function(fun, formula, ...) {
      fun <- match.fun(fun)
      args <- list(...)
      if (!is.null(args$data)) {
        stop("The 'data' argument should not be specified.",
             call. = FALSE)
      }
      if (is.null(self$samp_obj$mapped_data)) {
        stop("Mapped data not found. ",
             "Please call the mapping() method before fitting a model.",
             call. = FALSE)
      }

      # TODO: error if variables in formula aren't in data
      formula <- as.formula(formula)
      mapped_data <- self$samp_obj$mapped_data
      need_vars <- setdiff(all.vars(formula), colnames(mapped_data))
      y_and_x <- self$samp_obj$survey_data[, need_vars, drop = FALSE]

      args$formula <- formula
      args$data <- cbind(mapped_data, y_and_x)
      do.call(fun, args)
    },

    #' @description Use fitted model to add predicted probabilities to post-stratification dataset.
    #' @param fitted_model The name of the model that was fit. For example, `fit1`.
    #' @param fun The function to use to generate the predicted probabilities.
    #'   This should only be specified if using a custom function, otherwise for
    #'   \pkg{rstanarm} and \pkg{brms} models `posterior_epred()` is
    #'   automatically used (with the result transposed) and for \pkg{lme4}
    #'   models the [sim_posterior_epred()] is used. If `fun` is a custom
    #'   function then the first argument should take in the fitted model object
    #'   and the second argument should take in the poststratification
    #'   (`newdata`) data frame. The function must return a matrix with rows
    #'   corresponding to the columns of the poststrat data and columns
    #'   corresponding to simulations.
    #' @param ... Arguments other than the fitted model and `newdata` data frame
    #'   to pass to `fun`.
    #'
    predictify = function(fitted_model, fun = NULL, ...) {
      args <- list(...)
      if (!is.null(args$newdata)) {
        stop("The 'newdata' argument should not be specified.",
             call. = FALSE)
      }
      if (is.null(self$samp_obj$poststrat)) {
        stop("Post-stratification data not found. ",
             "Please call the tabulate() method before fitting a model.",
             call. = FALSE)
      }
      poststrat <- self$popn_obj$poststrat

      if (is.null(args$fun)) {
        if ("stanreg" %in% class(fitted_model)){
          return(
            t(rstanarm::posterior_epred(
              object = fitted_model,
              newdata = poststrat,
              ...
            ))
          )
        }
        if ("brmsfit" %in% class(fitted_model)){
          return(
            t(brms::posterior_epred(
              object = fitted_model,
              newdata = poststrat,
              allow_new_levels = TRUE,
              sample_new_levels =
                if (!is.null(args$sample_new_levels)) args$sample_new_levels
                else "gaussian",
              ...
            ))
          )
        }
        if ("glmerMod" %in% class(fitted_model)) {
          return(
            sim_posterior_epred(
              object = fitted_model,
              newdata = poststrat,
              ...
            )
          )
        }
      } else {
        poststrat <- self$popn_obj$poststrat
        fun <- match.fun(fun)
        fun(fitted_model, poststrat, ...)
      }
    }
  )
)

