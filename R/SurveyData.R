#' SurveyData objects
#'
#' @name SurveyData
#' @export
#' @description
#' A `SurveyData` object represents a survey and its metadata. The survey itself
#' is a data frame. The survey metadata consists of the text of the survey
#' questions, the allowed response values, and optionally survey weights and a
#' survey design specification.
#'
#' @examples
#'
#' head(feline_survey)
#' feline_prefs <- SurveyData$new(
#'   data = feline_survey,
#'   questions = list(
#'     age1 = "Please identify your age group",
#'     gender = "Please select your gender",
#'     pet_own = "Which pet do you own?",
#'     y = "Response"
#'   ),
#'   responses = list(
#'     age1 = levels(feline_survey$age1),
#'     gender = levels(feline_survey$gender),
#'     pet_own = levels(feline_survey$pet_own),
#'     y = c("no","yes")
#'   ),
#'   weights = feline_survey$wt,
#'   design = list(ids =~1)#, strata=~stype)
#' )
#' feline_prefs$print()
#'
#' head(approx_popn)
#' popn_obj <- SurveyData$new(
#'   data = approx_popn,
#'   questions = list(
#'     age2 = "Which age group are you?",
#'     gender = "Gender?",
#'     pet_pref = "Which pet would you like to own?"
#'   ),
#'   # order doesn't matter (gender before age2 here) because
#'   # the list has the names of the variables
#'   responses = list(
#'     gender = levels(approx_popn$gender),
#'     age2 = levels(approx_popn$age2),
#'     pet_pref = levels(approx_popn$pet_pref)
#'   ),
#'   weights = approx_popn$wt
#' )
#' popn_obj$print()
#'
SurveyData <- R6::R6Class(
    classname = "SurveyData",
    private = list(
        survey_data_ = data.frame(NULL),
        mapped_data_ = data.frame(NULL),
        questions_ = list(),
        responses_ = list(),
        weights_ = numeric(),
        design_ = list()
    ),
    public = list(
        #' @description Create a new SurveyData object
        #' @param data A data frame containing the survey data.
        #' @param questions,responses Named lists containing the text of the
        #'   survey questions and the allowed responses, respectively. The names
        #'   must correspond to the names of variables in `data`. See
        #'   **Examples**.
        #' @param weights Optionally, a vector of survey weights.
        #' @param design Optionally, a named list of arguments to pass to `survey::svydesign()`.
        initialize = function(data,
                              questions = list(),
                              responses = list(),
                              weights = numeric(),
                              design = list(ids =~1)) {
            if (ncol(data) == 0 || nrow(data) == 0) {
                stop("'data' cannot be empty.", call. = FALSE)
            }
            # allow no question/response, else require info for all columns
            if (length(questions) != 0 || length(responses) != 0) {
                if (ncol(data) != length(questions) &
                    length(questions) == sum(stats::complete.cases(data))) {
                    stop("Mismatch between number of data columns and questions.",
                         call. = FALSE)
                }
                if (length(responses) != length(questions)) {
                    stop("Mismatch between number of survey questions and answers.",
                         call. = FALSE)
                }
            }
            # allow no weights, else require weights for all columns
            if (length(weights) != 0 & nrow(data) != length(weights)) {
                stop("Mismatch between number of data columns and weights.",
                     call. = FALSE)
            }
            nms_q <- sort(names(questions))
            nms_r <- sort(names(responses))
            if (is.null(nms_q) || sum(nzchar(nms_q)) != length(nms_q)) {
                stop("All elements of 'questions' and 'responses' list must have names.", call. = FALSE)
            }
            if (!identical(nms_q, nms_r)) {
                stop("Names in 'questions' and 'responses' lists must be the same.")
            }
            if (!is.null(nms_q)) {
                questions <- questions[nms_q]
                responses <- responses[nms_q]
            }
            data <- data[, nms_q, drop = FALSE]

            private$questions_ <- questions
            private$responses_ <- responses
            private$weights_ <- weights
            private$design_ <- design
            private$survey_data_ <- data.frame(.key = 1:nrow(data), data)
            private$mapped_data_ <- data.frame(.key = 1:nrow(data))
            invisible(self)
        },

        #' @description Number of observations in the survey data
        n_obs = function() nrow(private$survey_data_),

        #' @description Number of survey questions
        n_questions = function() length(private$questions_),

        #' @description Print a summary of the survey data
        print = function(...) {
            cat("Survey with",
                self$n_obs(), "observations,",
                self$n_questions(), "questions",
                "\n")
            complex_svy_design <- do.call(svydesign, c(private$design_, list(weights = private$weights_, data = private$survey_data_)))
            design_out <- R.utils::captureOutput(print(complex_svy_design))
            cat(design_out[1])

            for (i in 1:ncol(self$survey_data(key = FALSE))) {
                cat("\nColumn label:", names(self$survey_data(key = FALSE))[i], "\n")
                if (length(private$questions_) > 0) {
                    cat("Question:", private$questions_[[i]], "\n")
                    cat("Allowed answers:",
                        paste(private$responses_[[i]], collapse = ", "), "\n")
                }
            }
            invisible(self)
        },

        add_survey_data_column = function(name, value) {
            private$survey_data_[[name]] <- value
            invisible(self)
        },
        add_mapped_data_column = function(name, value) {
            if (ncol(private$mapped_data_)  == 0) {
                private$mapped_data_ <- data.frame(value)
                colnames(private$mapped_data_) <- name
            } else {
                private$mapped_data_[[name]] <- value
            }
            invisible(self)
        },
        survey_data = function(key = TRUE) {
            if (key) {
                private$survey_data_
            } else {
                private$survey_data_[, colnames(private$survey_data_) != ".key", drop = FALSE]
            }
        },
        mapped_data = function(key = TRUE) {
            if (key) {
                private$mapped_data_
            } else {
                private$mapped_data_[, colnames(private$mapped_data_) != ".key", drop = FALSE]
            }
        },
        questions = function() private$questions_,
        responses = function() private$responses_,
        weights = function() private$weights_,
        design = function() private$design_
    )
)
