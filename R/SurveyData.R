#' SurveyData objects
#'
#' @name SurveyData
#' @export
#' @description
#' An [R6][R6::R6Class] `SurveyData` object represents a survey and its metadata.
#' The survey itself is a data frame.
#' The survey metatdata consists of the following:
#'  - per-column questions: a list of strings
#'  - per-column allowed response values: a list of character vectors
#'  - per-column survey weights: a vector of numeric weights
#'  - survey design: a string that specifies the survey design using \pkg{lme4}
#'  style formula syntax.
#'
#' Examples of survey designs include:
#'   - `~.`: a random sample
#'   - `~ (1|cluster)`: a one stage cluster sample
#'   - `~ stratum`: a stratified sample
#'
#' @examples
#' feline_prefs <- SurveyData$new(
#'   feline_survey[,c("age1","gender","pet_own","y")],
#'   questions = c(
#'     "Please identify your age group",
#'     "Please select your gender",
#'     "Which pet do you own?",
#'     "Response"
#'   ),
#'   responses = list(
#'     levels(feline_survey$age1),
#'     levels(feline_survey$gender),
#'     levels(feline_survey$pet_own),
#'     c("no","yes")
#'   ),
#'   weights = feline_survey$wt,
#'   design = formula("~.")
#' )
#' feline_prefs$print()
#'
#' popn_obj <- SurveyData$new(
#'   approx_popn[,c("age2","gender","pet_pref")],
#'   questions = c(
#'     "Which age group are you?",
#'     "Gender?",
#'     "Which pet would you like to own?"
#'   ),
#'   responses = list(
#'     levels(approx_popn$age2),
#'     levels(approx_popn$gender),
#'     levels(approx_popn$pet_pref)
#'   ),
#'   weights = approx_popn$wt,
#'   design = formula("~.")
#' )
#' popn_obj$print()
#'
SurveyData <- R6::R6Class(
    classname = "SurveyData",
    private = list(
        survey_data_ = data.frame(NULL),
        mapped_data_ = data.frame(NULL),
        questions_ = character(0),
        responses_ = list(),
        weights_ = numeric(),
        design_ = as.formula("~.")
    ),
    public = list(
        initialize = function(survey_data,
                              questions = character(0),
                              responses = list(),
                              weights = numeric(),
                              design = as.formula("~.")) {
            if (ncol(survey_data) == 0 || nrow(survey_data) == 0) {
                stop("survey_data cannot be empty.",
                     call. = FALSE)
            }
            # allow no question/response, else require info for all columns
            if (length(questions) != 0 || length(responses) != 0) {
                if (ncol(survey_data) != length(questions) &
                    length(questions) == sum(stats::complete.cases(questions))) {
                    stop("mismatch between number of survey_data columns and questions.",
                         call. = FALSE)
                }
                if (length(responses) != length(questions)) {
                    stop("mismatch between number of survey questions and answers.",
                         call. = FALSE)
                }
            }
            # allow no weights, else require weights for all columns
            if (length(weights) != 0 & nrow(survey_data) != length(weights)) {
                stop("mismatch between number of survey_data columns and weights.",
                     call. = FALSE)
            }
            private$questions_ <- questions
            private$responses_ <- responses
            private$weights_ <- weights
            private$design_ <- design
            private$survey_data_ <- data.frame(key = 1:nrow(survey_data), survey_data)
            private$mapped_data_ <- data.frame(key = 1:nrow(survey_data))
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
            if (length(private$design_ == 2)) {
                cat("Random Sampling Design \n")
            } else {
                cat("Simple")
                require_suggested_package("lme4")
                if (is.null(lme4::findbars(private$design_))){
                    cat(" stratified sample with strata", all.vars(terms(private$design_))[[1]], "\n")
                } else {
                    cat(" cluster sample with cluster", all.vars(terms(private$design_))[[1]], "\n")
                }
            }
            for (i in 1:ncol(self$survey_data(key = FALSE))) {
                cat("\nColumn label:", names(self$survey_data(key = FALSE))[i], "\n")
                if (length(private$questions_) > 0) {
                    cat("Question:", private$questions_[i], "\n")
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
                private$survey_data_[, colnames(private$survey_data_) != "key"]
            }
        },
        mapped_data = function(key = TRUE) {
            if (key) {
                private$mapped_data_
            } else {
                private$mapped_data_[, colnames(private$mapped_data_) != "key"]
            }
        },
        questions = function() private$questions_,
        responses = function() private$responses_,
        weights = function() private$weights_,
        design = function() private$design_
    )
)
