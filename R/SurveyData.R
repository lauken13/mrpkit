#' SurveyData objects
#'
#' @name SurveyData
#' @export
#' @description
#' A `SurveyData` object represents a survey and its metadata.
#' The survey itself is a data frame.
#' The survey metatdata consists of the following:
#'  - per-column questions: a list of strings
#'  - per-column allowed response values: a list of character vectors
#'  - per-column survey weights: a vector of numeric weights
#'  - survey design: a named list of elements to pass to svydesign in the survey package
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
#'   design = list(ids =~1, strata=~stype)
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
#'   design = NULL
#' )
#' popn_obj$print()
#'
SurveyData <- R6::R6Class(
    classname = "SurveyData",
    public = list(
        survey_data = data.frame(NULL),
        poststrat = data.frame(NULL),
        mapped_data = data.frame(NULL),
        questions = character(0),
        responses = list(),
        weights = numeric(),
        design = list(),
        initialize = function(survey_data,
                              questions = character(0),
                              responses = list(),
                              weights = numeric(),
                              design = as.formula("~.")) {
            self$survey_data <- survey_data
            self$questions <- questions
            self$responses <- responses
            self$weights <- weights
            self$design <- design
            if (ncol(survey_data) == 0) {
                stop("survey_data cannot be empty.",
                     call. = FALSE)
            }
            # allow no question/response, else require info for all columns
            if (length(questions) != 0 | length(responses) != 0) {
                if (ncol(survey_data) != length(questions) &
                    length(questions) == sum(complete.cases(questions))) {
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
        },
        print = function(...) {
            cat("Survey containing", nrow(self$survey_data), "observations", "\n")
            for (i in 1:ncol(self$survey_data)) {
                cat("Column ", i , " label: ", names(self$survey_data)[i], "\n")
                if (length(self$questions) > 0) {
                    cat("Question ", i, " (", self$questions[i], ")", "\n")
                    cat("Allowed answers: ",
                        paste(self$responses[[i]], collapse = ", "), "\n")
                }
            }
            invisible(self)
        }
    )
)
