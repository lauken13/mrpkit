#' SurveyMap
#'
#' @name SurveyMap
#'
#' @description A `SurveyMap` object holds the mappings
#' between the respective column names and column values
#'

SurveyMap <- R6::R6Class(
    classname = "SurveyMap",
    public = list(
        survey_answer = list(NULL),
        popn_answer = list(NULL),
        survey_name = as.character(NA),
        popn_name = as.character(NA),
        initialize = function(survey_answer,
                              popn_answer,
                              survey_name,
                              popn_name) {
            self$survey_answer <- survey_answer
            self$pop_answer <- pop_answer
            self$survey_name <- survey_name
            self$pop_name <- pop_name
            if (length(survey_answer) != length(popn_answer)) {
                stop("Survey and population lists must have same number of entries.",
                     call. = FALSE)
            }
        }
    )
)
