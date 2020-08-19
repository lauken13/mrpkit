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
            self$popn_answer <- popn_answer
            self$survey_name <- survey_name
            self$popn_name <- popn_name
            if (length(survey_answer) != length(popn_answer)) {
                stop("Survey and population lists must have same number of entries.",
                     call. = FALSE)
            }
        },
        print = function(...) {
            for(j in 1: length(self$survey_name)){
                cat("==============",'\n')
                cat(self$survey_name[j], "=", self$popn_name[j], '\n')
                cat("--------------",'\n')
                for(i in 1:length(self$survey_answer[[j]])){
                    cat(self$survey_answer[[j]][i], "=", self$popn_answer[[j]][i], '\n')
                }
            }
            invisible(self)
        }
    )
)
