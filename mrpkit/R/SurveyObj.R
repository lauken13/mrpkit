#' SurveyObject
#'
#' @name SurveyObject
#'
#' @description A `SurveyObject` represents a survey.  It contains
#' a dataframe which contains all the survey data,
#' a vector of questions, and a formula class that specifies
#' the survey design.
#'
#' Examples of survey design include:
#' ~. a random sample, ~ (1|cluster), a one stage cluster sample, ~ stratum, a stratified sample
#'

SurveyObj <- R6::R6Class(
    classname = "SurveyObj",
    public = list(
        survey_data = data.frame(NULL),
        questions = as.character(NA),
        answers = list(NULL),
        design = as.formula("~."),
        initialize = function(survey_data,
                              questions,
                              answers,
                              design) {
            self$survey_data <- survey_data
            self$questions <- questions
            self$answers <- answers
            self$design <- design
            if (ncol(survey_data) != length(questions) &
                length(questions) == sum(complete.cases(questions))) {
                stop("survey_data columns must match number of questions.",
                     call. = FALSE)
            }
            if (length(answers) != length(questions) &
                length(answers) != sum(complete.cases(answers))) {
                stop("found mismatch between number of survey questions and answers.",
                     call. = FALSE)
            }
        }
    )
)

