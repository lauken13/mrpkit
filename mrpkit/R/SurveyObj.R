#' An S4 class to represent a survey
#'
#' @slot survey_data A dataframe
#' @slot questions A vector of questions
#' @slot design A formula class that specifies the survey design. Examples include
#' ~. a random sample, ~ (1|cluster), a one stage cluster sample, ~ stratum, a stratified sample
#'
#' @export

SurveyObj <- setClass("SurveyObj",
                      slots = c(
                        survey_data = "data.frame",
                        questions = "character",
                        answers = "list",
                        design = "formula"
                      ),
                      prototype = c(
                        survey_data = data.frame(NULL),
                        questions = as.character(NA),
                        answers = list(NULL),
                        design = as.formula("~.")
                      )
)


SurveyObj<- function(survey_data, questions = NA, answers = NA, design = "~.") {
  survey_data <- as.data.frame(survey_data)
  questions <- as.character(questions)
  answers <- as.list(answers)
  design <- as.formula(design)
  new("SurveyObj", survey_data = survey_data, questions = questions, answers = answers, design = design)
}

setValidity("SurveyObj", function(object) {
  if (ncol(object@survey_data) != length(object@questions) & length(object@questions) == sum(complete.cases(object@questions))) {
    "@survey_data and @questions must be same length"
  } else if (length(object@answers) != length(object@questions) & length(object@answers) != sum(complete.cases(object@answers))) {
    "@answers and @questions must be same length"
  } else {
    TRUE
  }
})


