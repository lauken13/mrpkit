#' An S4 class to map survey classes together
#'
#' @slot survey_vars A vector of colnames
#' @slot popn_vars A vector of colnames
#'
#'
#' @export

SurveyMap <- setClass("SurveyMap",
                      slots = c(
                        survey_answer = "list",
                        popn_answer = "list",
                        survey_name = "character",
                        popn_name = "character"
                      ),
                      prototype = c(
                        survey_answer = list(NULL),
                        popn_answer = list(NULL),
                        survey_name = as.character(NA),
                        popn_name = as.character(NA)
                      )
)


SurveyMap<- function(SurveyMap, survey_answer, popn_answer, survey_name, popn_name) {
  survey_answer <- as.list(survey_answer)
  popn_answer <- as.list(popn_answer)
  survey_name <- as.character(survey_name)
  popn_name <- as.character(popn_name)
  new("SurveyMap", survey_answer = survey_answer, popn_answer = popn_answer, survey_name = survey_name, popn_name = popn_name)
}

setValidity("SurveyMap", function(object) {
  for(i in length(object@survey_name)){
    if (length(object@survey_answer) != length(object@popn_answer)) {
      cat("Survey and population isn't the same length for",object@survey_name[i],'or',object@popn_name[i])
    } else {
      TRUE
    }
  }
})

