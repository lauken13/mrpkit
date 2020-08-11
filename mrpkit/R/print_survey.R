#' Print survey object summary
#'
#' @title print_survey
#'
#' @description Prints the survey object in a neat format
#'
#' @param SurveyObj A survey object.
#'
#' @return A neat summmary.
#'
#' @examples test_obj <- SurveyObj(survey_data = data.frame(a=c(1,2),b=c(3,4)),
#' questions = c("q1","q2"),
#' answers = list(c('a','b'),c("c",'d')))
#' PrintSurvey(test_obj)
#'
#' @export

setGeneric("print_survey",
           function(x, ..., verbose = TRUE) standardGeneric("print_survey"),
           signature = "x"
)
setMethod("print_survey", "SurveyObj", function(x) {
  cat("Survey containing",nrow(x@survey_data),"observations", "\n")
  for(i in 1:nrow(x@survey_data)){
    cat("Question ",i," (",x@questions[i],")","\n",sep = "")
    cat(x@answers[[i]], "\n")
  }
})
