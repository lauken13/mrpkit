#' Print survey object summary
#'
#' @title print_survey
#'
#' @description Prints the survey object or survey mapin a neat format
#'
#' @param SurveyObj or SurveyMap
#'
#' @return A neat summmary.
#'
#' @examples test_obj <- SurveyObj(survey_data = data.frame(a=c(1,2),b=c(3,4)),
#' questions = c("q1","q2"),
#' answers = list(c('a','b'),c("c",'d')),
#' design =  "~.")
#' print_survey(test_obj)
#'
#' test_obj_cluster <- SurveyObj(survey_data = data.frame(a=c(1,2),b=c(3,4)),
#' questions = c("q1","q2"),
#' answers = list(c('a','b'),c("c",'d')),
#' design =  as.formula("~ (1|school)"))
#' print_survey(test_obj_cluster)
#'
#' test_obj_stratified <- SurveyObj(survey_data = data.frame(a=c(1,2),b=c(3,4)),
#' questions = c("q1","q2"),
#' answers = list(c('a','b'),c("c",'d')),
#' design =  as.formula("~ geo_strata"))
#' print_survey(test_obj_stratified)
#'
#' survey_answers <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
#' popn_answers <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
#' test_map <- map_survey_popn(survey_answers, popn_answers, "q1","q1alt")
#' survey_answers2 <- c('M','F')
#' popn_answers2 <- c('Male','Female')
#' test_map <- map_survey_popn(survey_answers2, popn_answers2, "q2","q2alt", survey_map = test_map)
#' print_survey(test_map)
#'
#' @export

setGeneric("print_survey",
           function(x, ..., verbose = TRUE) standardGeneric("print_survey"),
           signature = "x"
)
setMethod("print_survey", "SurveyObj", function(x) {
  cat("Survey containing",nrow(x@survey_data),"observations", "\n")
  if(length(x@design==2) & x@design[2]==".()"){
    cat("Random Sampling Design \n")
  } else if(length(x@design==2)){
    cat("Simple")
    if(is.null(findbars(x@design))){
      cat(" stratified sample with strata",all.vars(terms(x@design))[[1]],"\n")
    } else {
      cat(" cluster sample with cluster",all.vars(terms(x@design))[[1]],"\n")
    }
  }
  for(i in 1:nrow(x@survey_data)){
    cat("Question ",i," (",x@questions[i],")","\n",sep = "")
    cat(x@answers[[i]], "\n")
  }
})

setMethod("print_survey", "SurveyMap", function(x) {
  for(j in 1: length(x@survey_name)){
    cat("==============",'\n')
    cat(x@survey_name[j], "=", x@popn_name[j], '\n')
    cat("--------------",'\n')
    for(i in 1:length(x@survey_answer[[j]])){
      cat(x@survey_answer[[j]][i], "=", x@popn_answer[[j]][i], '\n')
    }
  }
})

