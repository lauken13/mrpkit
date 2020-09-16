#' Map survey answers to population answers
#'
#' @title map_survey_popn
#'
#' @description Helps map responses in surveys
#'
#' @param survey_answers
#' @param popn_answers
#' @param survey_name
#' @param popn_name
#'
#' @return A mapping object
#'
#' @examples
#' survey_answers <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
#' popn_answers <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
#' test_map <- map_survey_popn(survey_answers, popn_answers, "q1","q1alt")
#'
#' survey_answers2 <- c('M','F')
#' popn_answers2 <- c('Male','Female')
#' test_map <- map_survey_popn(survey_answers2, popn_answers2, "q2","q2alt", survey_map = test_map)
#'
#' @export

map_survey_popn <- function(survey_answers, popn_answers,
                            survey_name, popn_name,survey_map=NULL){
  sm <- SurveyMap$new(survey_answer = list(survey_answers),popn_answer = list(popn_answers),
            survey_name = survey_name,
            popn_name = popn_name)
  if(!is.null(survey_map)){
    ll <- length(survey_map$survey_answer)+1
    survey_map$survey_answer[[ll]] <- sm$survey_answer[[1]]
    survey_map$popn_answer[[ll]] <- sm$popn_answer[[1]]
    survey_map$survey_name <- c(survey_map$survey_name,sm$survey_name)
    survey_map$popn_name <- c(survey_map$popn_name,sm$popn_name)
    return(survey_map)
  } else {
    return(sm)
  }
}

