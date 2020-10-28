#' SurveyQuestion
#'
#' @name QuestionMap
#'
#' @description A `QuestionMap` holds the mapping for one question
#' or demographic between the survey and population dataset.
#' The label maps the survey column name to the population column name.
#' The values map the survey response values to the population values.
#' If there is a meaningful ordering over the values,
#' they should be listed in that order, either descending or ascending.
#'
#' @examples
#' q1_map <- SurveyQuestion$new(names= list("Sex"="s"),
#'                               values= list("female"= 1,
#'                                            "male" = 2,
#'                                            "n/a" = 3))
#'  
SurveyQuestion <- R6::R6Class(
    classname = "SurveyQuestion",
    public = list(
        s2p_names = list(),
        s2p_values = list(),
        initialize = function(names = list(),
                              values = list()) {
            self$s2p_names = names  # validation:  must be non-null
            self$s2p_values = values  # validation:  pairs must be unique
        },
        survery_name = function() {
            names(self$s2p_names)
        },       
        popn_name = function() {
            self$s2p_names
        },       
        survery_values = function() {
            names(self$s2p_values)
        },       
        popn_values = function() {
            self$s2p_values
        }
    )
)
