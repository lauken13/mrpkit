#' SurveyObject
#'
#' @name SurveyObject
#'
#' @description A `SurveyObject` represents a survey and its metadata.
#' The survey itself is a dataframe.
#' The survey metatdata consists of
#'  - per-column questions - a list of strings
#'  - per-column allowed response values - a list of lists
#'  - per-column survey weights, - a vector of numeric weights
#'  - a string that specifies the survey design using lmer forumula syntax
#'
#' Examples of survey design include:
#' ~. a random sample, ~ (1|cluster), a one stage cluster sample, ~ stratum, a stratified sample
#'
#' @examples
#' feline_prefs = SurveyObj$new(feline_survey)

SurveyObj <- R6::R6Class(
    classname = "SurveyObj",
    public = list(
        survey_data = data.frame(NULL),
        questions = character(0),
        responses = list(),
        weights = numeric(),
        design = as.formula("~."),
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
                if (length(responses) != length(questions) &
                    length(responses) != sum(complete.cases(responses))) {
                    stop("mismatch between number of survey questions and answers.",
                     call. = FALSE)
                }
            }
            # allow no weights, else require weights for all columns
            if (length(weights) != 0 & ncol(survey_data) != length(weights)) {
                stop("mismatch between number of survey_data columns and weights.",
                     call. = FALSE)
            }
        },
        print = function(...) {
            cat("Survey containing",nrow(self$survey_data),"observations", "\n")
            if(length(self$design==2)) {
                cat("Random Sampling Design \n")
            } else {
                cat("Simple")
                if(is.null(findbars(self$design))){
                    cat(" stratified sample with strata",all.vars(terms(self$design))[[1]],"\n")
                } else {
                    cat(" cluster sample with cluster",all.vars(terms(self$design))[[1]],"\n")
                }
            }
            for(i in 1:ncol(self$survey_data)){
                cat("Column ",i," label: ",names(self$survey_data)[i],"\n")
                if (length(self$questions) > 0) {
                    cat("Question ",i," (",self$questions[i],")","\n")
                    cat("Allowed answers: ", paste(self$answers[[i]],collapse=", "), "\n")
                }
            }
            invisible(self)
        },
        summary = function(...) {
            for(i in 1:ncol(self$survey_data)){
                codes_used = names(table(self$survey_data[i]))
                cat("Column ",i," label: ",names(self$survey_data)[i],"\n")
                cat("\t", length(codes_used), " responses, values: ")
                cat(paste(codes_used, collapse=", "), "\n")
            }
        }
    )
)
