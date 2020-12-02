#' SurveyObject
#'
#' @name SurveyObject
#' @export
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
#' feline_prefs = SurveyObj$new(feline_survey[,c("age1","gender","pet_own","y")],
#' questions = c("Please identify your age group","Please select your gender","Which pet do you own?", "Response"),
#' responses = list(levels(feline_survey$age1),levels(feline_survey$gender),levels(feline_survey$pet_own),c("no","yes")),
#' weights = feline_survey$wt,
#' design = formula("~."))
#' feline_prefs$print()
#'popn_obj = SurveyObj$new(approx_popn[,c("age2","gender","pet_pref")],
#' questions = c("Which age group are you?","Gender?","Which pet would you like to own?"),
#' responses = list(levels(approx_popn$age2),levels(approx_popn$gender),levels(approx_popn$pet_pref)),
#' weights = approx_popn$wt,
#' design = formula("~."))
#' popn_obj$print()

SurveyObj <- R6::R6Class(
    classname = "SurveyObj",
    public = list(
        survey_data = data.frame(NULL),
        poststrat = data.frame(NULL),
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
                if (length(responses) != length(questions)) {
                    stop("mismatch between number of survey questions and answers.",
                     call. = FALSE)
                }
            }
            # allow no weights, else require weights for all columns
            if (length(weights) != 0 & nrow(survey_data) != length(weights)) {
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
                    cat("Allowed answers: ", paste(self$responses[[i]],collapse=", "), "\n")
                }
            }
            invisible(self)
        }
    )
)
