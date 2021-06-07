#' SurveyData objects
#'
#' @name SurveyData
#' @export
#' @description
#' A `SurveyData` object represents a survey and its metadata. The survey itself
#' is a data frame. The survey metadata consists of the text of the survey
#' questions, the allowed response values, and optionally survey weights and a
#' survey design specification.
#'
#' @examples
#'
#' head(feline_survey)
#' feline_prefs <- SurveyData$new(
#'   data = feline_survey,
#'   questions = list(
#'     age1 = "Please identify your age group",
#'     gender = "Please select your gender",
#'     pet_own = "Which pet do you own?",
#'     y = "Response"
#'   ),
#'   responses = list(
#'     age1 = levels(feline_survey$age1),
#'     gender = levels(feline_survey$gender),
#'     pet_own = levels(feline_survey$pet_own),
#'     y = c("no","yes")
#'   ),
#'   weights = feline_survey$wt,
#'   design = list(ids =~1)
#' )
#' feline_prefs$print()
#' feline_prefs$n_questions()
#'
#' head(approx_popn)
#' popn_obj <- SurveyData$new(
#'   data = approx_popn,
#'   questions = list(
#'     age2 = "Which age group are you?",
#'     gender = "Gender?",
#'     pet_pref = "Which pet would you like to own?"
#'   ),
#'   # order doesn't matter (gender before age2 here) because
#'   # the list has the names of the variables
#'   responses = list(
#'     gender = levels(approx_popn$gender),
#'     age2 = levels(approx_popn$age2),
#'     pet_pref = levels(approx_popn$pet_pref)
#'   ),
#'   weights = approx_popn$wt
#' )
#' popn_obj$print()
#'
SurveyData <- R6::R6Class(
  classname = "SurveyData",
  private = list(
    survey_data_ = data.frame(NULL),
    questions_ = list(),
    responses_ = list(),
    weights_ = numeric(),
    design_ = list()
  ),
  public = list(
    #' @description Create a new SurveyData object
    #' @param data A data frame containing the survey data.
    #' @param questions,responses Named lists containing the text of the survey
    #'   questions and the allowed responses, respectively. The names must
    #'   correspond to the names of variables in `data`. See **Examples**. If
    #'   these aren't provided then they will be created internally using all
    #'   factor, character, and binary variables in `data`.
    #' @param weights Optionally, a vector of survey weights.
    #' @param design Optionally, a named list of arguments (except `weights` and
    #'   `data`) to pass to `survey::svydesign()` to specify the survey design.
    initialize = function(data,
                          questions = list(),
                          responses = list(),
                          weights = numeric(),
                          design = list(ids =~1)) {
      if (ncol(data) == 0 || nrow(data) == 0) {
        stop("'data' cannot be empty.", call. = FALSE)
      }

      if (length(questions) == 0 && length(responses) == 0) {
        keep <- function(x) is.factor(x) || is.character(x) || length(unique(x)) == 2
        data_use <- data[, sapply(data, keep), drop = FALSE]
        questions <- setNames(as.list(colnames(data_use)), colnames(data_use))
        responses <- lapply(data_use, function(x) if (is.factor(x)) levels(x) else unique(x))
        warning(
          "No 'questions' and 'responses' provided. ",
          "Using all factor, character, and binary variables in 'data' by default.",
          call. = FALSE
        )
      }
      if (ncol(data) != length(questions) &
          length(questions) == sum(stats::complete.cases(data))) {
        stop("Mismatch between number of data columns and questions.",
             call. = FALSE)
      }
      if (length(responses) != length(questions)) {
        stop("Mismatch between number of survey questions and responses.",
             call. = FALSE)
      }
      if (length(responses) != length(unique(responses))) {
        stop("All elements of 'responses' must be unique.")
      }
      if (length(questions) != length(unique(questions))) {
        stop("All elements of 'questions' must be unique.")
      }

      nms_q <- sort(names(questions))
      nms_r <- sort(names(responses))
      if (is.null(nms_q) || sum(nzchar(nms_q)) != length(nms_q)) {
        stop("All elements of 'questions' and 'responses' list must have names.", call. = FALSE)
      }
      if (length(unique(nms_q)) != length(nms_q)) {
        stop("Names in 'questions' must be unique.", call = FALSE)
      }
      if (!identical(nms_q, nms_r)) {
        stop("Names in 'questions' and 'responses' lists must be the same.")
      }
      if (!all(nms_q %in% colnames(data))) {
        stop("Names of 'questions' much match column names in 'data'.", call. = FALSE)
      }
      questions <- questions[nms_q]
      responses <- responses[nms_q]

      for (j in seq_along(questions)) {
        responses_provided <- sort(responses[[j]])
        if (is.factor(data[[nms_q[j]]])) {
          responses_in_data <- sort(levels(data[[nms_q[j]]]))
        } else {
          responses_in_data <- sort(unique(data[[nms_q[j]]]))
        }
        if (!identical(responses_provided, responses_in_data)) {
          stop(
            "Values in data do not match specified responses for variable '", nms_q[j], "'. ",
            "\nValues in 'data': ", paste(responses_in_data, collapse = ", "),
            "\nValues in 'responses': ", paste(responses_provided, collapse = ", "),
            call. = FALSE
          )
        }
      }

      if (length(weights) != 0) {
        if (length(weights) != nrow(data)) {
          stop("Mismatch between number of data rows and number of weights.",
               call. = FALSE)
        }
        if (anyNA(weights)) {
          stop("NAs not allowed in weights.", call. = FALSE)
        }
      }

      if (!is.list(design) ||
          (is.null(names(design)) || any(!nzchar(names(design))))) {
        stop("'design' must be a named list.", call. = FALSE)
      }
      if (!"ids" %in% names(design)) {
        stop("'design' must contain an element 'ids'.", call. = FALSE)
      }
      if ("weights" %in% names(design)) {
        stop("'design' should not include element 'weights'.")
      }
      if ("data" %in% names(design)) {
        stop("'design' should not include element 'data'.")
      }

      private$questions_ <- questions
      private$responses_ <- responses
      private$weights_ <- weights
      private$design_ <- design
      private$survey_data_ <- data.frame(.key = 1:nrow(data), data)
      invisible(self)
    },

    #' @description Number of observations in the survey data
    n_obs = function() nrow(private$survey_data_),

    #' @description Number of survey questions
    n_questions = function() length(private$questions_),

    #' @description Print a summary of the survey data
    #' @param ... Currently ignored.
    print = function(...) {
      cat("Survey with",
          self$n_obs(), "observations,",
          self$n_questions(), "questions",
          "\n")

      print_survey_design(private$design_, private$weights_, private$survey_data_)
      print_questions_and_responses(private$questions_, private$responses_)
      invisible(self)
    },

    #' @description Add a column to the sample data. This is primarily
    #'   intended for internal use but may occasionally be useful.
    #' @param name,value The name of the new variable (a string) and the
    #' vector of values to add to the data frame.
    add_survey_data_column = function(name, value) {
      if (length(value) != nrow(private$survey_data_)) {
        stop("New variable must have same number of observations as the survey data.",
             call. = FALSE)
      }
      private$survey_data_[[name]] <- value
      invisible(self)
    },

    #' @description Access the data frame containing the sample data.
    #' @param key Should the `.key` column be included? This column just
    #'   indicates the original order of the rows and is primarily intended
    #'   for internal use.
    survey_data = function(key = TRUE) {
      if (key) {
        private$survey_data_
      } else {
        private$survey_data_[, colnames(private$survey_data_) != ".key", drop = FALSE]
      }
    },

    #' @description Access the list of survey questions
    questions = function() private$questions_,
    #' @description Access the list of allowed survey responses
    responses = function() private$responses_,
    #' @description Access the survey weights
    weights = function() private$weights_,
    #' @description Access the survey design
    design = function() private$design_
  )
)


# internal ----------------------------------------------------------------

# print 1-line summary of survey design
print_survey_design <- function(design, weights, data) {
  svy_design <- do.call(survey::svydesign, c(design, list(weights = weights, data = data)))
  svy_design$call <- NULL
  cat(utils::capture.output(print(svy_design))[1], "\n")
}

print_questions_and_responses <- function(questions, responses) {
  for (i in seq_along(questions)) {
    cat("\nColumn label:", names(questions)[i], "\n")
    cat("Question:", questions[[i]], "\n")
    cat("Allowed answers:", paste(responses[[i]], collapse = ", "), "\n")
  }
}
