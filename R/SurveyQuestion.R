#' SurveyQuestion
#'
#' @name SurveyQuestion
#' @export
#'
#' @description An [R6][R6::R6Class] `SurveyQuestion` object holds the mapping
#'   for one question or demographic between the survey and population dataset.
#'
#' @examples
#' q1 <- SurveyQuestion$new(
#'   name = "age",
#'   col_names = c("age1","age2"),
#'   values_map = list(
#'     "18-25" = "18-35", "26-35" = "18-35","36-45" = "36-55",
#'     "46-55" = "36-55", "56-65" = "56-65", "66-75" = "66+", "76-90" = "66+"
#'   )
#' )
#' print(q1)
#'
#' q2 <- SurveyQuestion$new(
#'   name = "pet",
#'   col_names = c("pet_pref","pet_own"),
#'   values_map = list("cat" = "cat", "kitten" = "cat","dog" = "dog","dog" = "puppy")
#' )
#' q3 <- SurveyQuestion$new(
#'   name = "gender",
#'   col_names = c("gender", "gender"),
#'   values_map = list("male" = "m","female" = "f", "nonbinary" = "nb")
#' )
#'
SurveyQuestion <- R6::R6Class(
  classname = "SurveyQuestion",
  public = list(
    name = character(),
    col_names = character(),
    values = list(),

    #' @description Create a new `SurveyQuestion`
    #' @param name The name of the underlying construct. For example if the
    #'   sample data uses the name `age1` and the population data uses
    #'   the name `age2` then `name` could be `age`.
    #' @param col_names A character vector of length two giving the column names
    #'   of the question in the sample data (first element) and the population
    #'   data (second element).
    #' @param values_map A named list where the names correspond to the
    #'   responses to the question in the sample data and the values correspond
    #'   to the responses in the population data. If there is a meaningful
    #'   ordering over the values, they should be listed in that order, either
    #'   descending or ascending.
    #' @return A `SurveyQuestion` object that can be added to a [`SurveyData`]
    #'   object.
    #'
    initialize = function(name, col_names, values_map) {
      if (is.na(name)) {
        stop("Please specify a general name for this construct", call. = FALSE)
      }
      if (anyNA(col_names)) {
        stop("Please specify the sample and population column names.", call. = FALSE)
      }
      if (length(col_names) != 2) {
        stop("'col_names' must have length 2.", call. = FALSE)
      }
      if (anyNA(names(values_map))) {
        stop("Missing sample levels", call. = FALSE)
      }
      if (anyNA(as.character(unlist(values_map)))) {
        stop("Missing population levels", call. = FALSE)
      }

      self$name <- name
      self$col_names <- col_names
      self$values <- data.frame(names(values_map), as.character(unlist(values_map)),
                                stringsAsFactors = TRUE)
      if (sum(duplicated(self$values)) > 0) {
        warning("Duplicated mapping in values, removing duplciates", call. = FALSE)
        self$values <- self$values[!duplicated(self$values), ]
      }
      if (sum(duplicated(self$values[, 2])) > 0 ||
          sum(duplicated(self$values[, 1])) > 0) {
        dup_labels_pop <- which(duplicated(self$values[, 2]))
        dup_labels_samp <- which(duplicated(self$values[, 1]))
        for (d in dup_labels_pop) {
          pop_loc_duplicates <- self$values[, 2] %in% self$values[d, 2]
          for (b in dup_labels_samp) {
            samp_loc_duplicates <- self$values[, 1] %in% self$values[b, 1]
            if (sum(pop_loc_duplicates & samp_loc_duplicates) > 0) {
              stop("Package can only handle many to one mappings", call. = FALSE)
            }
          }
        }
        for (b in dup_labels_samp) {
          samp_loc_duplicates <- self$values[, 1] %in% self$values[b, 1]
          for (d in dup_labels_pop) {
            pop_loc_duplicates <- self$values[, 2] %in% self$values[d, 2]
            if (sum(pop_loc_duplicates & samp_loc_duplicates) > 0) {
              stop("Package can only handle many to one mappings", call. = FALSE)
            }
          }
        }
      }
      names(self$values) <- col_names
      invisible(self)
    },

    #' @description Print a `SurveyQuestion`.
    #' @param ... Currently ignored.
    #' @return The `SurveyQuestion` object, invisibly.
    print = function(...) {
      cat("--------------",'\n')
      cat(self$name,'\n')
      cat(self$col_names[1], "=", self$col_names[2], '\n')
      cat("--------------",'\n')
      for (j in 1:nrow(self$values)) {
        cat(as.character(self$values[j, 1]), "=",
            as.character(self$values[j, 2]), '\n')
      }
      invisible(self)
    }
  )
)
