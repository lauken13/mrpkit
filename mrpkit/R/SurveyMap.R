#' SurveyMap
#'
#' @name SurveyMap
#'
#' @description A `SurveyMap` object holds the mapping
#' between a set of items in a survey and a population dataset.
#' The label is the item label in each dataset and the values
#' is a list of all possible values.   The values for the survey
#' and population must be aligned, i.e., the lists must have the
#' same number of elements and the values at index i in each list
#' are equivalent.  If there is a meaningful ordering over the values,
#' they should be listed in that order, either descending or ascending.
#'
#' @examples
#' survey_values <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
#' popn_values <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
#' test_map <- SurveyMap$new("age_s", "age_p", survey_values, popn_values)
#'
#' survey_values2 <- c('M','F')
#' popn_values2 <- c('Male','Female')
#' test_map$add_item("sex_s", "sex_p", survey_values2, popn_values2)
#'
#' test2_map$new(c("age_s", "sex_s"), c("age_p", "sex_p"),
#'               c(list(survey_values, survey_values2), list(pop_values, pop_values2))

SurveyMap <- R6::R6Class(
    classname = "SurveyMap",
    public = list(
        survey_labels = character(0),
        popn_labels = character(0),
        survey_values = list(),
        popn_values = list(),
        initialize = function(survey_labels,
                              popn_labels,
                              survey_values,
                              popn_values) {
            self$survey_labels <- survey_labels
            self$popn_labels <- popn_labels
            if (is.list(survey_values))
                self$survey_values = survey_values
            else
                self$survey_values <- list(survey_values)
            if (is.list(popn_values))
                self$popn_values = popn_values
            else
                self$popn_values <- list(popn_values)
            if (length(survey_labels) != length(popn_labels)) {
                stop("Survey and population must have same number of items.",
                     call. = FALSE)
            }
            if (length(survey_labels) == 1) {
                if (length(survey_values) != length(popn_values)) {
                    stop("Survey and population must have same number of values.",
                         call. = FALSE)
                }
            } else {
                for(i in 1: length(self$survey_labels)){
                    if (length(self$survey_values[i]) != length(self$popn_values[i])) {
                        stop("Survey and population must have same number of values. ",
                             "Check values for ", self$survey_labels[i], ".",
                             call. = FALSE)
                    }
                }
            }
        },
        print = function(...) {
            for(i in 1: length(self$survey_labels)){
                cat("==============",'\n')
                cat(self$survey_labels[i], "=", self$popn_labels[i], '\n')
                cat("--------------",'\n')
                for(j in 1:length(self$survey_values[[i]])){
                    cat(self$survey_values[[i]][j], "=", self$popn_values[[i]][j], '\n')
                }
            }
            invisible(self)
        },
        add = function(survey_label,
                       popn_label,
                       survey_values,
                       popn_values) {
            if (survey_label %in% self$survey_labels) {
                stop("Survey label: ", survey_label, " already defined.  ",
                     "Use function 'replace' instead. ", call. = FALSE)
            }
            if (popn_label %in% self$popn_labels) {
                stop("Population label: ", popn_label, " already defined.  ",
                     "Use function 'replace' instead. ", call. = FALSE)
            }
            if (length(survey_values) != length(popn_values)) {
                stop("Survey and population must have same number of values.",
                     call. = FALSE)
            }
            self$survey_labels[length(self$survey_labels) + 1] = survey_label
            self$popn_labels[length(self$popn_labels) + 1] = popn_label
            self$survey_values[[length(self$survey_values) + 1]] = survey_values
            self$popn_values[[length(self$popn_values) + 1]] = popn_values
        },
        delete = function(survey_label) {
            if (!(survey_label %in% self$survey_labels)) {
                stop("Survey label: ", survey_label, " not defined.  ",
                     call. = FALSE)
            }
            idx = match(survey_label, self$survey_labels)
            self$survey_labels = self$survey_labels[-idx]
            self$popn_labels = self$popn_labels[-idx]
            self$survey_values = self$survey_values[-idx]
            self$popn_values = self$popn_values[-idx]
        },

        replace = function(current_survey_label,
                           new_survey_label,
                           popn_label,
                           survey_values,
                           popn_values) {
            self$delete(current_survey_label)
            self$add(new_survey_label,
                           popn_label,
                           survey_values,
                           popn_values)
        }
    )
)
