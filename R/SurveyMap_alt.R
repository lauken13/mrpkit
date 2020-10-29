#' SurveyMap
#'
#' @name SurveyMap_alt
#'
#' @description A `SurveyMap_alt` object holds the mapping
#' between a set of items in a survey and a population dataset.
#' The label is the item label in each dataset and the values
#' is a list of all possible values.   The values for the survey
#' and population must be aligned, i.e., the lists must have the
#' same number of elements and the values at index i in each list
#' are equivalent.  If there is a meaningful ordering over the values,
#' they should be listed in that order, either descending or ascending.
#'
#' @examples
#'
#' tmp_map <- SurveyMap_alt$new(q1,q2)
#' print(tmp_map)
#' tmp_map$add(q3)
#' print(tmp_map)
#' tmp_map$delete(q3)
#' print(tmp_map)
#' tmp_map$delete("pet")
#' print(tmp_map)
#' tmp_map$add(q2)
#' print(tmp_map)
#' tmp_map$replace(q1,q3)
#' print(tmp_map)

# add a method called validate_questions on the survey map
# need methods that minimize the connection between two them
# Add sample object
# Some method that creates the dataframe that will be passed to the modelling function
# Need to take in weights
# validate method

SurveyMap_alt <- R6::R6Class(
  #Add in a named data frame for sample and population
  classname  = "survey",
  list(
    item_map = list(),
    initialize = function(...) {
      self$item_map <- list(...)
      for(i in 1:length(self$item_map)){
        names(self$item_map)[i] <- self$item_map[[i]]$name
      }
      invisible(self)
    },
    print = function(...) {
      if (length(self$item_map) > 0) {
        for(i in 1: length(self$item_map)){
          cat("==============",'\n')
          cat(self$item_map[[i]]$col_names[1], "=", self$item_map[[i]]$col_names[2], '\n')
          cat("--------------",'\n')
          for(j in 1:nrow(self$item_map[[i]]$values)){
            cat(as.character(self$item_map[[i]]$values[j,1]), "=", as.character(self$item_map[[i]]$values[j,2]), '\n')
          }
        }
      } else {
        cat("==============",'\n')
        cat("empty mapping",'\n')
      }
      invisible(self)
    },
    add = function(...){
      for(i in 1:length(list(...))){
        ll_length <- length(self$item_map)
        if (list(...)[[i]]$name %in% names(self$item_map)) {
          stop("Survey label: ", list(...)[[i]]$name, " already defined.  ",
               "Use function 'replace' instead. ", call. = FALSE)
        }
        self$item_map[[ll_length+1]]<- list(...)[[i]]
        names(self$item_map)[ll_length+1] <- self$item_map[[ll_length+1]]$name
      }
      invisible(self)
    },
    delete = function(...){
      tmp_list <- list(...)
      for(i in length(tmp_list)){
        if(inherits(tmp_list[[i]], "question")){
          loc_id <- names(self$item_map) %in% tmp_list[[i]]$name
          loc_name <- tmp_list$name[[i]]
        }else {
          loc_id <- names(self$item_map) %in% tmp_list[[i]]
          loc_name <- tmp_list[[i]]
        }
        if(sum(loc_id)==0){
          stop("Survey label: ", loc_name, " not defined.  ",
                                                call. = FALSE)
        }else{
          self$item_map[[which(loc_id)]] = NULL
        }
      }
      invisible(self)
    },
    replace = function(old_question, new_question) {
      self$delete(old_question)
      self$add(new_question)
    }
  )
)

