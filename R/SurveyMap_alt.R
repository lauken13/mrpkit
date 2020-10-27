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
#' survey_values <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
#' popn_values <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
#' q1 <- question$new(name = "age", col_names = c(samp = "age1",popn = "age2"), values = data.frame(age1 = survey_values,age2 = popn_values))
#' survey_values2 <- c('cat','cat','dog','dog')
#' popn_values2 <- c('cat','kitten','dog','puppy')
#' q2 <- question$new(name = "pet", col_names = c(samp = "pet_pref",popn = "pet_own"), values = data.frame(pet_pref = survey_values2,pet_own = popn_values2))
#' survey_values3 <- c('male','female','nonbinary')
#' popn_values3 <- c('m','f','nb')
#' q3 <- question$new(name = "gender", col_names = c(samp = "gender",popn = "gender"), values = data.frame(gender = survey_values3,gender = popn_values3))
#'
#' tmp_map <- SurveyMap_alt$new(q1,q2)
#' print(tmp_map)
#' tmp_map$add(q3)
#' print(tmp_map)
#' tmp_map$delete(q3)
#' print(tmp_map)
#' tmp_map$replace(q1,q3)
#' print(tmp_map)

question <- R6::R6Class(
  "question",
  list(
    name = character(),
    col_names = character(),
    values = data.frame(),
    initialize = function(name,col_names,values) {
      self$name <- name
      self$col_names  <- col_names
      self$values <- values
      names(self$values) <- col_names
      invisible(self)
    }
  )
)


SurveyMap_alt <- R6::R6Class(
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
        self$item_map[[ll_length+1]]<- list(...)[[i]]
        names(self$item_map)[ll_length+1] <- self$item_map[[ll_length+1]]$name
      }
      invisible(self)
    },
    delete = function(rm_val){
      if(inherits(rm_val, "question")){
        loc_id <- names(self$item_map) %in% rm_val$name
      }else {
        loc_id <- names(self$item_map) %in% rm_val
      }
      self$item_map[[which(loc_id)]] = NULL
      invisible(self)
    },
    replace = function(old_question, new_question) {
      self$delete(old_question)
      self$add(new_question)
    }
  )
)

