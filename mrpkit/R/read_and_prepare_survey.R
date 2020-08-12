#' Read and prepare survey data
#'
#' @title read_and_prepare_survey
#'
#' @description This gets a CSV and checks whether the list of the variables is correct.
#'
#' @param dataset A CSV file in which every row is a response and every column is a variable.
#' @param variables A list that specifies the necessary columns.
#' @param class A list that specifies the class of each of the necessary columns.
#'
#' @return survey_dataset A dataset that is reduced to essentials and properly formatted.
#'
#' @examples TBD
#'
#' @export

read_and_prepare_survey <- function(dataset_of_interest, desired_variables, correct_class) {


  survey_dataset <- readr::read_csv(dataset_of_interest)

  survey_dataset <-
    survey_dataset %>%
    select(desired_variables)

  desired_classes <- tibble(desired = correct_class)

  actual_classes <-
    survey_dataset %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather(variable, class)

  actual_vs_desired <- cbind(actual_classes, desired_classes)
  actual_vs_desired <- actual_vs_desired %>%
    mutate(class_matches = if_else(class == desired, TRUE, FALSE)) %>%
    filter(class_matches == FALSE) %>%
    select(-class_matches)

  if (nrow(actual_vs_desired) > 0)
  {
    return(actual_vs_desired)
  } else {
    return(survey_dataset)
  }

}

