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

  comparison_classes <- tibble(classes = correct_class)

  if (janitor::compare_df_cols_same(survey_dataset, comparison_classes) == TRUE)
  {
    return(survey_dataset)
  } else {
    return("Column classes do not match")
  }
}


