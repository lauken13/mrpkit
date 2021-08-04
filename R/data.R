#' Example survey for package testing
#' @source Created in CreateTestData.R
#' @format A data frame with columns:
#' \describe{
#'  \item{age1}{Age group}
#'  \item{gender}{Gender}
#'  \item{pet_own}{Pet ownership}
#'  \item{y}{Example binary outcome variable}
#'  \item{wt}{Raked and trimmed survey weight}
#' }
#'
#' @examples
#' str(feline_survey)
#'
"feline_survey"

#' Example approximate population (modelled after ACS)
#'
#' @source Created in CreateTestData.R
#' @format A data frame with columns:
#' \describe{
#'  \item{age2}{Age group}
#'  \item{gender}{Gender}
#'  \item{pet_pref}{Pet preference}
#'  \item{wt}{Raked and trimmed survey weight}
#' }
#'
#' @examples
#' str(approx_popn)
#'
"approx_popn"


#' Simulated Survey Data for Shape World
#'
#' This is a simulated survey of whether Shape World voters prefer the Box Party
#' (BP) or the Circle Party (CP) in the 2020 election. The data also include
#' several demographic variables and the party the respondent voted for in the
#' previous election. We use this data throughout the documentation to
#' demonstrate the usage of the package.
#'
#' @source Created in ShapeWorldVoters.R
#'
#' @format A data frame with columns:
#'
#' \describe{
#'  \item{age}{Age group}
#'  \item{gender}{Gender}
#'  \item{vote_for}{Party voted for in the 2016 election}
#'  \item{highest_educ}{Highest grade of education completed}
#'  \item{state}{State}
#'  \item{y}{Preference for BP over CP in 2020 election}
#'  \item{wt}{Raked and trimmed survey weight}
#' }
#' @examples
#'
#'  shape_survey
#'
"shape_survey"

#' Simulated approximate population data for the Shape World survey
#'
#' Simulated approximate population data for the [Shape World][shape_survey]
#' voters survey (modeled after ACS).
#'
#' @source Created in ShapeWorldVoters.R
#'
#' @format A data frame with columns:
#'
#' \describe{
#'  \item{age_group}{Age group}
#'  \item{gender}{Gender}
#'  \item{vote_pref}{Party voted for in the 2016 election}
#'  \item{education}{Highest grade of education completed}
#'  \item{state}{State}
#'  \item{wt}{Raked and trimmed survey weight}
#' }
#'
#' @examples
#' approx_voters_popn
#'
"approx_voters_popn"

