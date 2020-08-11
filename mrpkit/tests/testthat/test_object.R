context("give a context – human readable")

library(mrpkit)
test_that("Survey object ensures correct size", {
    expect_error(SurveyObj(survey_data = data.frame(a=c(1,2)),questions = c("q1","q2"), answers = list(c('a','b'),c("c",'d'))),
                "@survey_data and @questions must be same length")
    expect_error(SurveyObj(survey_data = data.frame(a=c(1,2),b=c(3,4)),questions = c("q1","q2"), answers = list(c('a','b'))),
               "@answers and @questions must be same length")
  })
