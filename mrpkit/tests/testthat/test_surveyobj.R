source("../../R/SurveyObj.R")

context("survey-object")


test_that("create SurveyObj missing args", {
    expect_error(SurveyObj$new())
})

test_that("create SurveyObj empty df", {
    empty_df = data.frame()
    expect_error(SurveyObj$new(empty_df),
                 "survey_data cannot be empty")
})
    
test_that("create SurveyObj df only", {
    ex_survey = read.csv("../../data/ex_survey_cats.csv")
    sobj = SurveyObj$new(ex_survey)
    expect_output(sobj$print(),"Survey containing 1200 observations")
    expect_output(sobj$print(),"Random Sampling Design")
})

test_that("create SurveyObj with questions, responses", {
    expect_error(SurveyObj$new(survey_data = data.frame(a=c(1,2)),
                           questions = c("q1","q2"),
                           responses = list(c('a','b'),c("c",'d'))),
                 "mismatch between number of survey_data columns and questions.")
    expect_error(SurveyObj$new(survey_data = data.frame(a=c(1,2),b=c(3,4)),
                           questions = c("q1","q2"),
                           responses = list(c('a','b'))),
                 "mismatch between number of survey questions and answers.")
})
