library(stringr)
source("../../R/SurveyMap.R")

context("survey-map")
survey_values <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
popn_values <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
survey_values2 <- c('M','F')
popn_values2 <- c('Male','Female')

test_that("create SurveyMap, no args", {
    smap = SurveyMap$new()
    expect_output(smap$print(),"empty mapping")
})

test_that("create SurveyMap, 1 mapping", {
    smap <- SurveyMap$new("age_s", "age_p", survey_values, popn_values)
    expect_output(smap$print(),"age_s = age_p")
    expect_output(smap$print(),"18-25 = 18-35")
})    

test_that("add mapping", {
    smap <- SurveyMap$new("age_s", "age_p", survey_values, popn_values)
    smap$add("sex_s", "sex_p", survey_values2, popn_values2)
    expect_output(smap$print(),"M = Male")
})

test_that("create SurveyMap, 2 mappings", {
    smap2 = SurveyMap$new(c("age_s", "sex_s"),
                          c("age_p", "sex_p"),
                          list(survey_values, survey_values2),
                          list(popn_values, popn_values2))
    expect_output(smap2$print(),"age_s = age_p")
    expect_output(smap2$print(),"M = Male")
})

test_that("delete mapping", {
    smap2 = SurveyMap$new(c("age_s", "sex_s"),
                          c("age_p", "sex_p"),
                          list(survey_values, survey_values2),
                          list(popn_values, popn_values2))
    smap2$delete("age_s")
    s = capture_output({
        smap2$print()
    })
    expect_equal(1, str_count(s, "--------------"))
})

test_that("rename mapping", {
    smap3 = SurveyMap$new(c("age_s", "sex_s"),
                          c("age_p", "sex_p"),
                          list(survey_values, survey_values2),
                          list(popn_values, popn_values2))
    survey_values3 <- c('H','F')
    popn_values3 <- c('Homme','Femme')
    smap3$replace("sex_s", "sex_s", "sex_p", survey_values3, popn_values3)
    expect_output(smap3$print(),"age_s = age_p")
    expect_output(smap3$print(),"H = Homme")
})

