test_that("error thrown if weights have NAs", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1)),
      weights = rep(NA, nrow(feline_survey)),
      design = formula("~.")
    ),
    "NAs not allowed in weights"
  )
})

test_that("error thrown if length(weights) != nrow(data)", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1)),
      weights = 1:3,
      design = formula("~.")
    ),
    "Mismatch between number of data rows and number of weights"
  )
})
