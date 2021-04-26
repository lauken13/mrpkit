test_that("error thrown if weights have NAs", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1)),
      weights = rep(NA, nrow(feline_survey))
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
      weights = 1:3
    ),
    "Mismatch between number of data rows and number of weights"
  )
})

test_that("error if duplicate question names", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group", age1 = "ABC"),
      responses = list(age1 = levels(feline_survey$age1), age1 = "ABC")
    ),
    "Names in 'questions' must be unique"
  )
})

test_that("error if unequal number of questions and responses", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group")
    ),
    "Mismatch between number of survey questions and responses"
  )
})


test_that("error if unmatch response and levels in data", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(gender = "Please select your gender"),
      responses = list(gender = c("male", "female"))
    ),
    "?? the error in SurveyData.R haven't been specified?"
  )
})


test_that("error if unmatch names of question and responses", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(gender = "Please select your gender"),
      responses = list(sex = list(levels(feline_survey$gender)))
    ),
    "Names in 'questions' and 'responses' lists must be the same."
  )
})
