# initialize() not done
# feline_prefs$add_mapped_data_column() not done
# feline_prefs$add_survey_data_column() not done
# feline_prefs$design() not done


feline_prefs <- SurveyData$new(
  data = feline_survey,
  questions = list(
    age1 = "Please identify your age group",
    gender = "Please select your gender",
    pet_own = "Which pet do you own?",
    y = "Response"
  ),
  responses = list(
    age1 = levels(feline_survey$age1),
    gender = levels(feline_survey$gender),
    pet_own = levels(feline_survey$pet_own),
    y = c("no","yes")
  ),
  weights = feline_survey$wt,
  design = list(ids =~1)
)

test_that("n_obs is working correctly", {
  expect_equal(feline_prefs$n_obs(), 500)
})

test_that("n_questions is working correctly", {
  expect_equal(feline_prefs$n_questions(), 4)
})


test_that("weights is working correctly", {
  expect_equal(feline_prefs$weights()[1], 98.98852)
})

test_that("survey_data is working correctly", {
  expect_equal(feline_prefs$survey_data()[1,6], 98.98852)
  expect_match(as.character(feline_prefs$survey_data()[1,3]), 'male')
  expect_equal(as.character(feline_prefs$survey_data()[1,2]), '26-35')
  expect_equal(as.character(feline_prefs$survey_data()[1,4]), 'cat')
  expect_equal(as.character(feline_prefs$survey_data()[1,5]), 'no')
  expect_error(mwhwhwhwhwhw$survey_data())
})

test_that("responses is working correctly", {
  expect_equal(length(feline_prefs$responses()$gender), 3)
})

test_that("questions is working correctly", {
  expect_match(feline_prefs$questions()$gender, "Please select your gender")
  expect_equal(length(feline_prefs$questions()), 4)
})

test_that("print is working correctly", {
  expect_output(feline_prefs$print(), 'Survey with 500 observations')
})

test_that("print is working correctly", {
  expect_equal(feline_prefs$mapped_data()$.key[1], 1)
  expect_equal(feline_prefs$mapped_data()$.key[2], 2)
})

test_that("clone is working correctly", {
  twins <- feline_prefs$clone()
  expect_equal(twins$mapped_data()$.key[1], 1)
  expect_equal(twins$mapped_data()$.key[2], 2)
})


