# objects to use in the tests
samp_obj <- SurveyData$new(
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
  design = formula("~.")
)

popn_obj <- SurveyData$new(
  data = approx_popn,
  questions = list(
    age2 = "Which age group are you?",
    gender = "Gender?",
    pet_pref = "Which pet would you like to own?"
  ),
  responses = list(
    gender = levels(approx_popn$gender),
    age2 = levels(approx_popn$age2),
    pet_pref = levels(approx_popn$pet_pref)
  ),
  weights = approx_popn$wt,
  design = formula("~.")
)

q1 <- SurveyQuestion$new(
  name = "age",
  col_names = c("age1","age2"),
  values_map = list(
    "18-25" = "18-35", "26-35" = "18-35","36-45" = "36-55",
    "46-55" = "36-55", "56-65" = "56-65", "66-75" = "66+", "76-90" = "66+"
  )
)
q2 <- SurveyQuestion$new(
  name = "pet",
  col_names = c("pet_own","pet_pref"),
  values_map = list("cat" = "cat", "kitten" = "cat","dog" = "dog","puppy" = "dog")
)
q3 <- SurveyQuestion$new(
  name = "gender",
  col_names = c("gender","gender"),
  values_map = data.frame("male" = "m","female" = "f", "nonbinary" = "nb")
)


test_that("error thrown if inputs are not SurveyData objects", {
  expect_error(
    SurveyMap$new("ABC", popn_obj),
    "samp_obj must be a SurveyData object"
  )
  expect_error(
    SurveyMap$new(samp_obj, "ABC"),
    "popn_obj must be a SurveyData object"
  )
})

test_that("initializing with 0 questions doesn't error", {
  expect_silent(SurveyMap$new(samp_obj, popn_obj))
})

test_that("add() errors if name already exists", {
  x <- SurveyMap$new(samp_obj, popn_obj, q1)
  expect_error(
    x$add(q1),
    "Survey label 'age' already defined"
  )
})
