expect_r6_class <- function(x, class) {
  expect_true(R6::is.R6(x))
  expect_true(inherits(x, class))
}

example_survey_map <- function() {
  samp <- SurveyData$new(
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

  popn <- SurveyData$new(
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
  SurveyMap$new(samp, popn, q1, q2, q3)
}

example_survey_fit <- function() {
  x <- example_survey_map()
  x$validate()
  suppressWarnings(x$mapping())
  x$tabulate()
  x$fit(lme4::glmer, formula = y ~ (1|age) + (1|gender), family = "binomial")
}
