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

test_that("initializing with >0 questions doesn't error", {
  expect_silent(SurveyMap$new(samp_obj, popn_obj, q1, q2))
})

test_that("add() errors if name already exists", {
  x <- SurveyMap$new(samp_obj, popn_obj, q1)
  expect_error(
    x$add(q1),
    "Survey label 'age' already defined"
  )
})

test_that("validate creates correct levels (example1)", {
  samp_obj <- SurveyData$new(
    data = data.frame(age1 = factor(rep(c("18-25","26-45","46+"),10)),
                      y = rbinom(30,1,.5)),
    questions = list(
      age1 = "Please identify your age group",
      y = "Response"
    ),
    responses = list(
      age1 = c("18-25","26-45","46+"),
      y = c("no","yes")
    ),
    weights = rnorm(30,0,1),
    design = formula("~.")
  )

  popn_obj <- SurveyData$new(
    data = data.frame(age2 = factor(rep(c("18-45","46+"),50))),
    questions = list(
      age2 = "Which age group are you?"
    ),
    responses = list(
      age2 = c("18-45","46+")
    ),
    weights = rnorm(100,0,1),
    design = formula("~.")
  )

  q1 <- SurveyQuestion$new(
    name = "age",
    col_names = c("age1","age2"),
    values_map = list(
      "18-25" = "18-45", "26-45" = "18-45","46+" = "46+")
  )
  expect_silent(ex_mapping1 <- SurveyMap$new(samp_obj, popn_obj, q1))
  ex_mapping1$validate()
  ex_mapping1$mapping()
  expect_setequal(ex_mapping1$.__enclos_env__$private$samp_obj_$mapped_data()$age,
                  c('18-45', "46+"))
  expect_setequal(ex_mapping1$.__enclos_env__$private$popn_obj_$mapped_data()$age,
                  c('18-45', "46+"))
})


test_that("validate creates correct levels (example2)", {
  samp_obj <- SurveyData$new(
    data = data.frame(age1 = factor(rep(c("18-25","26+"),15)),
                      y = rbinom(30,1,.5)),
    questions = list(
      age1 = "Please identify your age group",
      y = "Response"
    ),
    responses = list(
      age1 = c("18-25","26+"),
      y = c("no","yes")
    ),
    weights = rnorm(30,0,1),
    design = formula("~.")
  )

  popn_obj <- SurveyData$new(
    data = data.frame(age2 = factor(rep(c("18-25","26-34","35+"),30))),
    questions = list(
      age2 = "Which age group are you?"
    ),
    responses = list(
      age2 = c("18-25","26-34","35+")
    ),
    weights = rnorm(90,0,1),
    design = formula("~.")
  )

  q1 <- SurveyQuestion$new(
    name = "age",
    col_names = c("age1","age2"),
    values_map = list(
      "18-25" = "18-25", "26+" = "26-34","26+" = "35+")
  )
  expect_silent(ex_mapping2 <- SurveyMap$new(samp_obj, popn_obj, q1))
  ex_mapping2$validate()
  ex_mapping2$mapping()
  expect_setequal(levels(ex_mapping2$.__enclos_env__$private$samp_obj_$mapped_data()$age),
                  c('18-25', "26+"))
  expect_setequal(ex_mapping2$.__enclos_env__$private$popn_obj_$mapped_data()$age,
                  c('18-25', "26+"))
})
