test_that("error if data has zero obs", {
  # 0 rows, 0 cols
  expect_error(
    SurveyData$new(data = data.frame()),
    "'data' cannot be empty"
  )

  # 0 rows, >0 cols
  expect_error(
    SurveyData$new(data = data.frame(x = numeric(0))),
    "'data' cannot be empty"
  )

  # >0 rows, 0 cols
  df <- data.frame(x = 1:10)
  df$x <- NULL
  expect_error(
    SurveyData$new(data = df),
    "'data' cannot be empty"
  )
})

test_that("error if questions not all unique", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group",
                       age2 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1),
                       age2 = levels(feline_survey$age1))
    ),
    "All elements of 'questions' must be unique."
  )
})

test_that("error if questions not all named", {
  # none have names
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list("foo", "bar"),
      responses = list(1:10, 1:10)
    ),
    "All elements of 'questions' and 'responses' list must have names"
  )

  # subset has names
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(a = "foo", b = "bar", "baz"),
      responses = list(a = 1:10, b = 1:10, c = 1:10)
    ),
    "All elements of 'questions' and 'responses' list must have names"
  )
})

test_that("error if question names don't match data names", {
  # none have names
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(banana = "Please identify your age group"),
      responses = list(banana = 1:4)
    ),
    "Names of 'questions' must match column names in 'data'."
  )
})

test_that("error if weights column doesn't exist", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1)),
      weights = "wrong_column"
    ),
    "'weights' must be a string naming a column in 'data'"
  )
})

test_that("error if weights is not a string", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(feline_survey$age1)),
      weights = 3
    ),
    "'weights' must be a string naming a column in 'data'"
  )
})

test_that("error if weights have NAs", {
  d <- feline_survey
  d$wt[3] <- NA
  expect_error(
    SurveyData$new(
      data = d,
      questions = list(age1 = "Please identify your age group"),
      responses = list(age1 = levels(d$age1)),
      weights = "wt"
    ),
    "NAs not allowed in weights"
  )
})

test_that("weights equal to 1 if not specified", {
  suppressWarnings(x <- SurveyData$new(
    data = feline_survey,
    questions = list(age1 = "Please identify your age group"),
    responses = list(age1 = levels(feline_survey$age1))
  ))
  expect_true(all(x$weights() == 1))
})

test_that("weights equal to column if not specified", {
  # weights should be equal to specified column in data
  # and that column should be removed from the survey data
  x <- SurveyData$new(
    data = feline_survey,
    questions = list(age1 = "Please identify your age group"),
    responses = list(age1 = levels(feline_survey$age1)),
    weights = "wt"
  )
  expect_true(all(x$weights() == feline_survey$wt))
  expect_false("wt" %in% colnames(x$survey_data()))
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

test_that("error if unmatched response and levels in data", {
  # if variable is a factor
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(gender = "Please select your gender"),
      responses = list(gender = c("male", "female"))
    ),
    "Values in data do not match specified responses for variable 'gender'"
  )

  # if variable isn't a factor
  feline_survey2 <- feline_survey
  feline_survey2$gender <- as.character(feline_survey2$gender)
  expect_error(
    SurveyData$new(
      data = feline_survey2,
      questions = list(gender = "Please select your gender"),
      responses = list(gender = c("male", "female"))
    ),
    "Values in data do not match specified responses for variable 'gender'"
  )
})

test_that("error if unmatched names of question and responses", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(gender = "Please select your gender"),
      responses = list(sex = list(levels(feline_survey$gender)))
    ),
    "Names in 'questions' and 'responses' lists must be the same."
  )
})

test_that("error if design specified incorrectly", {
  expect_error(
    suppressWarnings(SurveyData$new(feline_survey, design = formula("~."))),
    "'design' must be a named list"
  )
  expect_error(
    suppressWarnings(SurveyData$new(feline_survey, design = list(ids = ~ 1, 2))),
    "'design' must be a named list"
  )
  expect_error(
    suppressWarnings(SurveyData$new(feline_survey, design = list(ids = ~ 1, data = data.frame()))),
    "'design' should not include element 'data'"
  )
  expect_error(
    suppressWarnings(SurveyData$new(feline_survey, design = list(ids = ~ 1, weights = 1))),
    "'design' should not include element 'weights'"
  )
  expect_error(
    suppressWarnings(SurveyData$new(feline_survey, design = list(a = 2))),
    "'design' must contain an element 'ids'"
  )
})


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
  weights = "wt",
  design = list(ids =~1)
)

test_that("object has correct R6 class", {
  expect_r6_class(feline_prefs, "SurveyData")
})

test_that("print output hasn't changed", {
  expect_known_output(
    print(feline_prefs),
    file = test_path("answers/SurveyData-print")
  )
})

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
  expect_match(as.character(feline_prefs$survey_data()[1,3]), 'male')
  expect_equal(as.character(feline_prefs$survey_data()[1,2]), '26-35')
  expect_equal(as.character(feline_prefs$survey_data()[1,4]), 'cat')
  expect_equal(as.character(feline_prefs$survey_data()[1,5]), 'no')
  expect_true(".key" %in% colnames(feline_prefs$survey_data()))
  expect_false(".key" %in% colnames(feline_prefs$survey_data(key = FALSE)))
})

test_that("survey_data factors match input levels", {
  expect_equal(
    feline_prefs$survey_data()$age1 %>% levels(),
    c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-90")
  )
  expect_equal(
    feline_prefs$survey_data()$gender %>% levels(),
    c("male", "female", "nonbinary")
  )
  expect_equal(
    feline_prefs$survey_data()$y %>% levels(),
    c("no", "yes")
  )
})

test_that("survey_data dimensions match", {
  expect_equal(dim(feline_prefs$survey_data()), c(500, 5))
})

test_that("responses returns correct object", {
  expect_equal(feline_prefs$responses()$gender, c("male", "female", "nonbinary"))
})

test_that("questions returns correct object", {
  expect_match(feline_prefs$questions()$gender, "Please select your gender")
  expect_equal(length(feline_prefs$questions()), 4)
})

test_that("print is working correctly", {
  expect_output(feline_prefs$print(), 'Survey with 500 observations')
})

test_that("add_survey_data_column works correctly", {
  expect_error(
    feline_prefs$add_survey_data_column("x", 1),
    "New variable must have same number of observations as the survey data"
  )

  feline_prefs$add_survey_data_column("x", rep(1, 500))
  expect_true("x" %in% colnames(feline_prefs$survey_data()))
})

test_that("questions and responses autogenerated if not provided", {
  # should look for all binary, character, and factor variables
  feline_survey2 <- feline_survey
  feline_survey2$y <- as.numeric(feline_survey$y)
  feline_survey$gender <- as.character(feline_survey$gender)
  expect_warning(
    x <- SurveyData$new(feline_survey2),
    "No 'questions' and 'responses' provided. Using all factor, character, and binary variables in 'data' by default."
  )
  expect_equal(
    x$questions(),
    list(age1 = "age1", gender = "gender", pet_own = "pet_own", y = "y")
  )
  expect_equal(
    x$responses(),
    list(
      age1 = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-90"),
      gender = c("male", "female", "nonbinary"),
      pet_own = c("cat", "kitten", "dog", "puppy"),
      y = c(1, 2)
    )
  )
})


feline_prefs_expanded <- SurveyData$new(
  data = feline_survey,
  questions = list(
    age1 = "Please identify your age group",
    gender = "Please select your gender",
    pet_own = "Which pet do you own?",
    y = "Response"
  ),
  responses = list(
    age1 = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-90"),
    gender = c("male", "female", "nonbinary"),
    pet_own = data.frame(data = c("cat", "kitten", "dog", "puppy"),
                         asked = c(data = c("adult cat > 1 year", "young cat < 1 year", "adult dog > 1 year", "young dog < 1 year"))),
    y = data.frame(data = c("no", "yes"),
                   asked = c("Prefer Dogs","Prefer Cats"))
  ),
  weights = "wt",
  design = list(ids =~1)
)

test_that("dataframe response option works appropriately", {
    expect_known_output(
      print(feline_prefs_expanded),
      file = test_path("answers/SurveyData-print-ExpandedDF")
    )
})

test_that("error thrown if dataframe response given in wrong format", {
  expect_error(
    SurveyData$new(
      data = feline_survey,
      questions = list(
        pet_own = "Which pet do you own?",
        y = "Response"
      ),
      responses = list(
        pet_own = data.frame(data = c("cat", "kitten", "dog", "puppy"),
                             actual = c("adult cat > 1 year", "young cat < 1 year", "adult dog > 1 year", "young dog < 1 year")),
        y = data.frame(data = c("no", "yes"),
                       asked = c("Prefer Dogs","Prefer Cats"))
      ),
      weights = "wt",
      design = list(ids =~1)
    ),
    "If providing responses as data and asked questions, must be in a single two dataframe column with column names `data` and `asked`."
  )
})

