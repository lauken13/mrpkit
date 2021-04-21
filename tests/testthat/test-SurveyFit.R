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
ex_map <- SurveyMap$new(samp_obj = samp_obj, popn_obj = popn_obj, q1)


# Small data fits for testing purposes:
if (requireNamespace("rstanarm", quietly = TRUE)){
  rstanarm_fit <- rstanarm::stan_glmer(y ~ (1|age1) + (1|gender),
                                       data = feline_survey[1:300,],
                                       family = binomial(link = "logit"),
                                       refresh = 0, iter = 500, chains = 2)
}
if (requireNamespace("lme4", quietly = TRUE)){
  glmer_fit <- lme4::glmer(y ~ (1|age1) + (1|gender), data = feline_survey,
                  family = binomial(link = "logit"))
}
glm_fit <- stats::glm(y ~ age1 + gender, data = feline_survey,
                        family = binomial(link = "logit"))
lm_fit <- stats::lm(y ~ age1 + gender, data = feline_survey)

example_newdata <- feline_survey[sample.int(nrow(feline_survey),150),]

test_that("sim_posterior_epred throws error if not given glmerMod object", {
  skip_if_not_installed("merTools")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  expect_error(
    sim_posterior_epred(rstanarm_fit,newdata = example_newdata),
    "Object must have class 'glmerMod'."
  )
  expect_error(
    sim_posterior_epred(glm_fit,newdata = example_newdata),
    "Object must have class 'glmerMod'."
  )
  expect_error(
    sim_posterior_epred(lm_fit,newdata = example_newdata),
    "Object must have class 'glmerMod'."
  )
  expect_equal(
    dim(sim_posterior_epred(glmer_fit,newdata = example_newdata)),
    c(150,4000)
  )
  expect_equal(
    sum(is.na(dim(sim_posterior_epred(glmer_fit,newdata = example_newdata)))),
    0)
})

test_that("Error if not fitting a bernoulli/binomial model", {
  skip_if_not_installed("rstanarm")
  expect_error(
    ex_map$fit(
    fun = rstanarm::stan_glmer,
    formula = y ~ (1|age) + (1|gender),
    refresh = 100,
    cores = 2
    ),
    "Currently only binomial and bernoulli models are supported."
  )
  skip_if_not_installed("lme4")
  expect_error(
    ex_map$fit(
      fun = lme4::glmer,
      formula = y ~ (1|age) + (1|gender)
    ),
    "Currently only binomial and bernoulli models are supported."
  )
  skip_if_not_installed("brms")
  expect_error(
    ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender)
    ),
    "Currently only binomial and bernoulli models are supported."
  )

})

test_that("Error if data hasn't been mapped yet",{
  skip_if_not_installed("lme4")
  expect_error(
    ex_map$fit(
      fun = lme4::glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Mapped data not found.", fixed = TRUE
  )
  skip_if_not_installed("rstanarm")
  expect_error(
    ex_map$fit(
      fun = rstanarm::stan_glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Mapped data not found.", fixed = TRUE
  )
  skip_if_not_installed("brms")
  expect_error(
    ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Mapped data not found.", fixed = TRUE
  )
})

#Create the mapped data
ex_map$mapping()

test_that("Error if poststrat matrix hasn't been created yet",{
  skip_if_not_installed("lme4")
  expect_error(
    ex_map$fit(
      fun = lme4::glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Post-stratification data not found.", fixed = TRUE
  )
  skip_if_not_installed("rstanarm")
  expect_error(
    ex_map$fit(
      fun = rstanarm::stan_glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Post-stratification data not found.", fixed = TRUE
  )
  skip_if_not_installed("brms")
  expect_error(
    ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit")
    ),
    "Post-stratification data not found.", fixed = TRUE
  )
})

test_that("Error if data is given as input",{
  skip_if_not_installed("lme4")
  expect_error(
    ex_map$fit(
      fun = lme4::glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit"),
      data = feline_survey
    ),
    "The 'data' argument should not be specified.", fixed = TRUE
  )
  skip_if_not_installed("rstanarm")
  expect_error(
    ex_map$fit(
      fun = rstanarm::stan_glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit"),
      data = feline_survey
    ),
    "The 'data' argument should not be specified.", fixed = TRUE
  )
  skip_if_not_installed("brms")
  expect_error(
    ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link="logit"),
      data = feline_survey
    ),
    "The 'data' argument should not be specified.", fixed = TRUE
  )
})

ex_map$tabulate() # Use all variables in the map

test_that("Warning is given if fitting using packages that are not lme4, brms, rstanarm ",{
  expect_warning(
    ex_map$fit(
      fun = stats::glm,
      formula = y ~ age + gender,
      family = "binomial"
    ),
    "Only rstanarm, brms and lme4 are supported natively.", fixed = TRUE
  )
})

#TODO add a check for poststrat data in predictify
