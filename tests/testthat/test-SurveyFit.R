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
ex_map <- SurveyMap$new(samp_obj = samp_obj, popn_obj = popn_obj, q1,q2,q3)

ex_map$mapping()

ex_map$tabulate()

test_that("sim_posterior_epred throws error if not given glmerMod object", {
  skip_if_not_installed("merTools")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  expect_error(
    suppressWarnings(sim_posterior_epred(rstanarm_fit,newdata = example_newdata)),
    "Object must have class 'glmerMod'."
  )
  expect_error(
    suppressWarnings(sim_posterior_epred(glm_fit,newdata = example_newdata)),
    "Object must have class 'glmerMod'."
  )
  expect_error(
    suppressWarnings(sim_posterior_epred(lm_fit,newdata = example_newdata)),
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


test_that("Model fits do not cause errors ",{
  skip_if_not_installed("rstanarm")
    expect_error(rstanarm_fit <- ex_map$fit(
      fun = rstanarm::stan_glmer,
      formula = y ~ (1|age) + (1|gender),
      refresh = 100,
      cores = 2,
      family = binomial(link = "logit")
    ), regexp = NA)

  skip_if_not_installed("lme4")
    expect_error(lme4_fit <- ex_map$fit(
      fun = lme4::glmer,
      formula = y ~ (1|age) + (1|gender),
      family = binomial(link = "logit")
    ), regexp = NA)

  skip_if_not_installed("brms")
    expect_error(brms_fit <- ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender),
      refresh = 100,
      cores = 2,
      family = binomial(),
      backend = "cmdstanr"
    ), regexp = NA)
})

test_that("Predictify runs without errors",{
  skip_if_not_installed("rstanarm")
  expect_error(poststrat_preds_rstanarm <-
                 rstanarm_fit$predictify(),
               regexp = NA)

  skip_if_not_installed("lme4")
  expect_error(poststrat_preds_lme4 <-
                 lme4_fit$predictify(),
               regexp = NA)

  skip_if_not_installed("brms")
  expect_error(poststrat_preds_brms <-
                 brms_fit$predictify(),
               regexp = NA)
})

test_that("Aggregate runs without errors",{
  skip_if_not_installed("rstanarm")
  expect_error(popn_ests_rstanarm <-
               rstanarm_fit$aggregate(poststrat_preds_rstanarm),
               regexp = NA)


  skip_if_not_installed("lme4")
  expect_error(popn_ests_lme4 <-
               lme4_fit$aggregate(poststrat_preds_lme4),
               regexp = NA)

  skip_if_not_installed("brms")
  expect_error(popn_ests_brms <-
               brms_fit$aggregate(poststrat_preds_brms),
               regexp = NA)

})

test_that("Populations are within acceptable tolerance of previous runs (+/- 2% points)",{
  skip_if_not_installed("rstanarm")
  expect_lt(mean(popn_ests_rstanarm$value), .72 + .02)
  expect_gt(mean(popn_ests_rstanarm$value), .72 - .02)

  skip_if_not_installed("lme4")
  #Benchmark to a run from previous, so different benchmark values
  expect_lt(mean(popn_ests_lme4$value), .68 + .02)
  expect_gt(mean(popn_ests_lme4$value), .68 - .02)

  skip_if_not_installed("brms")
  #Benchmark to a run from previous, so different benchmark values
  expect_lt(mean(popn_ests_brms$value), .85 + .02)
  expect_gt(mean(popn_ests_brms$value), .85 - .02)
})

