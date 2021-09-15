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
  weights = "wt",
  design = list(ids =~1)
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
  weights = "wt",
  design = list(ids =~1))

q_age <- QuestionMap$new(
  name = "age",
  col_names = c("age1","age2"),
  values_map = list(
    "18-25" = "18-35", "26-35" = "18-35","36-45" = "36-55",
    "46-55" = "36-55", "56-65" = "56-65", "66-75" = "66+", "76-90" = "66+"
  )
)
q_pet <- QuestionMap$new(
  name = "pet",
  col_names = c("pet_own","pet_pref"),
  values_map = list("cat" = "cat", "kitten" = "cat","dog" = "dog","puppy" = "dog")
)
q_gender <- QuestionMap$new(
  name = "gender",
  col_names = c("gender","gender"),
  values_map = data.frame("male" = "m","female" = "f", "nonbinary" = "nb")
)
ex_map <- SurveyMap$new(samp_obj, popn_obj, q_age, q_pet, q_gender)
ex_map$mapping()
ex_map$tabulate()

suppressWarnings(
  fit_glm <- ex_map$fit(
    fun = stats::glm,
    formula = y ~ age + gender,
    family = "binomial"
  )
)

if (requireNamespace("rstanarm", quietly = TRUE)) {
  suppressWarnings(
    fit_stan_glmer <- ex_map$fit(
      fun = rstanarm::stan_glmer,
      formula = y ~ (1|age) + (1|gender),
      family = "binomial",
      iter = 10,
      chains = 1,
      refresh = 0,
      seed = 123
    )
  )
  suppressWarnings(
    fit_stan_glm <- ex_map$fit(
      fun = rstanarm::stan_glm,
      formula = y ~ age + gender,
      family = "binomial",
      iter = 10,
      chains = 1,
      refresh = 0,
      seed = 123
    )
  )
}

if (requireNamespace("brms", quietly = TRUE)) {
  brms_backend <- ifelse(requireNamespace("cmdstanr", quietly = TRUE),
                         "cmdstanr", "rstan")
  suppressWarnings(
    fit_brms <- ex_map$fit(
      fun = brms::brm,
      formula = y ~ (1|age) + (1|gender),
      family = "bernoulli",
      iter = 10,
      chains = 1,
      refresh = 0,
      seed = 123,
      backend = brms_backend
    )
  )
}

if (requireNamespace("lme4", quietly = TRUE)) {
  fit_glmer <- ex_map$fit(
    fun = lme4::glmer,
    formula = y ~ (1|age) + (1|gender),
    family = "binomial"
  )
}

test_that("fit objects have correct R6 class", {
  expect_r6_class(fit_stan_glmer, "SurveyFit")
  expect_r6_class(fit_stan_glm, "SurveyFit")
  expect_r6_class(fit_brms, "SurveyFit")
  expect_r6_class(fit_glmer, "SurveyFit")
  expect_r6_class(fit_glm, "SurveyFit")
})

test_that("map returns SurveyMap", {
  expect_r6_class(fit_stan_glmer$map(), "SurveyMap")
  expect_r6_class(fit_stan_glm$map(), "SurveyMap")
  expect_r6_class(fit_brms$map(), "SurveyMap")
  expect_r6_class(fit_glmer$map(), "SurveyMap")
  expect_r6_class(fit_glm$map(), "SurveyMap")
})

test_that("fit returns fitted model object", {
  expect_s3_class(fit_stan_glmer$fit(), "stanreg")
  expect_s3_class(fit_stan_glm$fit(), "stanreg")
  expect_s3_class(fit_brms$fit(), "brmsfit")
  expect_s4_class(fit_glmer$fit(), "glmerMod")
  expect_s3_class(fit_glm$fit(), "glm")
})

test_that("population_predict returns correct objects",{
  # expect 5 draws (because iter = 10 above) for columns
  # expect same number of rows as poststrat data
  expected_dims <- c(nrow(ex_map$poststrat_data()), 5)

  skip_if_not_installed("rstanarm")
  expect_equal(dim(fit_stan_glmer$population_predict()), expected_dims)
  expect_equal(dim(fit_stan_glm$population_predict()), expected_dims)

  skip_if_not_installed("brms")
  expect_equal(dim(fit_brms$population_predict()), expected_dims)

  skip_if_not_installed("lme4")
  expect_equal(dim(fit_glmer$population_predict(nsamples = 5)), expected_dims)
})

test_that("population_predict throws correct errors", {
  # custom fun required but not specified
  expect_error(
    fit_glm$population_predict(),
    "Custom population_predict method required"
  )

  # newdata argument specified
  expect_error(
    fit_glm$population_predict(newdata = data.frame()),
    "The 'newdata' argument should not be specified"
  )
})


test_that("aggregate (to population) returns correct objects", {
  expected_dims <- c(5, 1) # 5 = 10 iter / 2

  skip_if_not_installed("rstanarm")
  x <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict())
  expect_s3_class(x, "data.frame")
  expect_named(x, "value")
  expect_equal(dim(x), expected_dims)

  expect_s3_class(x, "data.frame")
  expect_named(x, "value")
  expect_equal(dim(x), expected_dims)

  skip_if_not_installed("brms")
  x <- fit_brms$aggregate(fit_brms$population_predict())
  expect_s3_class(x, "data.frame")
  expect_named(x, "value")
  expect_equal(dim(x), expected_dims)

  skip_if_not_installed("lme4")
  x <- fit_glmer$aggregate(fit_glmer$population_predict(nsamples = 5))
  expect_s3_class(x, "data.frame")
  expect_named(x, "value")
  expect_equal(dim(x), expected_dims)
})

test_that("aggregate (by variable level) returns correct objects", {
  expected_dims <- c(5 * nlevels(popn_obj$survey_data()$age), 2) # 5 = 10 iter / 2
  expected_names <- c("age", "value")

  skip_if_not_installed("rstanarm")
  x <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict(), by = "age")
  expect_s3_class(x, "data.frame")
  expect_named(x, expected_names)
  expect_equal(dim(x), expected_dims)

  x <- fit_stan_glm$aggregate(fit_stan_glm$population_predict(), by = "age")
  expect_s3_class(x, "data.frame")
  expect_named(x, expected_names)
  expect_equal(dim(x), expected_dims)

  skip_if_not_installed("brms")
  x <- fit_brms$aggregate(fit_brms$population_predict(), by = "age")
  expect_s3_class(x, "data.frame")
  expect_named(x, expected_names)
  expect_equal(dim(x), expected_dims)

  skip_if_not_installed("lme4")
  x <- fit_glmer$aggregate(fit_glmer$population_predict(nsamples = 5), by = "age")
  expect_s3_class(x, "data.frame")
  expect_named(x, expected_names)
  expect_equal(dim(x), expected_dims)
})

test_that("aggregate throws correct errors", {
  expect_error(
    fit_glmer$aggregate(fit_glmer$population_predict(nsamples = 5), by = c("age", "gender")),
    "Currently only one variable can be named in 'by'"
  )
})

test_that("plot returns ggplot object", {
  popn <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict())
  expect_s3_class(fit_stan_glmer$plot(popn, additional_stats = "mrp"), "ggplot")
  expect_s3_class(fit_stan_glmer$plot(popn, additional_stats = "none"), "ggplot")

  by_age <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict(), by = "age")
  expect_s3_class(fit_stan_glmer$plot(by_age, additional_stats = c("wtd","raw")), "ggplot")
  expect_s3_class(fit_stan_glmer$plot(by_age, additional_stats = "none"), "ggplot")
})

test_that("Warnings provided if weights are all equal to 1",{
  expect_warning(samp_obj_wt1 <- SurveyData$new(
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
    design = list(ids =~1)
  ),"Weights have not been provided, assume all data weighted with weight 1."
  )
  ex_map_wt1 <- SurveyMap$new(samp_obj_wt1, popn_obj, q_age, q_pet, q_gender)
  ex_map_wt1$mapping()
  ex_map_wt1$tabulate()
  suppressWarnings(
        fit_stan_glm_wt1 <- ex_map_wt1$fit(
           fun = rstanarm::stan_glm,
           formula = y ~ age + gender,
             family = "binomial",
             iter = 10,
             chains = 1,
             refresh = 0,
             seed = 123
           )
       )
  predict_ests_wt1 <- fit_stan_glm_wt1$population_predict()
  agg_ests_wt1 <- fit_stan_glm_wt1$aggregate(predict_ests_wt1, by = "age")

  expect_warning(fit_stan_glm_wt1$plot(agg_ests_wt1),
                 "weights are all equal to 1 or no weights provided. Raw estimate and wtd estimate will be equivalent.")
})

test_that("plot method returns warnings for incorrect additional stats input",{
  x <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict(), by = "age")
  expect_error(fit_stan_glmer$plot(x, additional_stats = "a"),
               "only supply arguments `wtd`, `raw`, `mrp` or `none` as additional statistics.")

  expect_error(fit_stan_glmer$plot(x, additional_stats = FALSE),
               "only supply arguments `wtd`, `raw`, `mrp` or `none` as additional statistics.")


  expect_error(fit_stan_glmer$plot(x, additional_stats = NA),
               "only supply arguments `wtd`, `raw`, `mrp` or `none` as additional statistics.")


  expect_error(fit_stan_glmer$plot(x, additional_stats = NULL),
               "when choosing no additional statistics, only supply `none`")

  expect_error(fit_stan_glmer$plot(x, additional_stats = c("mrp","none")),
               "when choosing no additional statistics, only supply `none`")
})

test_that("summary works with different fit objects",{

  x <- fit_brms$aggregate(fit_brms$population_predict(), by = "age")
  expect_s3_class(fit_brms$summary(x), "data.frame")

  x <- fit_glmer$aggregate(fit_glmer$population_predict(), by = "age")
  expect_s3_class(fit_brms$summary(x), "data.frame")

  x <- fit_stan_glm$aggregate(fit_stan_glm$population_predict(), by = "age")
  expect_s3_class(fit_stan_glm$summary(x), "data.frame")

  x <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict(), by = "age")
  expect_s3_class(fit_stan_glmer$summary(x), "data.frame")


})

test_that("plot appearance hasn't changed", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # need to use deterministic inputs to the plots to test appearance changes
  popn <- data.frame(value = c(0.60, 0.65, 0.70, 0.75, 0.80))
  by_age <- data.frame(age = factor(rep(1:4, 5), labels = levels(ex_map$mapped_sample_data()$age)),
                       value = c(0.467,0.541,0.553, 0.507,
                                 0.554,0.630,0.636,   0.589,
                                 0.648,0.717, 0.719,  0.678,
                                 0.732,0.792, 0.796, 0.761,
                                 0.804,0.847, 0.850, 0.823)
                       )

  # vdiffr::write_svg(fit_stan_glmer$plot(popn, additional_stats = "none"),
  #                   "tests/figs/SurveyFit/plot-population.svg", title = "plot-population")
  #
  # vdiffr::expect_doppelganger(
  #   "plot-population",
  #   fit_stan_glmer$plot(popn, additional_stats = "none"),
  #   path = "tests/figs/SurveyFit/plot-population.svg"
  # )
  #
  # vdiffr::expect_doppelganger(
  #   "plot-population-weights",
  #   fit_stan_glmer$plot(popn, additional_stats = c("mrp","raw","wtd")),
  #   path = "SurveyFit"
  # )
  #
  # vdiffr::expect_doppelganger(
  #   "plot-group",
  #   fit_stan_glmer$plot(by_age, additional_stats = "none"),
  #   path = "SurveyFit"
  # )
  #
  # vdiffr::expect_doppelganger(
  #   "plot-group-weights",
  #   fit_stan_glmer$plot(by_age, additional_stats = c("mrp","raw","wtd")),
  #   path = "SurveyFit"
  # )
})

test_that("populations are within acceptable tolerance of previous runs (+/- 2% points)",{

  skip_if_not_installed("rstanarm")
  x <- fit_stan_glmer$aggregate(fit_stan_glmer$population_predict())
  expect_equal(mean(x$value), expected = .72, tolerance = .05)

  skip_if_not_installed("lme4")
  #lme4 consistently lower
  x <- fit_glmer$aggregate(fit_glmer$population_predict(nsamples = 50))
  expect_equal(mean(x$value), expected = .68, tolerance = .05)

  skip_if_not_installed("brms")
  x <- fit_brms$aggregate(fit_brms$population_predict())
  expect_equal(mean(x$value), expected = .72, tolerance = .05)

  #This is VERY noisy
  x <- fit_stan_glm$aggregate(fit_stan_glm$population_predict())
  expect_equal(mean(x$value), expected = .72, tolerance = .15)
})

test_that("summary requires the correct input",{
  expect_error(fit_glm$summary(),
               "argument \"aggregated_estimates\" is missing, with no default")
  expect_error(fit_glm$summary(c(1,2)),
               'dimensions of aggregated estimates must be two')

})


test_that("print method calls fitted model's print method", {
  # if the formulas are printed then the print method is working,
  # we don't need to check for the entire print output
  expect_output(
    fit_stan_glm$print(),
    "y ~ age + gender",
    fixed = TRUE
  )
  expect_output(
    fit_stan_glmer$print(),
    "y ~ (1 | age) + (1 | gender)",
    fixed = TRUE
  )
  expect_output(
    fit_brms$print(),
    "y ~ (1 | age) + (1 | gender)",
    fixed = TRUE
  )
  expect_output(
    fit_glmer$print(),
    "y ~ (1 | age) + (1 | gender)",
    fixed = TRUE
  )
})

test_that("force factor works appropriately",{
  x <- c()
  expect_error(force_factor(x),"x must have length n")

  x <- data.frame(a = c(1,2),b=c(1,2))
  expect_error(force_factor(x),"x must be a vector of length n")

  x <- factor(c("a","b","c"))
  expect_error(force_factor(x),"x cannot have more than 2 levels")

  x <- factor(c("yes", "no", NA))
  expect_equal(force_factor(x), c(1,0, NA))

  x <- factor(c("yes", "no", "yes"))
  expect_equal(force_factor(x), c(1,0, 1))

  x <- c(1,2,3)
  expect_error(force_factor(x),"x must have only two unique numeric values")

  x <- c(1,2, NA)
  expect_error(force_factor(x),"x must only contain 1, 0 and missing values")

  x <- c(1,1, NA)
  expect_equal(force_factor(x), c(1,1, NA))
})
