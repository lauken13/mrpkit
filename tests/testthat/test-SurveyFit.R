
#Small data fits for testing purposes:

rstanarm_fit <- rstanarm::stan_glmer(y ~ (1|age1) + (1|gender), data = feline_survey[1:300,],
                         family = binomial(link = "logit"))
glmer_fit <- lme4::glmer(y ~ (1|age1) + (1|gender), data = feline_survey,
                family = binomial(link = "logit"))
glm_fit <- glm(y ~ age1 + gender, data = feline_survey,
                        family = binomial(link = "logit"))
lm_fit <- lm(y ~ age1 + gender, data = feline_survey)

example_newdata <- feline_survey[sample.int(nrow(feline_survey),150),]

test_that("sim_posterior_epred throws error if not given glmerMod object", {
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
