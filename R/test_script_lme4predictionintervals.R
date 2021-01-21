library(mrpkit)
library(brms)
library(merTools)
fm1 <- glmer(y ~ gender + (1|age1) + (1|pet_own), data=feline_survey, family = "binomial")
display(fm1)
pred_obs <- predictInterval(merMod = fm1, newdata = feline_survey,
                      level = 0.95, n.sims = 1000,
                      stat = "median", type="probability",
                      include.resid.var = TRUE,
                      returnSims = TRUE)

#Obtain 1000 predicted probabilities
predicted_results <- inv_logit_scaled(attributes(pred_obs)$sim.results)
