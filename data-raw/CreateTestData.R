library(DeclareDesign)
library(brms)
library(survey)

design <- declare_population(N = 50000,
                             age_group = sample.int(7, size=N, replace = TRUE, prob = c(1,1,2,2,3,2,1)),
                             gender = sample.int(3, size=N, replace = TRUE, prob = c(.48,.49,.03)),
                             pet_own = sample.int(4, size=N, replace = TRUE, prob = abs(rnorm(4,0,1))),
                             y = rbinom(N,1,inv_logit_scaled(c(1,-.5,.5)[gender] +
                                         c(-.4,.4,.5,1.0)[pet_own] +
                                         c(-.2,-.5,.5,.5,1.0,.4,.7)[age_group])))
### get population
popn <- draw_data(design +
                    declare_sampling(n=50000))

get_sample <- function(data, n) {
  p = inv_logit_scaled(.5 + c(-1,-2,-1,-.5,1,.5,.1)[data$age_group]+
                         c(2,-.5,0)[data$gender] +
                         c(1,-.5,-1,.5)[data$pet_own])
  data[sample.int(nrow(data),n,replace=TRUE,prob = p), ]
}

get_pop_sample <- function(data, n) {
  p = inv_logit_scaled(.5 + c(-.1,-.2,-.1,-.5,.1,.5,.01)[data$age_group]+
                         c(.2,-.5,0)[data$gender] +
                         c(.1,-.5,-.1,.5)[data$pet_own])
  data[sample.int(nrow(data),n,replace=TRUE,prob = p), ]
}

feline_survey <- get_sample(popn,500)

approx_popn <- get_pop_sample(popn, 5000)%>%
  select(-y)

age_margin <- xtabs(~age_group, data = popn)
gender_margin <- xtabs(~gender, data = popn)
pet_margin <- xtabs(~pet_own, data = popn)

approx_popn$wt = 1
svy_design_approxpopn <- svydesign(ids=~1,weights = ~wt, data= approx_popn)

rkd_obj_approx_popn <- rake(design = svy_design_approxpopn, sample.margins = list(
  ~ gender, ~ age_group, ~ pet_own), population.margins = list(gender_margin,
                                                                 age_margin, pet_margin))
wts_trim_approx_popn <- trimWeights(rkd_obj_approx_popn, upper = quantile(weights(rkd_obj_approx_popn),.975))
approx_popn$wt <- weights(wts_trim_approx_popn)


#### Feline survey ####

# Create weights #
feline_survey$wt = 1
svy_design_feline_survey <- svydesign(ids=~1,weights = ~wt, data= feline_survey)

rkd_obj_feline_survey <- rake(design = svy_design_feline_survey, sample.margins = list(
  ~ gender, ~ age_group, ~ pet_own), population.margins = list(gender_margin,age_margin, pet_margin))
wts_trim_feline_survey <- trimWeights(rkd_obj_feline_survey, upper = quantile(weights(rkd_obj_feline_survey),.975))
feline_survey$wt <- weights(wts_trim_feline_survey)

# Adjust measurement  #

feline_survey <- feline_survey %>%
  select(-S_inclusion_prob) %>%
  mutate_at(c("age_group","gender","pet_own"), as.factor) %>%
  mutate(age1 =fct_recode(age_group, `18-25` = "1", `26-35` = "2", `36-45` = "3",`46-55` = "4", `56-65` = "5", `66-75` = "6", `76-90` = "7"),
         gender = fct_recode(gender, "male" = "1", "female" = "2", "nonbinary" = "3"),
         pet_own = fct_recode(pet_own, "cat" = "1", "kitten" = "2", "dog" = "3", "puppy" = "4")) %>%
  select(c(age1, gender, pet_own, y, wt))

approx_popn <- approx_popn %>%
  select(-S_inclusion_prob) %>%
  mutate_at(c("age_group","gender","pet_own"), as.factor) %>%
  mutate(age2 =fct_recode(age_group, `18-35` = "1", `18-35` = "2", `36-55` = "3",`36-55` = "4", `56-65` = "5", `66+` = "6", `66+` = "7"),
         gender = fct_recode(gender, "m" = "1", "f" = "2", "nb" = "3"),
         pet_pref = fct_recode(pet_own, "cat" = "1", "cat" = "2", "dog" = "3", "dog" = "4")) %>%
  select(c(age2, gender, pet_pref, wt))

