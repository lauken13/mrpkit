# End to end tests

###############################################################################
####                      Without weights                                 #####
###############################################################################

library(dplyr)

head(shape_survey)
box_prefs <- SurveyData$new(
  data = shape_survey,
  questions = list(
    age = "Please identify your age group",
    gender = "Please select your gender",
    vote_for = "Which party did you vote for in the 2018 election?",
    y = "If today is the election day, would you vote for the Box Party?"
  ),
  responses = list(
    age = levels(shape_survey$age),
    gender = levels(shape_survey$gender),
    vote_for = levels(shape_survey$vote_for),
    y = c("no","yes")
  )
)
box_prefs$print()
box_prefs$n_questions()

head(approx_voters_popn)
popn_obj <- SurveyData$new(
  data = approx_voters_popn,
  questions = list(
    age_group = "Which age group are you?",
    gender = "Gender?",
    vote_pref = "Which party do you prefer to vote for?"
  ),
  responses = list(
    gender = levels(approx_voters_popn$gender),
    age_group = levels(approx_voters_popn$age_group),
    vote_pref = levels(approx_voters_popn$vote_pref)
  ))
popn_obj$print()

q_age <- QuestionMap$new(
  name = "age",
  col_names = c("age","age_group"),
  values_map = list(
    "18-25" = "18-35", "26-35" = "18-35","36-45" = "36-55",
    "46-55" = "36-55", "56-65" = "56-65", "66-75" = "66+", "76-90" = "66+"
  )
)
print(q_age)

q_party_pref <- QuestionMap$new(
  name = "party_pref",
  col_names = c("vote_for","vote_pref"),
  values_map = list("Box Party" = "BP",  "BP" = "BP","Circle Party" = "CP", "CP" = "CP")
)
q_gender <- QuestionMap$new(
  name = "gender",
  col_names = c("gender", "gender"),
  values_map = list("male" = "m","female" = "f", "nonbinary" = "nb")
)

# Create SurveyMap object adding all questions at once
ex_map <- SurveyMap$new(
  sample = box_prefs,
  population = popn_obj,
  q_age,
  q_party_pref,
  q_gender
)
print(ex_map) # or ex_map$print()

# Or can add questions incrementally
ex_map <- SurveyMap$new(sample = box_prefs, population = popn_obj)
print(ex_map)

ex_map$add(q_age, q_party_pref)
print(ex_map)

ex_map$add(q_gender)
print(ex_map)

# Create the mapping between sample and population
ex_map$mapping()

# Create the poststratification data frame using all variables in the mapping
# (alternatively, can specify particular variables, e.g. tabulate("age"))
ex_map$tabulate()

# Use lme4 for speed (rstanarm/brms tested elsewhere)

fit_2 <- ex_map$fit(
  fun = lme4::glmer,
  formula = y ~ (1|age) + (1|gender),
  family = "binomial"
)

# predicted probabilities
# returns matrix with rows for poststrat cells, cols for posterior draws
poststrat_estimates <- fit_2$population_predict()

# estimates by age level
estimates_by_age <- fit_2$aggregate(poststrat_estimates, by = "age")
head(estimates_by_age)
estimates_by_age %>%
  group_by(age) %>%
  summarize(mean = mean(value), sd = sd(value))

# plot estimates by age
fit_2$plot(estimates_by_age, weights = FALSE)
fit_2$plot(estimates_by_age, weights = TRUE)

# population estimate
estimates_popn <- fit_2$aggregate(poststrat_estimates)
mean(estimates_popn$value)

# plot population estimate
fit_2$plot(estimates_popn, weights = FALSE)
fit_2$plot(estimates_popn, weights = TRUE)

