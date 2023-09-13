library(mrpkit)
library(tidyverse)
library(haven)
library(labelled)

survey_dat <- SurveyData$new(feline_survey_haven)
survey_dat$questions()
survey_dat$responses()

generate_dictionary(feline_survey_haven) %>% View()

feline_survey_haven2 <- feline_survey_haven %>%
  mutate(pet_own = remove_val_labels(pet_own))


generate_dictionary(feline_survey_haven2) %>% View()

survey_dat <- SurveyData$new(feline_survey_haven2)
survey_dat$questions()
survey_dat$responses()
