source("SurveyObj.R")

# get us some survey data
ex_survey = read.csv("ex_survey_cats.csv")

# create obj sans info
sobj = SurveyObj$new(ex_survey)
sobj


