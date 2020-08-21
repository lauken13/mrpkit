source("SurveyMap.R")

cat("test 1\n")
survey_values <- c('18-25','26-35','36-45','46-55','56-65','66-75','76-90')
popn_values <- c('18-35','18-35','36-55','36-55','56-65','66+','66+')
test_map <- SurveyMap$new("age_s", "age_p", survey_values, popn_values)
test_map
cat("add\n")
survey_values2 <- c('M','F')
popn_values2 <- c('Male','Female')
test_map$add("sex_s", "sex_p", survey_values2, popn_values2)
test_map


cat("test 2\n")
test2_map = SurveyMap$new(c("age_s", "sex_s"),
                          c("age_p", "sex_p"),
                          list(survey_values, survey_values2),
                          list(popn_values, popn_values2))


test2_map
cat("delete\n")
test2_map$delete("age_s")
test2_map


cat("test 3\n")
test3_map = SurveyMap$new(c("age_s", "sex_s"),
                          c("age_p", "sex_p"),
                          list(survey_values, survey_values2),
                          list(popn_values, popn_values2))


test3_map
cat("replace\n")
survey_values3 <- c('H','F')
popn_values3 <- c('Homme','Femme')

test3_map$replace("sex_s", "sex_s", "sex_p", survey_values3, popn_values3)
test3_map
