library(readstata13)

cces_2018 = read.dta13("cces18_common_vv.dta", convert.factors = T, nonint.factors = T, generate.factors=T)
col_labels <- attr(cces_2018, "var.labels")
col_names <- attr(cces_2018, "names")

demo_cols <- c(which(col_names == "birthyr"),
              which(col_names == "gender" ),
              which(col_names == "educ"),
              which(col_names == "race"),
              which(col_names == "hispanic"),
              which(col_names == "marstat"),
              which(col_names == "inputstate"))
keep_rows <- complete.cases(cces_2018[, demo_cols]) # no missing demographics

outcome_cols <- c(which(startsWith(col_labels, "Abortion --")),
                 which(startsWith(col_labels, "Immigration --")),
                 which(startsWith(col_labels, "Taxes --")),
                 which(startsWith(col_labels, "Health Care --")),
                 which(startsWith(col_labels, "President 20")))
keep_cols <- sort(c(demo_cols, outcome_cols))

cces_2018_subset <- subset(cces_2018, keep_rows, keep_cols)
# `subset` drops attributes, add back the ones we need
attr(cces_2018_subset, "var.labels") <- attr(cces_2018, "var.labels")[keep_cols]
droplevels(cces_2018_subset)

cces_race2 <- cces_2018_subset$race
cces_race2[cces_2018_subset$hispan == "Yes"] <- "Hispanic"
# add to acs_2018_subset data.frame - respect column types
cces_2018_subset$race2 = cces_race2
attr(cces_2018_subset, "var.labels")[['race2']] <- "Race or Hispanic"


# bin CCES birthyear
cohort_breaks = c(1900, 1935, 1945, 1955, 1965, 1975, 1985, 2000)
cohort_labels=c("before-1936",
         "1936-1945",
         "1946-1955",
         "1956-1965",
         "1966-1975",
         "1976-1985",
         "after 1986")
cces_2018_subset$birthyr_cohorts = cut(cces_2018_subset$birthyr, cohort_breaks, cohort_labels)
attr(cces_2018_subset, "var.labels")[['birthyr_cohort']] <- "Birthyear cohort"


save(cces_2018_subset, file="cces_2018_subset.RData")
