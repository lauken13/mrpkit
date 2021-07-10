if (!require("ipumsr")) stop("This case study requires ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
if (!require("labelled")) stop("This case study requires labelled package. It can be installed using the following command: install.packages('labelled')")
library(ipumsr)
library(labelled)
library(readstata13)


# download dataset from IPUMS
# xml ddi file and Stata dat file should have same name, be in same directory
acs_2018_ddi <- read_ipums_ddi("usa_00007.xml")
acs_2018 <- read_ipums_micro(acs_2018_ddi)

# save object for later
save(acs_2018, acs_2018_ddi, file= "acs_2018_subset_raw.RData")

# drop underage, non-citizens
citizen = as_factor(acs_2018$CITIZEN) != "Not a citizen"
over_18 = acs_2018$AGE > 17
acs_2018_subset = acs_2018[(over_18 & citizen) , ]  

# create column which blends "race" "hispan"
acs_race2 <- factor(as_factor(acs_2018_subset$RACE), levels=c(levels(as_factor(acs_2018_subset$RACE)), "Hispanic"))

# if "hispan" is "Yes", "race" label is "Hispanic".
is_hispan <- acs_2018_subset$HISPAN > 0 & acs_2018_subset$HISPAN < 5
acs_race2[is_hispan==TRUE] <- "Hispanic"
acs_2018_subset$race2 = to_labelled(acs_race2)

# create column which conflates spouse present, absent
acs_marst2 <- as_factor(acs_2018_subset$MARST)
levels(acs_marst2) = c(levels(acs_marst2), "Married")
acs_marst2[acs_marst2 =="Married, spouse present"] <- "Married"
acs_marst2[acs_marst2 =="Married, spouse absent"] <- "Married"

# add to acs_2018_subset data.frame - respect column types
acs_2018_subset$marst2 = to_labelled(droplevels(acs_marst2))

acs_birthyr = 2018 - acs_2018_subset$AGE
cohort_breaks = c(1900, 1935, 1945, 1955, 1965, 1975, 1985, 2000)
cohort_labels=c("before-1936",
         "1936-1945",
         "1946-1955",
         "1956-1965",
         "1966-1975",
         "1976-1985",
         "after 1986")
birthyr_cohorts = cut(acs_birthyr, cohort_breaks, cohort_labels)
acs_2018_subset$birthyr_cohorts = to_labelled(birthyr_cohorts)

save(acs_2018_subset, acs_2018_ddi, file="acs_2018_subset.RData")
