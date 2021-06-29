# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# download dataset from IPUMS
# xml and dat file should have same name, be in same directory
acs_2018_ddi <- read_ipums_ddi("usa_00007.xml")
acs_2018 <- read_ipums_micro(acs_2018_ddi)

# save object for later
save(acs_2018, acs_2018_ddi, file= "acs_2018_subset.RData")

acs_colnames <- names(acs_2018)
print(acs_colnames)
print(class(acs_colnames[1]))
for (i in 1:length(acs_colnames)) {
    print(acs_colnames[i])
    print(ipums_val_labels(acs_2018[[i]]))
}
