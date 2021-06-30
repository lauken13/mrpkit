library(readstata13)
cces_2018 = read.dta13("cces18_common_vv.dta", convert.factors = T, nonint.factors = T, generate.factors=T)

attr_names <- names(attributes(cces_2018))
col_labels <- attr(cces_2018, "var.labels")
col_names <- attr(cces_2018, "names")

demo_cols <- c(1, which(col_names == "birthyr"),
              which(col_names == "gender" ),
              which(col_names == "educ"),
              which(col_names == "race"),
              which(col_names == "hispanic"),
              which(col_names == "marstat"),
#              which(col_names == "region"),
#              which(col_names == "employ"),
#              which(col_names == "hadjob"),
              which(col_names == "inputstate"))
col_labels[demo_cols]

outcome_cols <- c(which(startsWith(col_labels, "Abortion --")),
                 which(startsWith(col_labels, "Immigration --")),
                 which(startsWith(col_labels, "Taxes --")),
                 which(startsWith(col_labels, "Health Care --")),
                 which(startsWith(col_labels, "President 20")))
col_labels[outcome_cols]

keep_cols <- sort(c(demo_cols, outcome_cols))

cces_2018_subset <- subset(cces_2018[keep_cols])
attr(cces_2018_subset, "var.labels") <- col_labels[keep_cols]
attr(cces_2018_subset, "names") <- col_names[keep_cols]

table(cces_2018_subset$gender)
table(cces_2018_subset$educ)
table(cces_2018_subset$race)
table(cces_2018_subset$marstat)
length(table(cces_2018_subset$inputstate))
length(table(cces_2018_subset$birthyr))

(length(table(cces_2018_subset$gender))-2) *
(length(table(cces_2018_subset$educ))-2) *
(length(table(cces_2018_subset$race))-2) *
(length(table(cces_2018_subset$marstat))-2) *
length(table(cces_2018_subset$inputstate)) *
length(table(cces_2018_subset$birthyr))

save(cces_2018_subset, file="cces_2018_subset.RData")

## # viz demographics
## demographics_2018 = cces_2018[demo_cols]
## demo_names = names(demographics_2018)
## library(tidyverse)
## categories_bins_plot <- function(df, column_name)
## {
##     df2 = as.data.frame(group_by_(df, column_name) %>% summarise(n = n()))
##     ggplot(df2, aes(x=df2[,1], y=df2[,2])) +
##         geom_bar(stat = "identity") +
##         xlab(column_name) + ylab(element_blank()) +
##         ggtitle(paste("CCES 2018", column_name, "raw counts")) +
##         theme(axis.text.x = element_text(angle = 90, hjust = 1))
## }

## for(i in 1:length(demo_names)) {
##     name = paste("cces_2018_",demo_names[i],".png", sep="", collapse="")
##     p = categories_bins_plot(demographics_2018, demo_names[i])
##     png(filename=name)
##     show(p)
##     dev.off()
## }
