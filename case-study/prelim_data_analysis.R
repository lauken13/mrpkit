setwd("~/github/cces/dataverse")

# data clean - bin race, age, inc, educ - ordered numeric categories

# read 2018 data
library(readstata13)
cces_2018 = read.dta13("cces18_common_vv.dta", convert.factors = T, nonint.factors = T, generate.factors=T)

# prelim data analysis
table(complete.cases(cces_2018))

# get demographics
profile =c('birthyr','gender', 'educ', 'race', 'hispanic', 'marstat', 'employ', 'faminc_new', 'inputstate')
profile_2018 = cces_2018[profile]

library(tidyverse)
categories_bins_plot <- function(df, column_name)
{
    df2 = as.data.frame(group_by_(df, column_name) %>% summarise(n = n()))
    ggplot(df2, aes(x=df2[,1], y=df2[,2])) +
        geom_bar(stat = "identity") +
        xlab(column_name) + ylab(element_blank()) +
        ggtitle(paste("CCES 2018", profile[i], "raw counts")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

for(i in 1:length(profile)) {
    name = paste("cces_2018_",profile[i],".png", sep="", collapse="")
    p = categories_bins_plot(profile_2018, profile[i])
    png(filename=name)
    show(p)
    dev.off()
}
