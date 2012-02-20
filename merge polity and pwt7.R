# Merging polity with Penn World Tables
source("polity setup.r")

pwt7 <- read.csv("../Data/pwt70_w_country_names.csv")

names(pwt7)[1] <- "country.pwt"

require(countrycode)

polity <- transform(polity, isocode = countrycode(ccode,"cown","iso3c"))

polity <- merge(polity,pwt7,all.x=TRUE)