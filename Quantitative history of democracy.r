# First we setup the polity dataset

source("polity setup.r")

# for stacked area plot (pretty)
require(plotrix)

labels <- levels(polity$exrec2)
stackpoly(t1,stack=TRUE, col=1:length(labels),main="Executive recruitment patterns, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

t2 <- with(polity, table(year,polcomp2))
labels <- levels(polity$polcomp2)
stackpoly(t2,stack=TRUE, col=1:length(labels),main="Types of political competition, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

t3 <- with(polity, table(year,exconst2))
labels <- levels(polity$exconst2)
stackpoly(t3,stack=TRUE, col=1:length(labels),main="Types of executive constraint, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

#Leadership transitions per year
t4 <- with(archigos, table(year, exitcode))
labels <- levels(archigos$exitcode)
stackpoly(t4, stack=TRUE,col=1:length(labels),main="Types of leadership transitions, 1840-2004", xaxlab = seq(1840,2004,by=20), xat=seq(1840-1839,2004-1839,by=20),xlab="Year",ylab="Number of leaders")
legend(legend=labels,fill=1:length(labels),x="topleft")

t5 <- with(archigos, table(year, exit_tv))
labels <- levels(archigos$exit_tv)
stackpoly(t5, stack=TRUE,col=1:length(labels),main="Types of leadership transitions, 1840-2004", xaxlab = seq(1840,2004,by=10), xat=seq(1840-1839,2004-1839,by=10),staxx=TRUE,xlab="Year",ylab="Number of leaders")
legend(legend=labels,fill=1:length(labels),x="topleft")

polity <- merge(polity, codes.table, all.x=TRUE)

# Competitive regimes by region
t7 <- with(subset(polity, exrec == 8), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackpoly(t7,stack=TRUE, col=1:length(labels),main="Competitive regimes by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# A proportional version of the same

t7.5<-100*prop.table(t7,margin=1)
labels <- levels(polity$un_continent_name)
stackpoly(t7.5,stack=TRUE, col=1:length(labels),main="Competitive regimes by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Proportion of all competitive regimes in the world")
legend(legend=labels,fill=1:length(labels),x="topleft")

# For comparison purposes here are the proportions of countrie in the dataset by year
t14 <- with(polity, 100*prop.table(table(year, un_continent_name),margin=1))
labels <- levels(polity$un_continent_name)
stackpoly(t14,stack=TRUE, col=1:length(labels),main="Distribution of countries by region in Polity IV dataset",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Proportion of all regimes in the world")
legend(legend=labels,fill=1:length(labels),x="topleft")

# What percentages of the regimes are democratic 
# within each region (as proportion of the toal number of regimes
# in the region)

require(lattice)

t17.5 <- with(polity, table(year, un_region_name))
t22 <- with(subset(polity, exrec== 8), table(year, un_region_name))
t22 <- ts(100*t22/t17.5,start=1800)
xyplot(window(t22,start=1950),ylim=c(0,100),xlab="Year",ylab="Proportion of competitive electoral regimes in region as % of independent states")

t17.5 <- with(polity, table(year, un_continent_name))
t22 <- with(subset(polity, exrec== 8), table(year, un_continent_name))
t22 <- ts(100*t22/t17.5,start=1800)
xyplot(t22,ylim=c(0,100),xlab="Year",ylab="Proportion of competitive electoral regimes in region")

#Proportion of hereditary monarchies per region
t17.5 <- with(polity, table(year, un_region_name))
t22 <- with(subset(polity, exrec > 0 & exrec < 3), table(year, un_region_name))
t22 <- ts(100*t22/t17.5,start=1800)
xyplot(t22,ylim=c(0,100),xlab="Year",ylab="Proportion of hereditary monarchies in region as % of independent states")

# Proportion of heavily repressive regimes per region
t22 <- with(subset(polity, polcomp %in% c(0,1)), table(year, un_region_name))
t22 <- ts(100*t22/t17.5,start=1800)
xyplot(window(t22,start=1945),ylim=c(0,100),xlab="Year",ylab="Proportion of heavily repressive regimes in region as % of independent states")

# A more detailed regional breakdown

t8 <- with(subset(polity, exrec == 8), table(year, un_region_name))
labels <- levels(polity$un_region_name)
stackpoly(t8,stack=TRUE, col=1:length(labels),main="Competitive regimes by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# Non-competitive regimes by region
t9 <- with(subset(polity, exrec > 0 & exrec < 8), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackpoly(t9,stack=TRUE, col=1:length(labels),main="Non democratic regimes by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# Monarchies by region
t10 <- with(subset(polity, exrec > 0 & exrec < 3), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackpoly(t10,stack=TRUE, col=1:length(labels),main="Hereditary monarchies by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# Limited elite selection regimes by region
t11 <- with(subset(polity, exrec == 3), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackpoly(t11,stack=TRUE, col=1:length(labels),main="Limited elite selection regimes by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# Suppressed competition regimes
t12 <- with(subset(polity, polcomp > 0 & polcomp < 3), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackpoly(t12,stack=TRUE, col=1:length(labels),main="Regimes with limited competition by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Number of countries")
legend(legend=labels,fill=1:length(labels),x="topleft")

# A proportional version of the same
t13<-100*prop.table(t12,margin=1)
labels <- levels(polity$un_continent_name)
stackpoly(t13,stack=TRUE, col=1:length(labels),main="Regimes with limited competition by region, 1800-2010",xaxlab=seq(1800,2010,by=10),xat=seq(1,212,by=10),staxx=TRUE,xlab="Year",ylab="Proportion of regimes of limited competition")
legend(legend=labels,fill=1:length(labels),x="topleft")