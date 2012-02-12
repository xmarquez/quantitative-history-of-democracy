# First we setup the polity dataset

source("polity setup.r")

# for stacked area plot (pretty)
require(plotrix)

# These are convenience functions for creating the plots

# This one draws vertical lines around the major events: WWI, WWII, 
# African decolonization in 1960, the end of the cold war in 1989
major.events <- function(start=1900) {
  abline(v=c(1914-start,1918-start,1939-start,1945-start,1960-start,1989-start),
         col="lightgray")  
}

# This one is a convenience wrapper for stackpoly
stackplot <- function (table, title=NULL,start=1800,end=2010,by=10,
                   xlab="Year",ylab="Number of countries",legend=FALSE,
                       labels=NULL,color=FALSE) {
  
  colors <- gray(0:ncol(table)/ncol(table))
  if(color) {
    colors <- rainbow(ncol(table))
  }

  stackpoly(table,main=title,
            stack=TRUE, col=colors,
            xaxlab=seq(start,end,by=by),xat=seq(0,end-start+1,by=by),
            staxx=TRUE,xlab=xlab,ylab=ylab)
  major.events(start)
  if (legend) {
    legend(legend=labels,fill=colors,x="topleft")
  }
  }


  

t1 <- with(polity, table(year,exrec2))
stackplot(t1,title="Executive recruitment patterns, 1800-2010")

#labels <- levels(polity$exrec2)
#legend(legend=labels,fill=gray(0:length(labels)/length(labels)),x="topleft")

# The same graph but as proportions of total number of regimes
start <- 1900
t1.5 <- ts(100*prop.table(t1,margin=1),start=1800)
stackplot(window(t1.5,start),title="Executive recruitment patterns",
          ylab="Proportion of independent states",start=start)


# The distribution of types of political competition

start <- 1800
t2 <- with(polity, table(year,polcomp2))
labels <- levels(polity$polcomp2)
stackplot(t2,"Types of political competition, 1800-2010")

#legend(legend=labels,fill=1:length(labels),x="topleft")

# Same graph, but in proportional terms
t2.5 <- ts(100*prop.table(t2,margin=1),start=1800)
start <- 1900
stackplot(window(t2.5,start),title="Types of political competition",
          ylab="Proportion of independent states",start=start)

# The distribution of executive constraints

start <- 1800
t3 <- with(polity, table(year,exconst2))
labels <- levels(polity$exconst2)
stackplot(t3,title="Types of executive constraint, 1800-2010")

# A proportional version of the same
start <- 1900
t3.5 <- ts(100*prop.table(t3,margin=1),start=1800)
stackplot(window(t3.5,start),title="Types of executive constraint, 1800-2010",
          start=start,ylab="Proportion of independent states")

source("archigos_tv setup.R")

#Leadership transitions per year
t4 <- with(archigos, table(year, exit_tv))
labels <- levels(archigos$exit_tv)
start <- 1840
stackplot(t4,title="Types of leadership transitions, 1840-2004",
          start=start,end=2004,ylab="Number of leaders",color=TRUE)

# A proportional version of the same
start <- 1900
t4.5 <- ts(100*prop.table(t4,margin=1),start=1840)
stackplot(window(t4.5,start),title="Types of leadership transitions, 1840-2004",
          start=start,end=2004,ylab="Proportion of leaders",color=TRUE)
#legend(legend=labels,fill=gray(0:length(labels)/length(labels),x="topleft"))


# Competitive regimes by region
t7 <- with(subset(polity, exrec == 8), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackplot(t7,title="Competitive regimes by region, 1800-2010",legend=TRUE,
          labels=labels,color=TRUE)

# A proportional version of the same, starting in 1960

t7.5<-ts(100*prop.table(t7,margin=1),start=1800)
start=1960
stackplot(window(t7.5,start=start),title="Competitive regimes by region, 1800-2010",
          legend=TRUE,labels=labels,color=TRUE, start=1960,
          ylab="Proportion of regimes")

# For comparison purposes here are the proportions of countrie in the dataset by year
t14 <- with(polity, 100*prop.table(table(year, un_continent_name),margin=1))
labels <- levels(polity$un_continent_name)
stackplot(t14,
          title="Distribution of countries by region in Polity IV dataset",
          color=TRUE,legend=TRUE,labels=labels,
          ylab="Proportion of all regimes in the dataset")

# What percentages of the regimes are democratic 
# within each region (as proportion of the toal number of regimes
# in the region)

require(lattice)

t17.5 <- with(polity, table(year, un_region_name))
t22 <- with(subset(polity, exrec== 8), table(year, un_region_name))
t22 <- ts(100*t22/t17.5,start=1800)
xyplot(window(t22,start=1950),ylim=c(0,100),xlab="Year",
       ylab="Proportion of competitive electoral regimes in region as % of independent states")

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