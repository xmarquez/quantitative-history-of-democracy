# First we setup the polity dataset

source("polity setup.r")

# for stacked area plot (pretty)
require(plotrix)

# These are convenience functions for creating the plots

# This one draws shaded lines around the major events: the great depression,
# WWI, WWII, African decolonization in 1960, the end of the cold war in 1989
# the breakup of the Soviet Union in 1991
major.events <- function(start=1900, ylim=NULL) {
  color <- rgb(190, 190, 190, alpha=70, maxColorValue=255)
  rect(xleft=1939-start, xright=1945-start, ybottom=0, ytop=ylim, 
       border="transparent", col=color)
  rect(xleft=1914-start, xright=1918-start, ybottom=0, ytop=ylim, 
       border="transparent", col=color)
  rect(xleft=1989-start, xright=1991-start, ybottom=0, ytop=ylim, 
       border="transparent", col=color)
  
  abline(v=c(1914-start,1918-start,1929-start,1939-start,1945-start,1960-start,1989-start,
             1991-start),
         col="lightgray")  
}

# This one is a convenience wrapper for stackpoly in plotrix
stackplot <- function (table, title=NULL,start=1800,end=2010,by=10,
                   xlab="Year",ylab="Number of countries",legend=FALSE,
                       labels=NULL,color=FALSE,colors=NULL,labelcolors=NULL) {
  
  col <- gray(0:ncol(table)/ncol(table))
  if(color) {
    col <- colors
    }
  
  stackpoly(table,main=title,
            stack=TRUE, col=col,
            xaxlab=seq(start,end,by=by),xat=seq(0,end-start+1,by=by),
            staxx=TRUE,xlab=xlab,ylab=ylab)
  major.events(start,ylim=max(margin.table(table,margin=1)))
  if (legend) {
    legend(legend=labels,fill=labelcolors,x="topleft",cex=0.5)
  }
  }


# Basic plot of executive recruitment patterns
# figure 1

t1 <- with(polity, table(year,exrec2))

labels <- levels(polity$exrec2)[4:length(levels(polity$exrec2))]
interruption.colors <- gray(0:2/3)
regime.colors <- rainbow(length(labels))
colors <- c(interruption.colors,regime.colors)                      
stackplot(t1,title="Executive recruitment patterns, 1800-2010",color=TRUE,
          colors=colors,legend=TRUE,labels=labels,labelcolors=regime.colors)

# Same as this:
# stackpoly(t1,main="Executive recruitment patterns, 1800-2010",
#           stack=TRUE, col=colors,
#           xaxlab=seq(1800,2010,by=10),xat=seq(0,2010-1800+1,by=10),
#           staxx=TRUE,xlab="Year",ylab="Number of countries")
# major.events(1800)
# legend(legend=labels,fill=regime.colors,x="topleft",cex=0.5)

#labels <- levels(polity$exrec2)
#legend(legend=labels,fill=gray(0:length(labels)/length(labels)),x="topleft")

# The same graph but as proportions of total number of regimes
start <- 1900
t1.5 <- ts(100*prop.table(t1,margin=1),start=1800)
stackplot(window(t1.5,start),title="Executive recruitment patterns",
          ylab="Proportion of independent states",start=start,color=TRUE,
          colors=colors,legend=TRUE,labels=labels,labelcolors=regime.colors)

# Same graph, as lattice
require(lattice)
my.plot <- xyplot(window(t1.5,start),ylim=c(0,50))
update(my.plot, panel = function(...) {
  panel.abline(v=c(1914,1918,1929,1939,1945,1960,1989,
                   1991), col = "lightgray")
  panel.xyplot(...)
})


#Figure 3
# Competitive regimes by region
t7 <- with(subset(polity, exrec %in% c(7,8)), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackplot(t7,title="Competitive regimes by region, 1800-2010",legend=TRUE,
          labels=labels,color=TRUE, labelcolors=rainbow(length(labels)))


# What percentages of the regimes are democratic 
# within each region (as proportion of the total number of regimes
# in the region)

require(lattice)

#Figure 4
t17.5 <- with(polity, table(year, un_region_name))
t22 <- with(subset(polity, exrec %in% c(7,8)), table(year, un_region_name))
t22 <- ts(100*t22/t17.5,start=1800)
my.plot <- xyplot(window(t22,start=1950),ylim=c(0,100),xlab="Year",
       ylab="Proportion of electoral regimes in region as % of independent states")
update(my.plot, panel = function(...) {
  panel.abline(v=c(1914,1918,1939,1945,1960,1989,
                   1991), col = "lightgray")
  panel.xyplot(...)
})


#Figure 5
# Non-competitive regimes by region
t9 <- with(subset(polity, exrec %in% c(1:6)), table(year, un_continent_name))
labels <- levels(polity$un_continent_name)
stackplot(t9,title="Non-competitive regimes by region, 1800-2010",
          ylab="Number of countries",legend=TRUE,
          labels=labels,color=TRUE,labelcolors=rainbow(length(labels)))


#Now we merge with PWT
source("merge polity and pwt7.r")
postwarpolity <- subset(polity, year > 1949 & year < 2010)
require(plyr)

postwarpolity <- ddply(postwarpolity, 
                       .(year),transform , 
                       incomequantile = cut(rgdpl,quantile(rgdpl,na.rm=TRUE)))
postwarpolity <- transform(postwarpolity, incomequantile2 = incomequantile)
quantilenames <- rep(c("Poorest","2nd","3rd","Richest"),60)
levels(postwarpolity$incomequantile2) <- quantilenames

#these are various potential breakdowns by income

regimes.per.income <- with(postwarpolity, 
                           table(year, incomequantile2))

electorally.competitive.regimes.per.income <- with(subset(postwarpolity, exrec==8),
                                       table(year,
                                             incomequantile2))

competitive.regimes.per.income <- with(subset(postwarpolity, exrec %in% c(7,8)),
                                       table(year,
                                             incomequantile2))

repressive.regimes.per.income <- with(subset(postwarpolity, polcomp %in% c(1,2)),
                                      table(year, incomequantile2))

monarchies.per.income <- with(subset(postwarpolity, exrec %in% c(1,2)),
                              table(year,
                                    incomequantile2))

limited.elite.regimes.per.income <- with(subset(postwarpolity, exrec == 3),
                                         table(year,
                                               incomequantile2))


tcomp <-  ts(100*(competitive.regimes.per.income / regimes.per.income),
             start=1950)

tcomp2 <- ts(100*prop.table(competitive.regimes.per.income,margin=1),
             start=1950)

tecomp <-  ts(100*(electorally.competitive.regimes.per.income / regimes.per.income),
             start=1950)

tecomp2 <- ts(100*prop.table(electorally.competitive.regimes.per.income,margin=1),
             start=1950)

rcomp <- ts(100*(repressive.regimes.per.income / regimes.per.income),
            start=1950)

rcomp2 <- ts(100*prop.table(repressive.regimes.per.income,margin=1),
             start=1950)

mcomp <- ts(100*(monarchies.per.income / regimes.per.income),
            start=1950)

mcomp2 <- ts(100*prop.table(monarchies.per.income,margin=1),
             start=1950)

lelite <- ts(100*(limited.elite.regimes.per.income / regimes.per.income),
            start=1950)

#Fig 6
my.plot <- xyplot(tcomp,ylim=c(0,100), 
                  xlab="Year",
                  main="Proportion of competitive regimes in each income quantile")
update(my.plot, panel = function(...) {
  panel.abline(v=c(1914,1918,1929,1939,1945,1960,1989,
                   1991), col = "lightgray")
  panel.xyplot(...)
})

#Fig 7
my.plot <- xyplot(tecomp,ylim=c(0,100),
                  xlab="Year",
                  main="Proportion of fully competitive regimes in each income quantile")
update(my.plot, panel = function(...) {
  panel.abline(v=c(1914,1918,1929,1939,1945,1960,1989,
                   1991), col = "lightgray")
  panel.xyplot(...)
})


# Calculating transition matrixes
all.trans <- round(100*prop.table(with(polity,table(exrec,toexrec)),margin=1),2)
postwar <- round(100*prop.table(with(subset(polity,year > 1945),
                                     table(exrec,toexrec)),margin=1),2)
coldwar <- round(100*prop.table(with(subset(polity,year > 1945 & year < 1989),
                                     table(exrec,toexrec)),margin=1),2)
postcoldwar <- round(100*prop.table(with(subset(polity,year > 1989),
                                         table(exrec,toexrec)),margin=1),2)
prewar <- round(100*prop.table(with(subset(polity,year < 1939),
                                    table(exrec,toexrec)),margin=1),2)

#Tables 1 and 2 are made this way
postwar.trans.income <- with(postwarpolity,table(exrec, toexrec, incomequantile2))
postwar.trans.income.prop <- round((prop.table(all.trans.income,margin=c(1,3))),2)

# A regional transition matrix
all.trans.region <- with(polity,table(exrec, toexrec, un_continent_name))
all.trans.region.prop <- round(100*(prop.table(all.trans.region,margin=c(1,3))),2)

