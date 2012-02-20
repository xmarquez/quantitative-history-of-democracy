# This is all for setting up the polity dataset so that it is easier 
# to use with various manipulations
# You will need to download it from the web

code.table <- read.csv("codes.csv")
polity <- read.csv("../Data/p4v2010.csv")


#create factors
polity$exrec2 <- as.factor(polity$exrec)
polity$polcomp2 <- as.factor(polity$polcomp)
polity$exconst2 <- as.factor(polity$exconst)

exrec2labels <-c("Transition","Anarchy","Foreign occupation","Hereditary monarchy",
                 "Hereditary monarchy plus limited elite selection",
                 "Limited elite selection","Self-selection",
                 "Executive-guided transition", "Hereditary monarchy plus election",
                 "Competitive authoritarian (unfair electoral)",
                 "Competitive Election")

polcomp2labels <-c("Transition","Anarchy","Foreign occupation","Suppressed",
                   "Restricted","Imposed transition","Uninstitutionalized",
                   "Transition from uninstitutionalized","Factional restricted",
                   "Factional","Electoral transition - persistent conflict",
                   "Electoral transition - limited conflict",
                   "Institutionalized electoral")

exconst2labels <-c("Transition","Anarchy","Foreign occupation","Unlimited",
                   "Intermediate between unlimited and moderate",
                   "Slight to moderate limitations", 
                   "Intermediate between moderate and substantial",
                   "Substantial",
                   "Intermediate between substantial and parity or subordination",
                   "Executive parity or subordination")

levels(polity$exrec2) <- exrec2labels
levels(polity$polcomp2) <- polcomp2labels
levels(polity$exconst2) <- exconst2labels

polity <- merge(polity,code.table,all.x=TRUE)

# Create a full regime description string
polity <- transform(polity, regdescription = paste(exrec2,polcomp2,exconst2,sep="."))

# Create a dichotomous democracy/non-democracy regime variable
polity <- transform(polity, demstatus = ifelse(exrec < 0, "Transitional",ifelse(exrec < 8, "Non-democratic authority pattern","Competitive electoral regime")))

rm(code.table,exrec2labels,exconst2labels,polcomp2labels)

# For regime transition matrixes
require(plyr)
polity <- ddply(polity,.(ccode), transform, 
                topolcomp = c(polcomp[2:length(polcomp)],NA))

polity <- ddply(polity,.(ccode), transform, 
                frompolcomp = c(NA,polcomp[1:length(polcomp)-1]))

polity <- ddply(polity,.(ccode), transform, 
                toexrec = c(exrec[2:length(exrec)],NA))

polity <- ddply(polity,.(ccode), transform, 
                fromexrec = c(NA,exrec[1:length(exrec)-1]))

polity <- ddply(polity,.(ccode), transform, 
                toexconst = c(exrec[2:length(exconst)],NA))

polity <- ddply(polity,.(ccode), transform, 
                fromexconst = c(NA,exrec[1:length(exconst)-1]))

# For exrec, we get rid of the "transition" indexes (-88 and -5) to do better
# transition claculations
# For checking head(unique(polity[,c("country","exrec","toexrec","fromexrec","exrecnotrans")]),20)

polity$exrecnotrans <- polity$exrec
for (i in 1:length(polity$exrecnotrans)) {
  if(!is.na(polity$exrecnotrans[i])) {
    if (polity$exrecnotrans[i] %in% c(-88,5)) {
      polity$exrecnotrans[i] =  polity$exrecnotrans[i-1]
    }
  }
}

polity <- ddply(polity,.(ccode), transform, 
                  toexrecnotrans = c(exrecnotrans[2:length(exrecnotrans)],NA))  

polity$exrecnotrans2 <- polity$exrec
for (i in 1:length(polity$exrecnotrans2)) {
  if(!is.na(polity$exrecnotrans2[i])) {
    if (polity$exrecnotrans2[i] %in% c(-88,5,-77)) {
      polity$exrecnotrans2[i] =  polity$exrecnotrans2[i-1]
    }
  }
}

polity <- ddply(polity,.(ccode), transform, 
                toexrecnotrans2 = c(exrecnotrans2[2:length(exrecnotrans2)],NA))  
