# This is all for setting up the polity dataset so that it is easier 
# to use with various manipulations
# You will need to download it from the web

code.table <- read.csv("codes.csv")
polity <- read.csv("p4v2010.csv")

#create factors
polity$exrec2 <- as.factor(polity$exrec)
polity$polcomp2 <- as.factor(polity$polcomp)
polity$exconst2 <- as.factor(polity$exconst)

exrec2labels <-c("Transition","Interregnum","Interruption","Hereditary monarchy",
                 "Hereditary monarchy plus limited elite selection",
                 "Limited elite selection","Self-selection",
                 "Executive-guided transition", "Hereditary monarchy plus election",
                 "Competitive authoritarian (unfair electoral)",
                 "Competitive Election")

polcomp2labels <-c("Transition","Interregnum","Interruption","Suppressed",
                   "Restricted","Imposed transition","Uninstitutionalized",
                   "Transition from uninstitutionalized","Factional restricted",
                   "Factional","Electoral transition - persistent conflict",
                   "Electoral transition - limited conflict",
                   "Institutionalized electoral")

exconst2labels <-c("Transition","Interregnum","Interruption","Unlimited",
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