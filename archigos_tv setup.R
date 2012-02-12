# You will need to download the data here:
# http://www.rochester.edu/college/faculty/hgoemans/data.htm
# This is for setting up the archigos datset, time-varying file

library(foreign)
archigos <- read.dta("Archigos_v.2.9_tv-Public.dta")

archigos$startdate <- as.Date(archigos$startdate,format="%d/%m/%Y")
archigos$enddate <- as.Date(archigos$enddate,format="%d/%m/%Y")
archigos$eindate <- as.Date(archigos$eindate,format="%d/%m/%Y")
archigos$eoutdate <- as.Date(archigos$eoutdate,format="%d/%m/%Y")

archigos$entry <- factor(archigos$entry, labels=c("Unknown",
                                                  "Regular",
                                                  "Irregular",
                                                  "Foreign Imposition"))

archigos$exit <- factor(archigos$exit,labels=c("Still in power",
                                               "Unknown","Regular means",
                                               "Natural causes","Ill health",
                                               "Suicide","Irregular means",
                                               "Deposed by another state"))

archigos$exit_tv <- factor(archigos$exit_tv,labels=c("Still in power",
                                                     "Unknown","Regular means",
                                                     "Natural causes",
                                                     "Ill health","Suicide",
                                                     "Irregular means",
                                                     "Deposed by another state"))

labelsexit <- c("Missing","Regular","Popular protest with foreign support",
                "Popular protest without foreign support",
                "Rebel forces with foreign support",
                "Rebel forces without foreign support",
                "Military actors with foreign support", 
                "Domestic military actors without foreign support",
                "Other domestic government actors with foreign support", 
                "Other domestic government actors without foreign support",
                "Foreign force","Assassination by unsupported individual",
                "Power struggle within military short of coup","Other irregular")

archigos$exitcode <- factor(archigos$exitcode,labels=labelsexit)

posttenurelabels  <- c("Lost office in 2004","Still in power",
                       "Natural death up to six months after losing office", 
                       "No information could be found","OK",
                       "Exile","Imprisonment (including house arrest)","Death")

# A hack due to a problem with the dataset
archigos <- transform(archigos, posttenurefate = ifelse(posttenurefate == 3.1, 3, posttenurefate))

archigos$posttenurefate <- factor(archigos$posttenurefate, labels=posttenurelabels)

archigos <- transform(archigos, posttenurefate_tv = ifelse(posttenurefate_tv == 3.1, 3, posttenurefate))

archigos$posttenurefate_tv <- factor(archigos$posttenurefate_tv,labels=posttenurelabels)

archigos$gender <- factor(archigos$gender, labels=c("Male","Female"))

archigos$yrdied <- ifelse(archigos$yrdied == -777, NA,archigos$yrdied)

archigos$obsid <- as.factor(archigos$obsid)
archigos$leadid <- as.factor(archigos$leadid)
archigos$idacr <- as.factor(archigos$idacr)
archigos$leader <- as.factor(archigos$leader)

archigos <- transform(archigos, tenure = enddate - startdate)
archigos <- transform(archigos, tenureyrs = as.numeric(tenure, unit="days")/365.25)

archigos <- transform(archigos, dead = ifelse(archigos$exit=="Still in power",FALSE,TRUE))

archigos <- transform(archigos, yearbegin = as.Date(paste(year,"1","1",sep="-")))
archigos <- transform(archigos, yearend = as.Date(paste(year,"12","31",sep="-")))

rm(posttenurelabels,labelsexit)
