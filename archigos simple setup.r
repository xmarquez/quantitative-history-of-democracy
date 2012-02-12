# You will need to download the data here:
# http://www.rochester.edu/college/faculty/hgoemans/data.htm
# This is for setting up the archigos datset, case file

archigos.simple <- read.dta("Archigos_2.9-Public.dta")

archigos.simple$startdate <- as.Date(archigos.simple$startdate,format="%d/%m/%Y")
archigos.simple$enddate <- as.Date(archigos.simple$enddate,format="%d/%m/%Y")
archigos.simple$eindate <- as.Date(archigos.simple$eindate,format="%d/%m/%Y")
archigos.simple$eoutdate <- as.Date(archigos.simple$eoutdate,format="%d/%m/%Y")

archigos.simple$entry <- factor(archigos.simple$entry, labels=c("Unknown",
                                                                "Regular",
                                                                "Irregular",
                                                                "Foreign Imposition"))

archigos.simple$exit <- factor(archigos.simple$exit,labels=c("Still in power",
                                                             "Unknown",
                                                             "Regular means",
                                                             "Natural causes",
                                                             "Ill health",
                                                             "Suicide",
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
                "Power struggle within military short of coup",
                "Other irregular")

archigos.simple$exitcode <- factor(archigos.simple$exitcode,labels=labelsexit)

posttenurelabels  <- c("Lost office in 2004","Still in power",
                       "Natural death up to six months after losing office", 
                       "No information could be found","OK","Exile",
                       "Imprisonment (including house arrest)","Death")

# a hack due to a problem with the dataset
archigos.simple <- transform(archigos.simple, 
                             posttenurefate = ifelse(posttenurefate == 3.1, 3, posttenurefate))

archigos.simple$posttenurefate <- factor(archigos.simple$posttenurefate,
                                         labels=posttenurelabels)

archigos.simple$gender <- factor(archigos.simple$gender, labels=c("Male","Female"))

archigos.simple$yrdied <- ifelse(archigos.simple$yrdied == -777, NA,archigos.simple$yrdied)

archigos.simple$obsid <- as.factor(archigos.simple$obsid)
archigos.simple$leadid <- as.factor(archigos.simple$leadid)
archigos.simple$idacr <- as.factor(archigos.simple$idacr)
archigos.simple$leader <- as.factor(archigos.simple$leader)

archigos.simple <- transform(archigos.simple, tenure = enddate - startdate)
archigos.simple <- transform(archigos.simple, tenureyrs = as.numeric(tenure, unit="days")/365.25)

archigos.simple <- transform(archigos.simple, dead = ifelse(archigos.simple$exit=="Still in power",FALSE,TRUE))

rm(posttenurelabels)