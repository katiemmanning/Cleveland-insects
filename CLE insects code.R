#bring in 2019 datasets
bowls19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202019%20-%20Bowl_cleveland.csv",na.strings = NULL)
summary(bowls19)
str(bowls19) 
ramps19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202019%20-%20Ramp_cleveland.csv",na.strings = NULL)
summary(ramps19)
str(ramps19)
sticky19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202019%20-%20Sticky%20card_cleveland.csv",na.strings = NULL)
summary(sticky19)
str(sticky19)

#combine data tables 
##rbind.fill instead of rbind let's us have datasets with different columns (but then you'd have to get rid of NAs)
library (plyr)
bowlramp19 <- rbind.fill (bowls19, ramps19)
allbugs19 <-rbind.fill (bowlramp19, sticky19)

#add column for natural vs green roof
allbugs19$sitetype<-ifelse(allbugs19$Site=="SSH", "Natural",
                      ifelse(allbugs19$Site=="DGM", "Natural",
                          ifelse(allbugs19$Site=="BFB", "Natural", "Greenroof")))
str(allbugs19)

#To obtain richness counts
allbugs19.rowsums <- rowSums(allbugs19[,4:41]>0)
allbugs19$richness <- allbugs19.rowsums

#To obtain abundance counts
allbugs19.abun <- rowSums(allbugs19[,4:41])
allbugs19$abundance <- allbugs19.abun

#load vegan
library(vegan)

#calculate Shannon diversity
diversity <-diversity(allbugs19[,4:41])
allbugs19$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs19[,4:41]))
allbugs19$evenness <- evenness

summary(allbugs19)
str(allbugs19)

#add data subset for green roof sites
greenroofbugs19 <- allbugs19[ which(allbugs19$sitetype=="Greenroof"), ]
#add column for plant type (designed for habitat vs designed for mitigation - ie storm water)
greenroofbugs19$planttype<-ifelse(greenroofbugs19$Site=="EWB", "Mitigation",
                                ifelse(greenroofbugs19$Site=="WSC", "Mitigation", "Habitat"))
str(greenroofbugs19)

#add data subset for natural sites
naturalbugs19 <- allbugs19[ which(allbugs19$sitetype=="Natural"), ]
str(naturalbugs19)


######
#bring in 2021 data sets
bowls21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202021%20-%20Bowl.csv",na.strings = NULL)
summary(bowls21)
str(bowls21) 
jars21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202021%20-%20Jar.csv",na.strings = NULL)
summary(jars21)
str(jars21)
sticky21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Insect%20ID%202021%20-%20Sticky%20card.csv",na.strings = NULL)
summary(sticky21)
str(sticky21)

#combine data tables 
##rbind.fill instead of rbind let's us have datasets with different columns (but then you'd have to get rid of NAs)
#library (plyr)
bowljar21 <- rbind.fill (bowls21, jars21)
allbugs21 <-rbind.fill (bowljar21, sticky21)

#add column for natural vs green roof
allbugs21$sitetype<-ifelse(allbugs21$Site=="SSH", "Natural",
                           ifelse(allbugs21$Site=="DGM", "Natural",
                                  ifelse(allbugs21$Site=="BFB", "Natural", "Greenroof")))
str(allbugs21)

#To obtain richness counts
allbugs21.rowsums <- rowSums(allbugs21[,4:37]>0)
allbugs21$richness <- allbugs21.rowsums

#To obtain abundance counts
allbugs21.abun <- rowSums(allbugs21[,4:37])
allbugs21$abundance <- allbugs21.abun

#calculate Shannon diversity
diversity <-diversity(allbugs21[,4:37])
allbugs21$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs21[,4:37]))
allbugs21$evenness <- evenness

summary(allbugs21)
str(allbugs21)

#add data subset for green roof sites
greenroofbugs21 <- allbugs21[ which(allbugs21$sitetype=="Greenroof"), ]
#add column for plant type (designed for habitat vs designed for mitigation - ie storm water)
greenroofbugs21$planttype<-ifelse(greenroofbugs21$Site=="EWB", "Mitigation",
                                  ifelse(greenroofbugs21$Site=="WSC", "Mitigation", "Habitat"))
str(greenroofbugs21)

#add data subset for natural sites
naturalbugs21 <- allbugs21[ which(allbugs21$sitetype=="Natural"), ]
str(naturalbugs21)
