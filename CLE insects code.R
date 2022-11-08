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

bowls19$Trap="bowl"
ramps19$Trap="ramp"
sticky19$Trap="sticky"

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
allbugs19.rowsums <- rowSums(allbugs19[,4:43]>0)
allbugs19$richness <- allbugs19.rowsums

#To obtain abundance counts
allbugs19.abun <- rowSums(allbugs19[,4:43])
allbugs19$abundance <- allbugs19.abun

#load vegan
library(vegan)

#calculate Shannon diversity
diversity <-diversity(allbugs19[,4:43])
allbugs19$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs19[,4:43]))
allbugs19$evenness <- evenness

summary(allbugs19)
str(allbugs19)

#add data subset for green roof sites
greenroofbugs19 <- allbugs19[ which(allbugs19$sitetype=="Greenroof"), ]
#add column for plant type (designed for habitat vs designed for mitigation - ie storm water)
greenroofbugs19$design<-ifelse(greenroofbugs19$Site=="EWB", "Mitigation",
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

bowls21$Trap="bowl"
jars21$Trap="jar"
sticky21$Trap="sticky"

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
allbugs21.rowsums <- rowSums(allbugs21[,4:43]>0)
allbugs21$richness <- allbugs21.rowsums

#To obtain abundance counts
allbugs21.abun <- rowSums(allbugs21[,4:43])
allbugs21$abundance <- allbugs21.abun

#calculate Shannon diversity
diversity <-diversity(allbugs21[,4:43])
allbugs21$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs21[,4:43]))
allbugs21$evenness <- evenness

summary(allbugs21)
str(allbugs21)

#add data subset for green roof sites
greenroofbugs21 <- allbugs21[ which(allbugs21$sitetype=="Greenroof"), ]
#add column for plant type (designed for habitat vs designed for mitigation - ie storm water)
greenroofbugs21$design<-ifelse(greenroofbugs21$Site=="EWB", "Mitigation",
                                  ifelse(greenroofbugs21$Site=="WSC", "Mitigation", "Habitat"))
str(greenroofbugs21)

#add data subset for natural sites
naturalbugs21 <- allbugs21[ which(allbugs21$sitetype=="Natural"), ]
str(naturalbugs21)

###

#put together all 2019 and 2021 data
allbugs <- rbind.fill (allbugs19, allbugs21)
str (allbugs)

#print data into csv file for pooling purposes later
#write.csv(allbugs, file="allbugs_2019 and 2021.csv", row.names=FALSE)

#models and checking assumptions
library (emmeans) #for pairwise comparisons
#library (lme4) #for linear mixed effects models
#library (lmerTest) #to obtain p values for linear mixed effects models
library (multcompView) #to view letters
library (nortest)
library (bbmle)
library (DHARMa)
library (ggplot2)
library (sjPlot)
library (jtools)
library (interactions)

##richness linear model
richmodel <- lm(richness~Date + Site + sitetype + Trap, data=allbugs)  #AIC = 1726
summary(richmodel)
AIC(richmodel)
anova(richmodel) 

rich.emm<-emmeans(richmodel,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural and green roofs (p < 0.0001)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.s<-emmeans(richmodel,pairwise~Site) 
rich.emm.s
#results: 
rich.cld.s<-multcomp::cld(rich.emm.s, alpha = 0.05, Letters = LETTERS)
rich.cld.s 

rich.emm.t<-emmeans(richmodel,pairwise~Trap) 
rich.emm.t
#results: significant difference between all trap types 
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(allbugs$richness, main = "richness") # way to visualize outliers

with(allbugs, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 1.091e-08

with(allbugs, bartlett.test(richness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.1285

plot(richmodel) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel))
qqline(resid(richmodel))

plot(simulateResiduals(richmodel)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.95682
#dispersion test: p = 0.616
#outlier test: p = 0.27415
#no significant problems detected 

densityPlot(rstudent(richmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel)
influenceIndexPlot(richmodel, vars = c("Cook"), id = list(n = 3))

#

##abundance linear model
abunmodel <- lm(abundance~Date + Site + sitetype + Trap, data=allbugs)  #AIC = 6307
#abunmodel <- glm(abundance~Date + Site + sitetype, data=allbugs, family = negative.binomial(2))  #AIC = 
summary(abunmodel)
AIC(abunmodel)
anova(abunmodel)

abun.emm<-emmeans(abunmodel,pairwise~sitetype) 
abun.emm
#results: difference between natural and green roofs (p=0.0289)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.s<-emmeans(abunmodel,pairwise~Site) 
abun.emm.s
#results:
abun.cld.s<-multcomp::cld(abun.emm.s, alpha = 0.05, Letters = LETTERS)
abun.cld.s 

abun.emm.t<-emmeans(abunmodel,pairwise~Trap) 
abun.emm.t
#results: same for all except difference between bowl-sticky and jar-sticky
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(allbugs$abundance, main = "abundance") # way to visualize outliers
#clustered towards 0 --- outlier of 4800 and 6800

with(allbugs, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(allbugs, bartlett.test(abundance ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = < 2.2e-16

plot(abunmodel) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel))
qqline(resid(abunmodel))

plot(simulateResiduals(abunmodel)) # another way to check for normality and homogeneity of variance
#KS test: p = 0  *sig deviation
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(abunmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel)
influenceIndexPlot(abunmodel, vars = c("Cook"), id = list(n = 3))

#

##diversity linear model
divmodel <- lm(diversity~Date + Site + sitetype + Trap, data=allbugs)  #AIC = 487
summary(divmodel)
AIC(divmodel)
anova(divmodel)

div.emm<-emmeans(divmodel,pairwise~sitetype) 
div.emm
#results: no difference between natural and green roofs (p=0.7749)
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.s<-emmeans(divmodel,pairwise~Site) 
div.emm.s
#results: 
div.cld.s<-multcomp::cld(div.emm.s, alpha = 0.05, Letters = LETTERS)
div.cld.s 

div.emm.t<-emmeans(divmodel,pairwise~Trap) 
div.emm.t
#results: difference = bowl-jar, bowl-ramp, jar-ramp, jar-sticky... similar = bowl-sticky & ramp-sticky
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(allbugs$diversity, main = "diversity") # way to visualize outliers

with(allbugs, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 4.764e-08

with(allbugs, bartlett.test(diversity ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.01665

plot(divmodel) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel))
qqline(resid(divmodel))

plot(simulateResiduals(divmodel)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0.05355
#dispersion test: p = 0.616
#outlier test: p = 0.7801
#no significant problems detected  

densityPlot(rstudent(divmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel)
influenceIndexPlot(divmodel, vars = c("Cook"), id = list(n = 3))

#

##evenness linear mixed effects model
evenmodel <- lm(evenness~Date + Site + sitetype + Trap, data=allbugs)  #AIC = -141
summary(evenmodel)
AIC(evenmodel)
anova(evenmodel) 

even.emm<-emmeans(evenmodel,pairwise~sitetype) 
even.emm
#results: difference between natural and green roofs (p < 0.0001)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

even.emm.s<-emmeans(evenmodel,pairwise~Site) 
even.emm.s
#results: 
even.cld.s<-multcomp::cld(even.emm.s, alpha = 0.05, Letters = LETTERS)
even.cld.s 

even.emm.t<-emmeans(evenmodel,pairwise~Trap) 
even.emm.t
#results: sticky sig diff than all, everything else no difference
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t 

#check assumptions
dotchart(allbugs$evenness, main = "evenness") # way to visualize outliers

with(allbugs, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 5.155e-11

with(allbugs, bartlett.test(evenness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.08879

plot(evenmodel) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel))
qqline(resid(evenmodel))

plot(simulateResiduals(evenmodel)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0.12149
#dispersion test: p = 0.632
#outlier test: p =  *SIG*
#no significant problems detected 

densityPlot(rstudent(evenmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel)
influenceIndexPlot(evenmodel, vars = c("Cook"), id = list(n = 3))

#######
#ggplot box plots
library (ggplot2)

#site richness by site type
richness.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot

#site abundance by site type
abundance.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=abun.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
abundance.plot

#site diversity by site type
diversity.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot

#site evenness by site type
evenness.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot

###
#mush together plots
library(ggpubr) 
allbugs_boxplot <- ggarrange(richness.plot, abundance.plot, diversity.plot, evenness.plot,
                             #labels = c("A", "B", "C", "D"),
                             ncol = 1, nrow = 4,
                             common.legend = TRUE, legend = "bottom")
allbugs_boxplot

pdf("allbugs_boxplot.pdf", height=8, width=8) #height and width in inches
allbugs_boxplot
dev.off()

###
#models for greenroofs

#put together all 2019 and 2021 data
greenroofbugs <- rbind.fill (greenroofbugs19, greenroofbugs21)
str (greenroofbugs)

##richness linear model
richmodel.d <- lm(richness~Date + Site + design + Trap, data=greenroofbugs)  #AIC = 927
summary(richmodel.d)
AIC(richmodel.d)
anova(richmodel.d) 

rich.emm.d<-emmeans(richmodel.d,pairwise~design) #comparing habitat vs mitigation
rich.emm.d
#results: no difference between hab and mit (p=0.1631)
rich.cld.d<-multcomp::cld(rich.emm.d, alpha = 0.05, Letters = LETTERS)
rich.cld.d 

rich.emm.t<-emmeans(richmodel.d,pairwise~Trap) #comparing habitat vs mitigation
rich.emm.t
#results: sig diff between all except bowl-ramp
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(greenroofbugs$richness, main = "richness") # way to visualize outliers

with(greenroofbugs, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 7.91e-07

with(greenroofbugs, bartlett.test(richness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.3196

plot(richmodel.d) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel.d))
qqline(resid(richmodel.d))

plot(simulateResiduals(richmodel.d)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0.99196
#dispersion test: p = 0.656
#outlier test: p = 1
#no significant problems detected 

densityPlot(rstudent(richmodel.d)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.d)
influenceIndexPlot(richmodel.d, vars = c("Cook"), id = list(n = 3))

#

##abundance linear model
abunmodel.d <- lm(abundance~Date + Site + design + Trap, data=greenroofbugs)  #AIC = 2777
summary(abunmodel.d)
AIC(abunmodel.d)
anova(abunmodel.d)

abun.emm.d<-emmeans(abunmodel.d,pairwise~design) 
abun.emm.d
#results: difference between habitat and mitigation (p=0.0056)
abun.cld.d<-multcomp::cld(abun.emm.d, alpha = 0.05, Letters = LETTERS)
abun.cld.d 

abun.emm.t<-emmeans(abunmodel.d,pairwise~Trap) 
abun.emm.t
#results: no sig diff btw any except bowl-sticky
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(greenroofbugs$abundance, main = "abundance") # way to visualize outliers
#clustered towards 0 --- outliers of ~1000

with(greenroofbugs, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(greenroofbugs, bartlett.test(abundance ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = < 2.2e-16

plot(abunmodel.d) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel.d))
qqline(resid(abunmodel.d))

plot(simulateResiduals(abunmodel.d)) # another way to check for normality and homogeneity of variance
#KS test: p = 0  *sig deviation
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(abunmodel.d)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel.d)
influenceIndexPlot(abunmodel.d, vars = c("Cook"), id = list(n = 3))

#

##diversity linear model
divmodel.d <- lm(diversity~Date + Site + design + Trap, data=greenroofbugs)  #AIC = 248
summary(divmodel.d)
AIC(divmodel.d)
anova(divmodel.d)

div.emm.d<-emmeans(divmodel.d,pairwise~design) 
div.emm.d
#results: no difference between hab and mit (p=0.0686)
div.cld.d<-multcomp::cld(div.emm.d, alpha = 0.05, Letters = LETTERS)
div.cld.d 

div.emm.t<-emmeans(divmodel.d,pairwise~Trap) 
div.emm.t
#results: sig diff between all except bowl-jar & ramp-sticky
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(greenroofbugs$diversity, main = "diversity") # way to visualize outliers

with(greenroofbugs, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 2.332e-08

with(greenroofbugs, bartlett.test(diversity ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.3899

plot(divmodel.d) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel.d))
qqline(resid(divmodel.d))

plot(simulateResiduals(divmodel.d)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0.14645
#dispersion test: p = 0.656
#outlier test: p = 1
#no significant problems detected  

densityPlot(rstudent(divmodel.d)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.d)
influenceIndexPlot(divmodel.d, vars = c("Cook"), id = list(n = 3))

#

##evenness linear mixed effects model
evenmodel.d <- lm(evenness~Date + Site + design + Trap , data=greenroofbugs)  #AIC = -54
summary(evenmodel.d)
AIC(evenmodel.d)
anova(evenmodel.d) 

even.emm.d<-emmeans(evenmodel.d,pairwise~design) 
even.emm.d
#results: difference between hab and mit (p 0.0273)
even.cld.d<-multcomp::cld(even.emm.d, alpha = 0.05, Letters = LETTERS)
even.cld.d

even.emm.t<-emmeans(evenmodel.d,pairwise~Trap) 
even.emm.t
#results: no sig diff between any except bowl-sticky
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t

#check assumptions
dotchart(greenroofbugs$evenness, main = "evenness") # way to visualize outliers

with(greenroofbugs, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = 1.713e-8

with(greenroofbugs, bartlett.test(evenness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.4547

plot(evenmodel.d) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel.d))
qqline(resid(evenmodel.d))

plot(simulateResiduals(evenmodel.d)) # another way to check for normailty and homogeneity of variance
#KS test: p = SIG
#dispersion test: p = 0.528
#outlier test: p = 0.07826
#no significant problems detected 

densityPlot(rstudent(evenmodel.d)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel.d)
influenceIndexPlot(evenmodel.d, vars = c("Cook"), id = list(n = 3))

#######
#ggplot box plots
#library (ggplot2)

#site richness by site type
richness.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("Mitigation","Habitat")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.d

#site abundance by site type
abundance.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("Mitigation","Habitat")), y = abundance, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=abun.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
abundance.plot.d

#site diversity by site type
diversity.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("Mitigation","Habitat")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.d

#site evenness by site type
evenness.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("Mitigation","Habitat")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot.d

###
#mush together plots
#library(ggpubr) 
greenroofbugs_boxplot <- ggarrange(richness.plot.d, abundance.plot.d, diversity.plot.d, evenness.plot.d,
                             #labels = c("A", "B", "C", "D"),
                             ncol = 1, nrow = 4,
                             common.legend = TRUE, legend = "none")
greenroofbugs_boxplot

pdf("greenroofbugs_boxplot.pdf", height=8, width=8) #height and width in inches
greenroofbugs_boxplot
dev.off()

###

#merge boxplots into one figure
#can't figure out the legend
multipanel_boxplot <- ggarrange(allbugs_boxplot, greenroofbugs_boxplot,
                                   labels = c("A", "B"),
                                   ncol = 2, nrow = 1,
                                   common.legend = TRUE, legend = "bottom")
multipanel_boxplot

pdf("multipanel_boxplot.pdf", height=8, width=8) #height and width in inches
multipanel_boxplot
dev.off()

#NMDS of insect community 
library (vegan)

#bring in data pooled by date-site
allbugs_pooled <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_2019%20and%202021_pooled.csv", na.strings = NULL)

#bring in beneficial insects taxa info 
taxa <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_taxa.csv", na.strings=NULL)

#Create matrix of environmental variables    
env.matrix<-allbugs_pooled[c(1:3)]

#create matrix of community variables
com.matrix<-allbugs_pooled[c(4:43)]

#change to presence/absence
com.matrix[com.matrix > 0] <- 1
str(com.matrix)
rowSums(com.matrix)

#ordination by NMDS
NMDS<-metaMDS(com.matrix, distance="bray", k=2, autotransform=TRUE, trymax=300)
NMDS
###stress = 0.20
stressplot(NMDS)

#what taxa to display using "taxa"
pollinator<-as.vector(t(taxa[1,]))
pollinator<-pollinator[-1]
natural_enemies<-as.vector(t(taxa[2,]))
natural_enemies<-natural_enemies[-1]
include<-as.vector(t(taxa[3,]))
include<-include[-1]

#plot NMDS 
#might need to change colors
#8 x 13
plot(NMDS, disp='sites', type="n")
#title(main="Arthropod community composition by site type", cex.main=1.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
#add data points
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Natural"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Greenroof"), pch=17, col="#009E73")
#add legend
legend(0.92,0.68, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Natural", "Greenroof"))
#add insect taxa as text
ordilabel(NMDS, display="species", select =which (include==TRUE & pollinator == TRUE), cex=0.6, col="black", fill="white")
ordilabel(NMDS, display="species", select =which (include==TRUE & natural_enemies == TRUE), cex=0.6, col="white", fill="black")

#bootstrapping and testing for differences between the groups (regions)
fit<-adonis(com.matrix ~ sitetype, data = env.matrix, permutations = 999, method="bray")
fit
#P=0.001

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$sitetype))
#P-value = 0.01 -- cannot assume homogeneity of multivariate dispersion

##

#NMDS of natural versus mitigation green roof
mit <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_pooled_mitigation%20and%20natural.csv", na.strings=NULL)

#Create matrix of environmental variables    
env.matrix_mit<-mit[c(1:3)]

#create matrix of community variables
com.matrix_mit<-mit[c(4:43)]

#change to presence/absence
com.matrix_mit[com.matrix_mit > 0] <- 1
str(com.matrix_mit)
rowSums(com.matrix_mit)

#ordination by NMDS
NMDS_mit<-metaMDS(com.matrix_mit, distance="bray", k=2, autotransform=TRUE, trymax=300)
NMDS_mit
###stress = 0.18
stressplot(NMDS_mit)

#plot
plot(NMDS_mit, disp='sites', type="n")
#add ellipsoids with ordiellipse
ordiellipse(NMDS_mit, env.matrix_mit$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Mitigation")
ordiellipse(NMDS_mit, env.matrix_mit$type, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
#add data points
points(NMDS_mit, display="sites", select=which(env.matrix_mit$type=="Natural"),pch=19, col="#E69F00")
points(NMDS_mit, display="sites", select=which(env.matrix_mit$type=="Mitigation"), pch=17, col="#009E73")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Natural", "Mitigation"))

#bootstrapping and testing for differences between the groups (mitigation and natural)
fit<-adonis(com.matrix_mit ~ type, data = env.matrix_mit, permutations = 999, method="bray")
fit
#P=0.08

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_mit)
anova(betadisper(distances_data, env.matrix_mit$type))
#P-value = 0.006 -- cannot assume homogeneity of multivariate dispersion

#

#NMDS of natural versus habitat green roof
hab <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_pooled_habitat%20and%20natural.csv", na.strings=NULL)

#Create matrix of environmental variables    
env.matrix_hab<-hab[c(1:3)]

#create matrix of community variables
com.matrix_hab<-hab[c(4:43)]

#change to presence/absence
com.matrix_hab[com.matrix_hab > 0] <- 1
str(com.matrix_hab)
rowSums(com.matrix_hab)

#ordination by NMDS
NMDS_hab<-metaMDS(com.matrix_hab, distance="bray", k=2, autotransform=TRUE, trymax=300)
NMDS_hab
###stress = 0.18
stressplot(NMDS_hab)

plot(NMDS_hab, disp='sites', type="n")
#add ellipsoids with ordiellipse
ordiellipse(NMDS_hab, env.matrix_hab$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "Habitat")
ordiellipse(NMDS_hab, env.matrix_hab$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
#add data points
points(NMDS_hab, display="sites", select=which(env.matrix_hab$type=="Natural"),pch=19, col="#009E73")
points(NMDS_hab, display="sites", select=which(env.matrix_hab$type=="Habitat"), pch=17, col="#CC79A7")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Natural", "Habitat"))

#bootstrapping and testing for differences between the groups (habitat and natural)
fit<-adonis(com.matrix_hab ~ type, data = env.matrix_hab, permutations = 999, method="bray")
fit
#P=0.001

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_hab)
anova(betadisper(distances_data, env.matrix_hab$type))
#P-value = 0.20 -- assumes homogeneity of multivariate dispersion

#merge habitat and mitigation NMDSs into one figure
pdf("design type NMDSs.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS_mit, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_mit, env.matrix_mit$type, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "Mitigation")
ordiellipse(NMDS_mit, env.matrix_mit$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS_mit, display="sites", select=which(env.matrix_mit$type=="Natural"),pch=19, col="#009E73")
points(NMDS_mit, display="sites", select=which(env.matrix_mit$type=="Mitigation"), pch=15, col="#F0E442")
legend(-0.365,1.38, title=NULL, pch=c(19,15), col=c("#009E73","#F0E442"), cex=1.5, legend=c("Natural", "Mitigation"))

plot(NMDS_hab, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_hab, env.matrix_hab$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "Habitat")
ordiellipse(NMDS_hab, env.matrix_hab$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS_hab, display="sites", select=which(env.matrix_hab$type=="Natural"),pch=19, col="#009E73")
points(NMDS_hab, display="sites", select=which(env.matrix_hab$type=="Habitat"), pch=18, col="#CC79A7")
legend(0.022,1.2, title=NULL, pch=c(19,18), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Natural", "Habitat"))
dev.off()
###

#NMDS of green roof insect community 
library (vegan)

#bring in data pooled by date-site
greenroofbugs_pooled <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/greenroofbugs_2019%20and%202021_pooled.csv", na.strings = NULL)

#Create matrix of environmental variables    
env.matrix_gr<-greenroofbugs_pooled[c(1:3)]

#create matrix of community variables
com.matrix_gr<-greenroofbugs_pooled[c(4:43)]

#change to presence/absence
com.matrix_gr[com.matrix_gr > 0] <- 1
str(com.matrix_gr)
rowSums(com.matrix_gr)

#ordination by NMDS
NMDS_gr<-metaMDS(com.matrix_gr, distance="bray", k=2, autotransform=TRUE, trymax=300)
NMDS_gr
###stress = 0.16
stressplot(NMDS_gr)

#plot NMDS
#might need to change colors
#8 x 13
plot(NMDS_gr, disp='sites', type="n")
#title(main="Arthropod community composition by site type", cex.main=1.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "Mitigation")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "Habitat")
#add data points
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Habitat"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Mitigation"), pch=15, col="#F0E442")
#add legend
legend(0.375,0.815, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("Habitat", "Mitigation"))
#ordilabel(NMDS, display="species", select =which (include==TRUE & pollinator == TRUE), cex=0.6, col="black", fill="white")
#ordilabel(NMDS, display="species", select =which (include==TRUE & natural_enemies == TRUE), cex=0.6, col="white", fill="black")

#bootstrapping and testing for differences between the groups (habitat v mitigation)
fit<-adonis(com.matrix_gr ~ design, data = env.matrix_gr, permutations = 999, method="bray")
fit
#P=0.732

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_gr)
anova(betadisper(distances_data, env.matrix_gr$design))
#P-value = 0.2024 -- assumes homogeneity of multivariate dispersion

#plot greenroof sites
plot(NMDS_gr, disp='sites', type="n")
#title(main="Arthropod community composition by site type", cex.main=1.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS_gr, env.matrix_gr$Site, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "EWB")
ordiellipse(NMDS_gr, env.matrix_gr$Site, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "WSC")
ordiellipse(NMDS_gr, env.matrix_gr$Site, draw="polygon", col="blue",kind="sd", conf=0.95, label=FALSE, show.groups = "HDB")
ordiellipse(NMDS_gr, env.matrix_gr$Site, draw="polygon", col="grey",kind="sd", conf=0.95, label=FALSE, show.groups = "SNC")
#add data points
points(NMDS_gr, display="sites", select=which(env.matrix_gr$Site=="EWB"),pch=18, col="#F0E442")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$Site=="WSC"), pch=15, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$Site=="HDB"),pch=19, col="blue")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$Site=="SNC"), pch=20, col="grey")
#add legend
legend(0.375,0.815, title=NULL, pch=c(18,15, 19, 20), col=c("#F0E442", "#CC79A7", "blue", "grey"), cex=1.5, legend=c("EWB", "WSC", "HDB", "SNC"))
#ordilabel(NMDS, display="species", select =which (include==TRUE & pollinator == TRUE), cex=0.6, col="black", fill="white")
#ordilabel(NMDS, display="species", select =which (include==TRUE & natural_enemies == TRUE), cex=0.6, col="white", fill="black")

#bootstrapping and testing for differences between the groups (habitat v mitigation)
fit<-adonis(com.matrix_gr ~ Site, data = env.matrix_gr, permutations = 999, method="bray")
fit
#P > 0.05  ~0.08

library (pairwiseAdonis)
pairwise.adonis(com.matrix_gr, env.matrix_gr$Site)

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_gr)
anova(betadisper(distances_data, env.matrix_gr$Site))
#P-value = 0.5663 -- assumes homogeneity of multivariate dispersion

###

#merge allbugs and GR NMDSs into one figure and print to PDF
pdf("multi-NMDS.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Natural"),pch=19, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Greenroof"), pch=17, col="#E69F00")
legend(-0.25,1.21, title=NULL, pch=c(19,17), col=c("#009E73","#E69F00"), cex=1.5, legend=c("Natural", "Greenroof"))

plot(NMDS_gr, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "Mitigation")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "Habitat")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Habitat"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Mitigation"), pch=15, col="#F0E442")
legend(0.45,1.295, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("Habitat", "Mitigation"))
dev.off()

###

#analyse yellow ramp traps (ramps19) vs jar ramp traps (jars21)

#trap names
ramps19$Trap="ramp"
jars21$Trap="jar"

#merge
library (plyr)
ramps <- rbind.fill (ramps19, jars21)

#calculate mean and SE richness, abundance, diversity and evenness of each trap type

insects.abun <- rowSums(ramps19[,4:43])
ramps19$abundance <- insects.abun
insects.rowsums <- rowSums(ramps19[,4:43]>0)
ramps19$richness <- insects.rowsums
#need vegan to run diversity and evenness
diversity <-diversity(ramps19[,4:43])
ramps19$diversity <-diversity
evenness <-diversity/log(specnumber(ramps19[,4:43]))
ramps19$evenness <- evenness

insects.abun <- rowSums(jars21[,4:43])
jars21$abundance <- insects.abun
insects.rowsums <- rowSums(jars21[,4:43]>0)
jars21$richness <- insects.rowsums
diversity <-diversity(jars21[,4:43])
jars21$diversity <-diversity
evenness <-diversity/log(specnumber(jars21[,4:43]))
jars21$evenness <- evenness

mean(ramps19$abundance) #72.35
sd(ramps19$abundance)/sqrt(10) #58.84

mean(ramps19$richness) #5.82
sd(ramps19$richness)/sqrt(10) #0.88

mean(ramps19$diversity) #1.16
sd(ramps19$diversity)/sqrt(10) #0.15

mean(ramps19$evenness) #0.71
sd(ramps19$evenness)/sqrt(10) #0.06

mean(jars21$abundance) #7.43
sd(jars21$abundance)/sqrt(10) #3.84

mean(jars21$richness) #2.46
sd(jars21$richness)/sqrt(10) #0.54

mean(jars21$diversity) #0.59
sd(jars21$diversity)/sqrt(10) #0.19

mean(jars21$evenness) #NA
sd(jars21$evenness)/sqrt(10) #NA

#NMDS of insect community between trap types
#library (vegan)

#Create matrix of environmental variables
env.matrix_ramps<-ramps[c(1:3,44)]
#create matrix of community variables
com.matrix_ramps<-ramps[c(4:43)]

##Started pooling -- will finish if NMDS is wanted
#bring in pooled data
ramps19_pooled <- read.csv("", na.strings = NULL)
ramps19_pooled <- read.csv("", na.strings = NULL)
#combine
#library (plyr)
ramps_pooled <- rbind.fill (ramps19_pooled, jars21_pooled)

#Create matrix of environmental variables for pooled data
env.matrix_ramps<-ramps_pooled[c(1:3,44)]
#create matrix of community variables
com.matrix_ramps<-ramps_pooled[c(4:43)]

#change to presence/absence
com.matrix[com.matrix > 0] <- 1
str(com.matrix)
rowSums(com.matrix)

#ordination by NMDS
NMDS_ramps<-metaMDS(com.matrix_ramps, distance="bray", k=2, autotransform=TRUE, trymax=300)
NMDS_ramps
stressplot(NMDS_ramps)
#stress=0.14

#species accumulation 
library (BiodiversityR)
library(ggplot2)

#individual curves for each trap type
ramp.com.matrix<-ramps19[c(4:43)]
ramp_curve<-accumresult(ramp.com.matrix, method = "exact", permutations = 1000)

jar.com.matrix<-jars21[c(4:43)]
jar_curve<-accumresult(jar.com.matrix, method = "exact", permutations = 1000)

#first-order jackknife estimates are based on the number of singletons
#second-order jackknife estimates are based on the number of singletons and doubletons

#calculates order richness for each sample
specnumber(com.matrix_ramps) #ranges from 1 to 14

#calculates order richness by treatment (trap)
specnumber(com.matrix_ramps, groups = ramps$Trap) #jar=19; ramp=29

#total richness and jackknife
rich <- diversityresult(com.matrix_ramps, y=NULL, index = "richness")
rich # 32
j1 <- diversityresult(com.matrix_ramps, y=NULL, index = "jack1")
j1 # 34.976
#91%
j2 <- diversityresult(com.matrix_ramps, y=NULL, index = "jack2")
j2 # 31.095548
#102% --> 100%

#ramp jackknife; richness = 29
j1.r <- diversityresult(ramp.com.matrix, y=NULL, index = "jack1")
j1.r # 31.95
#91%
j2.r <- diversityresult(ramp.com.matrix, y=NULL, index = "jack2")
j2.r # 30.098588
#96%

#jar jackknife; richness = 19
j1.j <- diversityresult(jar.com.matrix, y=NULL, index = "jack1")
j1.j # 23.923077
#79%
j2.j <- diversityresult(jar.com.matrix, y=NULL, index = "jack2")
j2.j # 23.998798
#79%

#BiodiversityR::accumcomp
Accum.1 <- accumcomp(com.matrix_ramps, y=env.matrix_ramps, factor='Trap', 
                           method='random', conditioned=FALSE, plotit=FALSE)
Accum.1

#BiodiversityR::accumcomp.long
accum.long1 <- accumcomp.long(Accum.1, ci=NA, label.freq=5)
head(accum.long1)

#plot
#empty canvas
BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

accum <- ggplot(data=accum.long1, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_color_manual(values=c("gray","black"))+
  scale_shape_manual(values=c(19,17,15,25))+
  geom_line(aes(colour=Grouping), size=0.1) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, 0.3))), 
              show.legend=FALSE, linetype = 0) + 
  geom_point(data=subset(accum.long1, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=3) +
  BioR.theme +
  labs(x = "Number of samples", y = "Richness", colour = "Trap", shape = "Trap")
accum

pdf("accum curves.pdf", height=6, width=8) #height and width in inches
accum
dev.off()

###

#beneficial insect analyses

#import data
P <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/pollinators_2019%20and%202021.csv", na.strings = NULL)
NE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/natural%20enemies_2019%20and%202021.csv", na.strings = NULL)

#To obtain richness counts
P.rowsums <- rowSums(P[,5:7]>0)
P$richness <- P.rowsums

NE.rowsums <- rowSums(NE[,5:14]>0)
NE$richness <- NE.rowsums

#To obtain abundance counts
P.abun <- rowSums(P[,5:7])
P$abundance <- P.abun

NE.abun <- rowSums(NE[,5:14])
NE$abundance <- NE.abun

#library(vegan)
#calculate Shannon diversity
P.diversity <-diversity(P[,5:7])
P$diversity <-P.diversity

NE.diversity <-diversity(NE[,5:14])
NE$diversity <-NE.diversity

#calculate Evenness
P.evenness <-P.diversity/log(specnumber(P[,5:7]))
P$evenness <- P.evenness

NE.evenness <-NE.diversity/log(specnumber(NE[,5:14]))
NE$evenness <- NE.evenness

summary(P)
str(P)
summary(NE)
str(NE)

##Pollinator richness linear model
richmodel.p <- lm(richness~Date + Site + sitetype + Trap, data=P)  #AIC = 764
summary(richmodel.p)
AIC(richmodel.p)
anova(richmodel.p) 

rich.emm<-emmeans(richmodel.p,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural (higher) and green roofs (p < 0.0001)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.s<-emmeans(richmodel,pairwise~Site) 
rich.emm.s
#results: 
rich.cld.s<-multcomp::cld(rich.emm.s, alpha = 0.05, Letters = LETTERS)
rich.cld.s 

rich.emm.t<-emmeans(richmodel,pairwise~Trap) 
rich.emm.t
#results: significant difference between all trap types, except ramp-sticky -- bowls had highest richness, then ramps
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(P$richness, main = "richness") # way to visualize outliers

with(P, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(P, bartlett.test(richness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.001298

plot(richmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel.p))
qqline(resid(richmodel.p))

plot(simulateResiduals(richmodel.p)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.003 SIG 
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(richmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.p)
influenceIndexPlot(richmodel.p, vars = c("Cook"), id = list(n = 3))

#

##Pollinator abundance linear model
abunmodel.p <- lm(abundance~Date + Site + sitetype + Trap, data=P)  #AIC = 2578
summary(abunmodel.p)
AIC(abunmodel.p)
anova(abunmodel.p)

abun.emm<-emmeans(abunmodel.p,pairwise~sitetype) 
abun.emm
#results: difference between natural (higher) and green roofs (p=0.0006)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.s<-emmeans(abunmodel.p,pairwise~Site) 
abun.emm.s
#results: no diff btw most
abun.cld.s<-multcomp::cld(abun.emm.s, alpha = 0.05, Letters = LETTERS)
abun.cld.s 

abun.emm.t<-emmeans(abunmodel.p,pairwise~Trap) 
abun.emm.t
#results: bowl sig diff from all trap types (bowls caught highest abun, then ramps) - no diff btw rest
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(P$abundance, main = "abundance") # way to visualize outliers
#clustered towards 0 --- outlier of 4800 and 6800

with(P, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(P, bartlett.test(abundance ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = < 2.2e-16

plot(abunmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel.p))
qqline(resid(abunmodel.p))

plot(simulateResiduals(abunmodel.p)) # another way to check for normality and homogeneity of variance
#KS test: p = 0  *sig deviation
#dispersion test: p = 
#outlier test: p = 0.007 *SIG
#no significant problems detected 

densityPlot(rstudent(abunmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel.p)
influenceIndexPlot(abunmodel.p, vars = c("Cook"), id = list(n = 3))

#

##Pollinator diversity linear model
divmodel.p <- lm(diversity~Date + Site + sitetype + Trap, data=P)  #AIC = -165
summary(divmodel.p)
AIC(divmodel.p)
anova(divmodel.p)

div.emm<-emmeans(divmodel.p,pairwise~sitetype) 
div.emm
#results: difference between natural and green roofs (p=0.0037) - higher in natural
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.s<-emmeans(divmodel.p,pairwise~Site) 
div.emm.s
#results: no diff btw most
div.cld.s<-multcomp::cld(div.emm.s, alpha = 0.05, Letters = LETTERS)
div.cld.s 

div.emm.t<-emmeans(divmodel.p,pairwise~Trap) 
div.emm.t
#results: bowl sig diff from all trap types (bowls caught highest div, then ramps) - no diff btw rest
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(P$diversity, main = "diversity") # way to visualize outliers

with(P, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(P, bartlett.test(diversity ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 3.63e-07

plot(divmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel.p))
qqline(resid(divmodel.p))

plot(simulateResiduals(divmodel.p)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0 *SIG
#dispersion test: p = 0.616
#outlier test: p = 5e-05 *SIG
#no significant problems detected  

densityPlot(rstudent(divmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.p)
influenceIndexPlot(divmodel.p, vars = c("Cook"), id = list(n = 3))

#

##Pollinator evenness linear model
evenmodel.p <- lm(evenness~Date + Site + sitetype + Trap, data=P)  #AIC = 68
summary(evenmodel.p)
AIC(evenmodel.p)
anova(evenmodel.p) 

even.emm<-emmeans(evenmodel.p,pairwise~sitetype) 
even.emm
#results: difference between natural (greater) and green roofs (p = 0.0068)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

even.emm.s<-emmeans(evenmodel.p,pairwise~Site) 
even.emm.s
#results: no diff btw most 
even.cld.s<-multcomp::cld(even.emm.s, alpha = 0.05, Letters = LETTERS)
even.cld.s 

even.emm.t<-emmeans(evenmodel.p,pairwise~Trap) 
even.emm.t
#results: bowl sig diff from all trap types (bowls most even, then ramps) - no diff btw rest
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t 

#check assumptions
dotchart(P$evenness, main = "evenness") # way to visualize outliers

with(P, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(P, bartlett.test(evenness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.0003803

plot(evenmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel.p))
qqline(resid(evenmodel.p))

plot(simulateResiduals(evenmodel.p)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0 *SIG
#dispersion test: p = 
#outlier test: p =  
#no significant problems detected 

densityPlot(rstudent(evenmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel.p)
influenceIndexPlot(evenmodel.p, vars = c("Cook"), id = list(n = 3))

#####

## START EDITING


#ggplot box plots
library (ggplot2)

#Pollinators

#site richness by site type
richness.plot.p<-ggplot(P, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.p

#site abundance by site type
abundance.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Abundance (log10)")+
  scale_y_continuous(trans="log10")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=abun.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
abundance.plot

#site diversity by site type
diversity.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot

#site evenness by site type
evenness.plot<-ggplot(allbugs, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot

###
#mush together plots
library(ggpubr) 
allbugs_boxplot <- ggarrange(richness.plot, abundance.plot, diversity.plot, evenness.plot,
                             #labels = c("A", "B", "C", "D"),
                             ncol = 1, nrow = 4,
                             common.legend = TRUE, legend = "bottom")
allbugs_boxplot

pdf("allbugs_boxplot.pdf", height=8, width=8) #height and width in inches
allbugs_boxplot
dev.off()

