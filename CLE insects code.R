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
richmodel <- lm(richness~Date + Site + sitetype, data=allbugs)  #AIC = 1929
summary(richmodel)
AIC(richmodel)
anova(richmodel) 

rich.emm<-emmeans(richmodel,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural and green roofs (p=0.0006)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.s<-emmeans(richmodel,pairwise~Site) 
rich.emm.s
#results: difference between natural and green roofs (p=0.0006)
rich.cld.s<-multcomp::cld(rich.emm.s, alpha = 0.05, Letters = LETTERS)
rich.cld.s 

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

plot(simulateResiduals(richmodel)) # another way to check for normailty and homogeneity of variance
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
abunmodel <- lm(abundance~Date + Site + sitetype, data=allbugs)  #AIC = 6319
#abunmodel <- glm(abundance~Date + Site + sitetype, data=allbugs, family = negative.binomial(2))  #AIC = 
summary(abunmodel)
AIC(abunmodel)
anova(abunmodel)

abun.emm<-emmeans(abunmodel,pairwise~sitetype) 
abun.emm
#results: difference between natural and green roofs (p=0.0492)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.s<-emmeans(abunmodel,pairwise~Site) 
abun.emm.s
#results: difference between natural and green roofs (p=0.0492)
abun.cld.s<-multcomp::cld(abun.emm.s, alpha = 0.05, Letters = LETTERS)
abun.cld.s 

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
divmodel <- lm(diversity~Date + Site + sitetype, data=allbugs)  #AIC = 526
summary(divmodel)
AIC(divmodel)
anova(divmodel)

div.emm<-emmeans(divmodel,pairwise~sitetype) 
div.emm
#results: no difference between natural and green roofs (p=0.6176)
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.s<-emmeans(divmodel,pairwise~Site) 
div.emm.s
#results: no difference between natural and green roofs (p=0.6176)
div.cld.s<-multcomp::cld(div.emm.s, alpha = 0.05, Letters = LETTERS)
div.cld.s 

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
evenmodel <- lm(evenness~Date + Site + sitetype, data=allbugs)  #AIC = -69
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
#results: difference between natural and green roofs (p < 0.0001)
even.cld.s<-multcomp::cld(even.emm.s, alpha = 0.05, Letters = LETTERS)
even.cld.s 

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
#outlier test: p = 1
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
richmodel.d <- lm(richness~Date + Site + design, data=greenroofbugs)  #AIC = 1035
summary(richmodel.d)
AIC(richmodel.d)
anova(richmodel.d) 

rich.emm.d<-emmeans(richmodel.d,pairwise~design) #comparing habitat vs mitigation
rich.emm.d
#results: no difference between natural and green roofs (p=0.4988)
rich.cld.d<-multcomp::cld(rich.emm.d, alpha = 0.05, Letters = LETTERS)
rich.cld.d 

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
abunmodel.d <- lm(abundance~Date + Site + design, data=greenroofbugs)  #AIC = 2786
summary(abunmodel.d)
AIC(abunmodel.d)
anova(abunmodel.d)

abun.emm.d<-emmeans(abunmodel.d,pairwise~design) 
abun.emm.d
#results: difference between natural and green roofs (p=0.0116)
abun.cld.d<-multcomp::cld(abun.emm.d, alpha = 0.05, Letters = LETTERS)
abun.cld.d 

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
divmodel.d <- lm(diversity~Date + Site + design, data=greenroofbugs)  #AIC = 303
summary(divmodel.d)
AIC(divmodel.d)
anova(divmodel.d)

div.emm.d<-emmeans(divmodel.d,pairwise~design) 
div.emm.d
#results: no difference between natural and green roofs (p=0.0557)
div.cld.d<-multcomp::cld(div.emm.d, alpha = 0.05, Letters = LETTERS)
div.cld.d 

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
evenmodel.d <- lm(evenness~Date + Site + design, data=greenroofbugs)  #AIC = -51
summary(evenmodel.d)
AIC(evenmodel.d)
anova(evenmodel.d) 

even.emm.d<-emmeans(evenmodel,pairwise~design) 
even.emm.d
#results: difference between natural and green roofs (p 0.0333)
even.cld.d<-multcomp::cld(even.emm.d, alpha = 0.05, Letters = LETTERS)
even.cld.d

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
#dispersion test: p = 0.704
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
                             common.legend = TRUE, legend = "bottom")
greenroofbugs_boxplot

pdf("greenroofbugs_boxplot.pdf", height=8, width=8) #height and width in inches
greenroofbugs_boxplot
dev.off()

###

#NMDS of insect community 
library (vegan)

#bring in data pooled by date-site
allbugs_pooled <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_2019%20and%202021_pooled.csv", na.strings = NULL)

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

#bootstrapping and testing for differences between the groups (regions)
fit<-adonis(com.matrix ~ sitetype, data = env.matrix, permutations = 999, method="bray")
fit
#P=0.001

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix)
anova(betadisper(distances_data, env.matrix$sitetype))
#P-value = 0.01 -- cannot assume homogeneity of multivariate dispersion

#pairwise adonis
library(pairwiseAdonis)
pairwise.adonis(com.matrix, env.matrix$sitetype) #sig diff between natural and GR (p=0.001)

###

#NMDS of green roof insect community 
#library (vegan)

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
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Habitat"),pch=19, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="Mitigation"), pch=17, col="#F0E442")
#add legend
legend(0.375,0.815, title=NULL, pch=c(19,17), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("Habitat", "Mitigation"))

#bootstrapping and testing for differences between the groups (regions)
fit<-adonis(com.matrix_gr ~ design, data = env.matrix_gr, permutations = 999, method="bray")
fit
#P=0.732

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_gr)
anova(betadisper(distances_data, env.matrix_gr$design))
#P-value = 0.2024 -- assumes homogeneity of multivariate dispersion

#pairwise adonis
library(pairwiseAdonis)
pairwise.adonis(com.matrix_gr, env.matrix_gr$design) #no diff between habitat and mitigation (p=0.0.743)

###

