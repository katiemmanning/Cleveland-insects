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
allbugs19.rowsums <- rowSums(allbugs19[,5:44]>0)
allbugs19$richness <- allbugs19.rowsums

#To obtain abundance counts
allbugs19.abun <- rowSums(allbugs19[,5:44])
allbugs19$abundance <- allbugs19.abun

#load vegan
library(vegan)

#calculate Shannon diversity
diversity <-diversity(allbugs19[,5:44])
allbugs19$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs19[,5:44]))
allbugs19$evenness <- evenness

summary(allbugs19)
str(allbugs19)

#add data subset for green roof sites
greenroofbugs19 <- allbugs19[ which(allbugs19$sitetype=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofbugs19$design<-ifelse(greenroofbugs19$Site=="EWB", "SE",
                                ifelse(greenroofbugs19$Site=="WSC", "SE", "BE"))
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
allbugs21.rowsums <- rowSums(allbugs21[,5:44]>0)
allbugs21$richness <- allbugs21.rowsums

#To obtain abundance counts
allbugs21.abun <- rowSums(allbugs21[,5:44])
allbugs21$abundance <- allbugs21.abun

#calculate Shannon diversity
diversity <-diversity(allbugs21[,5:44])
allbugs21$diversity <-diversity

#calculate Evenness
evenness <-diversity/log(specnumber(allbugs21[,5:44]))
allbugs21$evenness <- evenness

summary(allbugs21)
str(allbugs21)

#add data subset for green roof sites
greenroofbugs21 <- allbugs21[ which(allbugs21$sitetype=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofbugs21$design<-ifelse(greenroofbugs21$Site=="EWB", "SE",
                                  ifelse(greenroofbugs21$Site=="WSC", "SE", "BE"))
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
library (lme4) #for linear mixed effects models
library (lmerTest) #to obtain p values for linear mixed effects models
library (multcompView) #to view letters
library (nortest)
library (bbmle)
library (DHARMa)
library (ggplot2)
library (sjPlot)
library (jtools)
library (interactions)

##richness linear model
richmodel <- lmer(richness~Date + sitetype + Trap + (1|Site:Replicate), data=allbugs)  #AIC = 1774
summary(richmodel)
AIC(richmodel)
anova(richmodel) 

rich.emm<-emmeans(richmodel,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural (higher) and green roofs (p = 0.03)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

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
#KS test: p = 0.02 *sig
#dispersion test: p = 0.432
#outlier test: p = 0.7801
#no significant problems detected 

densityPlot(rstudent(richmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel)
influenceIndexPlot(richmodel, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##abundance linear model
abunmodel <- lmer(abundance~Date + sitetype + Trap + (1|Site:Replicate), data=allbugs)  #AIC = 6190
summary(abunmodel)
AIC(abunmodel)
anova(abunmodel)

abun.emm<-emmeans(abunmodel,pairwise~sitetype) 
abun.emm
#results: difference between natural (higher) and green roofs (p=0.0469)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.t<-emmeans(abunmodel,pairwise~Trap) 
abun.emm.t
#results: no difference between any except bowl-sticky (p=0.0036) and jar-sticky (p=0.0067)
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
divmodel <- lmer(diversity~Date + sitetype + Trap + (1|Site:Replicate), data=allbugs)  #AIC = 548
summary(divmodel)
AIC(divmodel)
anova(divmodel)

div.emm<-emmeans(divmodel,pairwise~sitetype) 
div.emm
#results: no difference between natural and green roofs (p=0.5287)
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

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
#KS test: p = 0.26541
#dispersion test: p = 0.72
#outlier test: p = 0.27415
#no significant problems detected  

densityPlot(rstudent(divmodel)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel)
influenceIndexPlot(divmodel, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##evenness linear model
evenmodel <- lmer(evenness~Date + sitetype + Trap + (1|Site:Replicate), data=allbugs)  #AIC = -76
summary(evenmodel)
AIC(evenmodel)
anova(evenmodel) 

even.emm<-emmeans(evenmodel,pairwise~sitetype) 
even.emm
#results: difference between natural and green roofs (p = 0.0003)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

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
#KS test: p =
#dispersion test: p = 
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
allbugs_boxplot <- ggarrange(richness.plot, diversity.plot,
                             #labels = c("A", "B"),
                             ncol = 1, nrow = 2,
                             common.legend = TRUE, legend = "bottom")
allbugs_boxplot

pdf("allbugs_boxplot.pdf", height=8, width=8) #height and width in inches
allbugs_boxplot
dev.off()

###
#linear mixed effects models for greenroofs

#put together all 2019 and 2021 data
greenroofbugs <- rbind.fill (greenroofbugs19, greenroofbugs21)
str (greenroofbugs)

##richness model
richmodel.d <- lmer(richness~Date + design + Trap + (1|Site:Replicate), data=greenroofbugs)  #AIC = 956
summary(richmodel.d)
AIC(richmodel.d)
anova(richmodel.d) 

rich.emm.d<-emmeans(richmodel.d,pairwise~design) #comparing SE vs BE
rich.emm.d
#results: no difference btw SE and BE (p=0.3971)
rich.cld.d<-multcomp::cld(rich.emm.d, alpha = 0.05, Letters = LETTERS)
rich.cld.d 

rich.emm.t<-emmeans(richmodel.d,pairwise~Trap) 
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

#NOT INCLUDING
##abundance linear model
abunmodel.d <- lmer(abundance~Date + design + Trap + (1|Site:Replicate), data=greenroofbugs)  #AIC = 2694
summary(abunmodel.d)
AIC(abunmodel.d)
anova(abunmodel.d)

abun.emm.d<-emmeans(abunmodel.d,pairwise~design) 
abun.emm.d
#results: difference btw design types (p=0.0273)
abun.cld.d<-multcomp::cld(abun.emm.d, alpha = 0.05, Letters = LETTERS)
abun.cld.d 

abun.emm.t<-emmeans(abunmodel.d,pairwise~Trap) 
abun.emm.t
#results: no sig diff btw any except bowl-sticky
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(greenroofbugs$abundance, main = "abundance") # way to visualize outliers

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
divmodel.d <- lmer(diversity~Date + design + Trap + (1|Site:Replicate), data=greenroofbugs)  #AIC = 300
summary(divmodel.d)
AIC(divmodel.d)
anova(divmodel.d)

div.emm.d<-emmeans(divmodel.d,pairwise~design) 
div.emm.d
#results: no difference (p=0.1764)
div.cld.d<-multcomp::cld(div.emm.d, alpha = 0.05, Letters = LETTERS)
div.cld.d 

div.emm.t<-emmeans(divmodel.d,pairwise~Trap) 
div.emm.t
#results: sig diff between all except bowl-ramp & ramp-sticky
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

#NOT INCLUDING
##evenness linear mixed effects model
evenmodel.d <- lmer(evenness~Date + design + Trap + (1|Site:Replicate), data=greenroofbugs)  #AIC = -0.34
summary(evenmodel.d)
AIC(evenmodel.d)
anova(evenmodel.d) 

even.emm.d<-emmeans(evenmodel.d,pairwise~design) 
even.emm.d
#results: no difference btw design types (p 0.0734)
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

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
#get names of colors used in last box plot to match colors to sites in this boxplot
display.brewer.pal(n = 8, name = 'Paired')
brewer.pal(n = 8, name = "Paired")
#EWB - light green = #B2DF8A
#WSC - light orange = #FDBF6F
#HDB - dark green = #33A02C
#SNC - pink = #FB9A99

#site richness by site type
richness.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("SE","BE")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.d

#site abundance by site type
abundance.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("SE","BE")), y = abundance, fill=Site))+
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
diversity.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("SE","BE")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.d

#site evenness by site type
evenness.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("SE","BE")), y = evenness, fill=Site))+
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
library(ggpubr) 
greenroofbugs_boxplot <- ggarrange(richness.plot.d, diversity.plot.d,
                             #labels = c("A", "B"),
                             ncol = 1, nrow = 2,
                             common.legend = TRUE, legend = "bottom")
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
NMDS<-metaMDS(com.matrix, distance="jaccard", k=2, autotransform=TRUE, trymax=300)
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

#NMDS of natural versus SE green roof
SE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_pooled_SE%20and%20natural.csv", na.strings=NULL)

#Create matrix of environmental variables    
env.matrix_SE<-SE[c(1:3)]

#create matrix of community variables
com.matrix_SE<-SE[c(4:43)]

#change to presence/absence
com.matrix_SE[com.matrix_SE > 0] <- 1
str(com.matrix_SE)
rowSums(com.matrix_SE)

#ordination by NMDS
NMDS_SE<-metaMDS(com.matrix_SE, distance="jaccard", k=2, autotransform=TRUE, trymax=300)
NMDS_SE
###stress = 0.18
stressplot(NMDS_SE)

#plot
plot(NMDS_SE, disp='sites', type="n")
#add ellipsoids with ordiellipse
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
#add data points
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="Natural"),pch=19, col="#E69F00")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="SE"), pch=17, col="#009E73")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Natural", "SE"))

#bootstrapping and testing for differences between the groups (SE and natural)
fit<-adonis(com.matrix_SE ~ type, data = env.matrix_SE, permutations = 999, method="bray")
fit
#P=0.08

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_SE)
anova(betadisper(distances_data, env.matrix_SE$type))
#P-value = 0.006 -- cannot assume homogeneity of multivariate dispersion

#

#NMDS of natural versus BE green roof
BE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/allbugs_pooled_BE%20and%20natural.csv", na.strings=NULL)

#Create matrix of environmental variables    
env.matrix_BE<-BE[c(1:3)]

#create matrix of community variables
com.matrix_BE<-BE[c(4:43)]

#change to presence/absence
com.matrix_BE[com.matrix_BE > 0] <- 1
str(com.matrix_BE)
rowSums(com.matrix_BE)

#ordination by NMDS
NMDS_BE<-metaMDS(com.matrix_BE, distance="jaccard", k=2, autotransform=TRUE, trymax=300)
NMDS_BE
###stress = 0.18
stressplot(NMDS_BE)

plot(NMDS_BE, disp='sites', type="n")
#add ellipsoids with ordiellipse
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
#add data points
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="Natural"),pch=19, col="#009E73")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="BE"), pch=17, col="#CC79A7")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Natural", "BE"))

#bootstrapping and testing for differences between the groups (BE and natural)
fit<-adonis(com.matrix_BE ~ type, data = env.matrix_BE, permutations = 999, method="bray")
fit
#P=0.001

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_BE)
anova(betadisper(distances_data, env.matrix_BE$type))
#P-value = 0.20 -- assumes homogeneity of multivariate dispersion

#merge habitat and mitigation NMDSs into one figure
pdf("design type NMDSs.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS_SE, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="Natural"),pch=19, col="#009E73")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="SE"), pch=15, col="#F0E442")
legend(-0.23,1.38, title=NULL, pch=c(19,15), col=c("#009E73","#F0E442"), cex=1.5, legend=c("Natural", "SE"))

plot(NMDS_BE, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="Natural"),pch=19, col="#009E73")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="BE"), pch=18, col="#CC79A7")
legend(0.022,1.2, title=NULL, pch=c(19,18), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Natural", "BE"))
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
NMDS_gr<-metaMDS(com.matrix_gr, distance="jaccard", k=2, autotransform=TRUE, trymax=300)
NMDS_gr
###stress = 0.16
stressplot(NMDS_gr)

#plot NMDS
#might need to change colors
#8 x 13
plot(NMDS_gr, disp='sites', type="n")
#title(main="Arthropod community composition by site type", cex.main=1.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
#add data points
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="BE"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="SE"), pch=15, col="#F0E442")
#add legend
legend(0.375,0.815, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("BE", "SE"))
#ordilabel(NMDS, display="species", select =which (include==TRUE & pollinator == TRUE), cex=0.6, col="black", fill="white")
#ordilabel(NMDS, display="species", select =which (include==TRUE & natural_enemies == TRUE), cex=0.6, col="white", fill="black")

#bootstrapping and testing for differences between the groups (BE v SE)
fit<-adonis(com.matrix_gr ~ design, data = env.matrix_gr, permutations = 999, method="bray")
fit
#P=0.703

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

#bootstrapping and testing for differences between the groups (BE v SE)
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
pdf("allbugs+grbugs.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$sitetype, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Natural")
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Natural"),pch=19, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$sitetype=="Greenroof"), pch=17, col="#E69F00")
legend(-0.253,1.21, title=NULL, pch=c(19,17), col=c("#009E73","#E69F00"), cex=1.5, legend=c("Natural", "Greenroof"))

plot(NMDS_gr, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="BE"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="SE"), pch=15, col="#F0E442")
legend(0.206,1.297, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("BE", "SE"))
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

insects.abun <- rowSums(ramps19[,5:44])
ramps19$abundance <- insects.abun
insects.rowsums <- rowSums(ramps19[,5:44]>0)
ramps19$richness <- insects.rowsums
#need vegan to run diversity and evenness
diversity <-diversity(ramps19[,5:44])
ramps19$diversity <-diversity
evenness <-diversity/log(specnumber(ramps19[,5:44]))
ramps19$evenness <- evenness

insects.abun <- rowSums(jars21[,5:44])
jars21$abundance <- insects.abun
insects.rowsums <- rowSums(jars21[,5:44]>0)
jars21$richness <- insects.rowsums
diversity <-diversity(jars21[,5:44])
jars21$diversity <-diversity
evenness <-diversity/log(specnumber(jars21[,5:44]))
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
env.matrix_ramps<-ramps[c(1:4,45)]
#create matrix of community variables
com.matrix_ramps<-ramps[c(5:44)]

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
ramp.com.matrix<-ramps19[c(5:44)]
ramp_curve<-accumresult(ramp.com.matrix, method = "exact", permutations = 1000)

jar.com.matrix<-jars21[c(5:44)]
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
P.rowsums <- rowSums(P[,6:8]>0)
P$richness <- P.rowsums

NE.rowsums <- rowSums(NE[,6:15]>0)
NE$richness <- NE.rowsums

#To obtain abundance counts
P.abun <- rowSums(P[,6:8])
P$abundance <- P.abun

NE.abun <- rowSums(NE[,6:15])
NE$abundance <- NE.abun

library(vegan)
#calculate Shannon diversity
P.diversity <-diversity(P[,6:8])
P$diversity <-P.diversity

NE.diversity <-diversity(NE[,6:15])
NE$diversity <-NE.diversity

#calculate Evenness
P.evenness <-P.diversity/log(specnumber(P[,6:8]))
P$evenness <- P.evenness

NE.evenness <-NE.diversity/log(specnumber(NE[,6:15]))
NE$evenness <- NE.evenness

summary(P)
str(P)
summary(NE)
str(NE)

library (emmeans) #for pairwise comparisons

##Pollinator richness linear mixed effect model
richmodel.p <- lmer(richness~Date + sitetype + Trap + (1|Site:Replicate), data=P)  #AIC = 813
summary(richmodel.p)
AIC(richmodel.p)
anova(richmodel.p) 

rich.emm<-emmeans(richmodel.p,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural (higher) and green roofs (p < 0.0001)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.t<-emmeans(richmodel.p,pairwise~Trap) 
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
#KS test: p = 0.00027 SIG 
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(richmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.p)
influenceIndexPlot(richmodel.p, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Pollinator abundance linear model
abunmodel.p <- lmer(abundance~Date + sitetype + Trap + (1|Site:Replicate), data=P)  #AIC = 2562
summary(abunmodel.p)
AIC(abunmodel.p)
anova(abunmodel.p)

abun.emm<-emmeans(abunmodel.p,pairwise~sitetype) 
abun.emm
#results: difference between natural (higher) and green roofs (p=0.0021)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

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
divmodel.p <- lmer(diversity~Date + sitetype + Trap + (1|Site:Replicate), data=P)  #AIC = -101
summary(divmodel.p)
AIC(divmodel.p)
anova(divmodel.p)

div.emm<-emmeans(divmodel.p,pairwise~sitetype) 
div.emm
#results: difference between natural and green roofs (p=0.0099) - higher in natural
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

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
#dispersion test: p = 0.664
#outlier test: p = 5e-05 *SIG
#no significant problems detected  

densityPlot(rstudent(divmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.p)
influenceIndexPlot(divmodel.p, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Pollinator evenness linear model
evenmodel.p <- lmer(evenness~Date + sitetype + Trap + (1|Site:Replicate), data=P)  #AIC = 125
summary(evenmodel.p)
AIC(evenmodel.p)
anova(evenmodel.p) 

even.emm<-emmeans(evenmodel.p,pairwise~sitetype) 
even.emm
#results: difference between natural (greater) and green roofs (p = 0.0154)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

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
abundance.plot.p<-ggplot(P, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
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
abundance.plot.p

#site diversity by site type
diversity.plot.p<-ggplot(P, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.p

#site evenness by site type
evenness.plot.p<-ggplot(P, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot.p

###
#mush together plots
library(ggpubr) 
pollinator_boxplot <- ggarrange(richness.plot.p, diversity.plot.p, 
                             ncol = 1, nrow = 2,
                             common.legend = TRUE, legend = "bottom")
pollinator_boxplot

pdf("pollinator_boxplot.pdf", height=8, width=8) #height and width in inches
pollinator_boxplot
dev.off()

##

##Natural enemy richness linear model
richmodel.n <- lmer(richness~Date + sitetype + Trap + (1|Site:Replicate), data=NE)  #AIC = 1104
summary(richmodel.n)
AIC(richmodel.n)
anova(richmodel.n) 

rich.emm<-emmeans(richmodel.n,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural (higher) and green roofs (p = 0.0115)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.t<-emmeans(richmodel.n,pairwise~Trap) 
rich.emm.t
#results: no difference between any except bowl-ramp and ramp-sticky
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(NE$richness, main = "richness") # way to visualize outliers

with(NE, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(NE, bartlett.test(richness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.7946

plot(richmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel.n))
qqline(resid(richmodel.n))

plot(simulateResiduals(richmodel.n)) # another way to check for normality and homogeneity of variance
#KS test: p = SIG
#dispersion test: p = 0.608
#outlier test: p = 1
#no significant problems detected 

densityPlot(rstudent(richmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.n)
influenceIndexPlot(richmodel.n, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Natural enemy abundance linear model
abunmodel.n <- lmer(abundance~Date + sitetype + Trap + (1|Site:Replicate), data=NE)  #AIC = 2749
summary(abunmodel.n)
AIC(abunmodel.n)
anova(abunmodel.n)

abun.emm<-emmeans(abunmodel.n,pairwise~sitetype) 
abun.emm
#results: difference between natural (higher) and green roofs (p=0.0057)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.t<-emmeans(abunmodel.n,pairwise~Trap) 
abun.emm.t
#results: no difference between any trap type
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(NE$abundance, main = "abundance") # way to visualize outliers

with(NE, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(NE, bartlett.test(abundance ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 6.409e-12

plot(abunmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel.n))
qqline(resid(abunmodel.n))

plot(simulateResiduals(abunmodel.n)) # another way to check for normality and homogeneity of variance
#KS test: p = 0  *sig deviation
#dispersion test: p = 0.576
#outlier test: p = 0.39957
#no significant problems detected 

densityPlot(rstudent(abunmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel.n)
influenceIndexPlot(abunmodel.n, vars = c("Cook"), id = list(n = 3))

#

##Natural enemy diversity linear model
divmodel.n <- lmer(diversity~Date + sitetype + Trap + (1|Site:Replicate), data=NE)  #AIC = 275
summary(divmodel.n)
AIC(divmodel.n)
anova(divmodel.n)

div.emm<-emmeans(divmodel.n,pairwise~sitetype) 
div.emm
#results: difference between natural (higher) and green roofs (p=0.0114) 
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.t<-emmeans(divmodel.n,pairwise~Trap) 
div.emm.t
#results: no difference except between bowl-ramp and ramp-sticky (maybe -- p=0.0564) 
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(NE$diversity, main = "diversity") # way to visualize outliers

with(NE, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(NE, bartlett.test(diversity ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.1929

plot(divmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel.n))
qqline(resid(divmodel.n))

plot(simulateResiduals(divmodel.n)) # another way to check for normailty and homogeneity of variance
#KS test: p = SIG
#dispersion test: p = 
#outlier test: p =
#no significant problems detected  

densityPlot(rstudent(divmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.n)
influenceIndexPlot(divmodel.n, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Natural enemy evenness linear model
evenmodel.n <- lmer(evenness~Date + sitetype + Trap + (1|Site:Replicate), data=NE)  #AIC = 219
summary(evenmodel.n)
AIC(evenmodel.n)
anova(evenmodel.n) 

even.emm<-emmeans(evenmodel.n,pairwise~sitetype) 
even.emm
#results: difference between natural (greater) and green roofs (p = 0.0065)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

even.emm.t<-emmeans(evenmodel.n,pairwise~Trap) 
even.emm.t
#results: Sig diff for all ramp traps (lowest) - same for all other comparisons
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t 

#check assumptions
dotchart(NE$evenness, main = "evenness") # way to visualize outliers

with(NE, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(NE, bartlett.test(evenness ~ sitetype)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.2373

plot(evenmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel.n))
qqline(resid(evenmodel.n))

plot(simulateResiduals(evenmodel.n)) # another way to check for normailty and homogeneity of variance
#KS test: p = 
#dispersion test: p = 
#outlier test: p =  
#no significant problems detected 

densityPlot(rstudent(evenmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel.n)
influenceIndexPlot(evenmodel.n, vars = c("Cook"), id = list(n = 3))

#####

#ggplot box plots
library (ggplot2)

#Natural enemies

#site richness by site type
richness.plot.n<-ggplot(NE,aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.n

#site abundance by site type
abundance.plot.n<-ggplot(NE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
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
abundance.plot.n

#site diversity by site type
diversity.plot.n<-ggplot(NE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.n

#site evenness by site type
evenness.plot.n<-ggplot(NE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot.n

###
#mush together plots
library(ggpubr) 
naturalenemy_boxplot <- ggarrange(richness.plot.n, diversity.plot.n, 
                                ncol = 1, nrow = 2,
                                common.legend = TRUE, legend = "bottom")
naturalenemy_boxplot

pdf("naturalenemy_boxplot.pdf", height=8, width=8) #height and width in inches
naturalenemy_boxplot
dev.off()

#

#beneficial insects on green roofs 

#Pollinators
#add data subset for green roof sites
greenroofpollinators <- P[ which(P$sitetype=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofpollinators$design<-ifelse(greenroofpollinators$Site=="EWB", "SE",
                               ifelse(greenroofpollinators$Site=="WSC", "SE", "BE"))
str(greenroofpollinators)

##Pollinator GR richness linear model
richmodel.p <- lmer(richness~Date + design + Trap + (1|Site:Replicate), data=greenroofpollinators)  #AIC = 402
summary(richmodel.p)
AIC(richmodel.p)
anova(richmodel.p) 

rich.emm<-emmeans(richmodel.p,pairwise~design) #comparing SE v BE
rich.emm
#results: no difference btw design types (p 0.3894)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.t<-emmeans(richmodel.p,pairwise~Trap) 
rich.emm.t
#results: bowls sig greater than all other trap types, same btw all others
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(greenroofpollinators$richness, main = "richness") # way to visualize outliers

with(greenroofpollinators, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofpollinators, bartlett.test(richness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.209

plot(richmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel.p))
qqline(resid(richmodel.p))

plot(simulateResiduals(richmodel.p)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.00002 SIG 
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(richmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.p)
influenceIndexPlot(richmodel.p, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Pollinator GR abundance linear model
abunmodel.p <- lmer(abundance~Date + design + Trap + (1|Site:Replicate), data=greenroofpollinators)  #AIC = 1096
summary(abunmodel.p)
AIC(abunmodel.p)
anova(abunmodel.p)

abun.emm<-emmeans(abunmodel.p,pairwise~design) 
abun.emm
#results: no difference between SE and BE (p=0.0969)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.t<-emmeans(abunmodel.p,pairwise~Trap) 
abun.emm.t
#results: bowl sig diff from jar and sticky cards (bowls caught highest abun, then ramps) - no diff btw rest
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(greenroofpollinators$abundance, main = "abundance") # way to visualize outliers

with(greenroofpollinators, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(greenroofpollinators, bartlett.test(abundance ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 2.933e-08

plot(abunmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel.p))
qqline(resid(abunmodel.p))

plot(simulateResiduals(abunmodel.p)) # another way to check for normality and homogeneity of variance
#KS test: p = 0  *sig deviation
#dispersion test: p = 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(abunmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel.p)
influenceIndexPlot(abunmodel.p, vars = c("Cook"), id = list(n = 3))

#

##Pollinator GR diversity linear model
divmodel.p <- lmer(diversity~Date + design + Trap + (1|Site:Replicate), data=greenroofpollinators)  #AIC = -103
summary(divmodel.p)
AIC(divmodel.p)
anova(divmodel.p)

div.emm<-emmeans(divmodel.p,pairwise~design) 
div.emm
#results: no difference btw SE and BE green roofs (p=0.4633)
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.t<-emmeans(divmodel.p,pairwise~Trap) 
div.emm.t
#results: bowl sig diff from all trap types (bowls caught highest div, then ramps) - no diff btw rest
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(greenroofpollinators$diversity, main = "diversity") # way to visualize outliers

with(greenroofpollinators, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofpollinators, bartlett.test(diversity ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.03511

plot(divmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel.p))
qqline(resid(divmodel.p))

plot(simulateResiduals(divmodel.p)) # another way to check for normailty and homogeneity of variance
#KS test: p = 0 *SIG
#dispersion test: p = 0.5
#outlier test: p = 0 *SIG
#no significant problems detected  

densityPlot(rstudent(divmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.p)
influenceIndexPlot(divmodel.p, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Pollinator GR evenness linear model
evenmodel.p <- lmer(evenness~Date + design + Trap + (1|Site:Replicate), data=greenroofpollinators)  #AIC = 52
summary(evenmodel.p)
AIC(evenmodel.p)
anova(evenmodel.p) 

even.emm<-emmeans(evenmodel.p,pairwise~design) 
even.emm
#results: no difference btw SE and BE green roofs (p = 0.6687)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

even.emm.t<-emmeans(evenmodel.p,pairwise~Trap) 
even.emm.t
#results: bowl sig diff from all trap types (bowls most even, then ramps) - no diff btw rest
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t 

#check assumptions
dotchart(greenroofpollinators$evenness, main = "evenness") # way to visualize outliers

with(greenroofpollinators, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofpollinators, bartlett.test(evenness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.2893

plot(evenmodel.p) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel.p))
qqline(resid(evenmodel.p))

plot(simulateResiduals(evenmodel.p)) # another way to check for normailty and homogeneity of variance
#KS test: p = 1e-05 *SIG
#dispersion test: p = 
#outlier test: p =  
#no significant problems detected 

densityPlot(rstudent(evenmodel.p)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel.p)
influenceIndexPlot(evenmodel.p, vars = c("Cook"), id = list(n = 3))

#####

#ggplot box plots
library (ggplot2)

#Pollinators

#site richness by site type
richness.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.p_gr

#site abundance by site type
abundance.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
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
abundance.plot.p_gr

#site diversity by site type
diversity.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.p_gr

#site evenness by site type
evenness.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot.p_gr

###
#mush together plots
library(ggpubr) 
pollinator.gr_boxplot <- ggarrange(richness.plot.p_gr, diversity.plot.p_gr, 
                                #labels = c("A", "B", "C", "D"),
                                ncol = 1, nrow = 2,
                                common.legend = TRUE, legend = "bottom")
pollinator.gr_boxplot

pdf("pollinator.gr_boxplot.pdf", height=8, width=8) #height and width in inches
pollinator.gr_boxplot
dev.off()

#

#Natural enemies
#add data subset for green roof sites
greenroofNE <- NE[ which(NE$sitetype=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofNE$design<-ifelse(greenroofNE$Site=="EWB", "SE",
                                    ifelse(greenroofNE$Site=="WSC", "SE", "BE"))
str(greenroofNE)

##Natural GR enemy richness linear model
richmodel.n <- lmer(richness~Date + design + Trap + (1|Site:Replicate), data=greenroofNE)  #AIC = 569
summary(richmodel.n)
AIC(richmodel.n)
anova(richmodel.n) 

rich.emm<-emmeans(richmodel.n,pairwise~design) #comparing SE and BE
rich.emm
#results: no difference btw SE and BE green roofs (p =0.2114)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

rich.emm.t<-emmeans(richmodel.n,pairwise~Trap) 
rich.emm.t
#results: no difference between any trap types
rich.cld.t<-multcomp::cld(rich.emm.t, alpha = 0.05, Letters = LETTERS)
rich.cld.t 

#check assumptions
dotchart(greenroofNE$richness, main = "richness") # way to visualize outliers

with(greenroofNE, ad.test(richness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofNE, bartlett.test(richness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.05587

plot(richmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(richmodel.n))
qqline(resid(richmodel.n))

plot(simulateResiduals(richmodel.n)) # another way to check for normality and homogeneity of variance
#KS test: p = 0.3
#dispersion test: p = 0.5
#outlier test: p = 0.27
#no significant problems detected 

densityPlot(rstudent(richmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(richmodel.n)
influenceIndexPlot(richmodel.n, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Natural enemy abundance linear model
abunmodel.n <- lmer(abundance~Date + design + Trap + (1|Site:Replicate), data=greenroofNE)  #AIC = 1305
summary(abunmodel.n)
AIC(abunmodel.n)
anova(abunmodel.n)

abun.emm<-emmeans(abunmodel.n,pairwise~design) 
abun.emm
#results: no difference btw SE and BE roofs (p=0.0947)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

abun.emm.t<-emmeans(abunmodel.n,pairwise~Trap) 
abun.emm.t
#results: no difference between any trap types 
abun.cld.t<-multcomp::cld(abun.emm.t, alpha = 0.05, Letters = LETTERS)
abun.cld.t 

#check assumptions
dotchart(greenroofNE$abundance, main = "abundance") # way to visualize outliers

with(greenroofNE, ad.test(abundance)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value = < 2.2e-16

with(greenroofNE, bartlett.test(abundance ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 6.409e-12

plot(abunmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(abunmodel.n))
qqline(resid(abunmodel.n))

plot(simulateResiduals(abunmodel.n)) # another way to check for normality and homogeneity of variance
#KS test: p = 
#dispersion test: 
#outlier test: p = 
#no significant problems detected 

densityPlot(rstudent(abunmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(abunmodel.n)
influenceIndexPlot(abunmodel.n, vars = c("Cook"), id = list(n = 3))

#

##Natural enemy diversity linear model
divmodel.n <- lmer(diversity~Date + design + Trap + (1|Site:Replicate), data=greenroofNE)  #AIC = 129
summary(divmodel.n)
AIC(divmodel.n)
anova(divmodel.n)

div.emm<-emmeans(divmodel.n,pairwise~design) 
div.emm
#results: no difference between SE and BE green roofs (p=0.1985) 
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.t<-emmeans(divmodel.n,pairwise~Trap) 
div.emm.t
#results: no difference between any trap types 
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(greenroofNE$diversity, main = "diversity") # way to visualize outliers

with(greenroofNE, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofNE, bartlett.test(diversity ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.6282

plot(divmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(divmodel.n))
qqline(resid(divmodel.n))

plot(simulateResiduals(divmodel.n)) # another way to check for normailty and homogeneity of variance
#KS test: p = 
#dispersion test: p = 
#outlier test: p =  
#no significant problems detected  

densityPlot(rstudent(divmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(divmodel.n)
influenceIndexPlot(divmodel.n, vars = c("Cook"), id = list(n = 3))

#

#NOT INCLUDING
##Natural enemy evenness linear model
evenmodel.n <- lmer(evenness~Date + design + Trap + (1|Site:Replicate), data=greenroofNE)  #AIC = 100
summary(evenmodel.n)
AIC(evenmodel.n)
anova(evenmodel.n) 

even.emm<-emmeans(evenmodel.n,pairwise~design) 
even.emm
#results: no difference between SE and BE (p = 0.8911)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld

even.emm.t<-emmeans(evenmodel.n,pairwise~Trap) 
even.emm.t
#results: no difference between any trap types
even.cld.t<-multcomp::cld(even.emm.t, alpha = 0.05, Letters = LETTERS)
even.cld.t 

#check assumptions
dotchart(greenroofNE$evenness, main = "evenness") # way to visualize outliers

with(greenroofNE, ad.test(evenness)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(greenroofNE, bartlett.test(evenness ~ design)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
#p-value = 0.627

plot(evenmodel.n) # check distribution of residuals

# check normality with these figures, are there outliers at either end
qqnorm(resid(evenmodel.n))
qqline(resid(evenmodel.n))

plot(simulateResiduals(evenmodel.n)) # another way to check for normailty and homogeneity of variance
#KS test: p = 
#dispersion test: p = 
#outlier test: p =  
#no significant problems detected 

densityPlot(rstudent(evenmodel.n)) # check density estimate of the distribution of residuals

# check for outliers influencing the data
outlierTest(evenmodel.n)
influenceIndexPlot(evenmodel.n, vars = c("Cook"), id = list(n = 3))

#####

#ggplot box plots
library (ggplot2)

#Natural enemies

#site richness by site type
richness.plot.n_gr<-ggplot(greenroofNE,aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.n_gr

#site abundance by site type
abundance.plot.n_gr<-ggplot(greenroofNE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = abundance, fill=Site))+
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
abundance.plot.n_gr

#site diversity by site type
diversity.plot.n_gr<-ggplot(greenroofNE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.n_gr

#site evenness by site type
evenness.plot.n_gr<-ggplot(greenroofNE, aes(x = factor(sitetype,level = c("Natural","Greenroof")), y = evenness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Evenness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=even.cld.s, aes(y = 25, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
evenness.plot.n_gr

###
#mush together plots
library(ggpubr) 
naturalenemy.gr_boxplot <- ggarrange(richness.plot.n_gr, diversity.plot.n_gr, 
                                  ncol = 1, nrow = 2,
                                  common.legend = TRUE, legend = "bottom")
naturalenemy.gr_boxplot

pdf("naturalenemy.gr_boxplot.pdf", height=8, width=8) #height and width in inches
naturalenemy.gr_boxplot
dev.off()
