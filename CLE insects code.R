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
greenroofbugs21$planttype<-ifelse(greenroofbugs21$Site=="EWB", "Mitigation",
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
write.csv(allbugs, file="allbugs_2019 and 2021.csv", row.names=FALSE)

#models and checking assumptions
library (emmeans) #for pairwise comparisons
library (lme4)
library (lmerTest) #to obtain p values
library (multcompView) #to view letters
library (nortest)
library (car)
library (bbmle)
library (DHARMa)
library (ggplot2)
library (sjPlot)
library (jtools)
library (interactions)

##richness linear model
richmodel <- lm(richness~Date + Site + sitetype, data=allbugs)  #AIC = 1930
summary(richmodel)
AIC(richmodel)
anova(richmodel) 

rich.emm<-emmeans(richmodel,pairwise~sitetype) #comparing natural vs GR
rich.emm
#results: difference between natural and green roofs (p=0.0006)
rich.cld<-multcomp::cld(rich.emm, alpha = 0.05, Letters = LETTERS)
rich.cld 

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
#abunmodel <- glmer(abundance~region + Date + Trap + (1|Site:Replicate),data=allbugs, family = negative.binomial(2)) #AIC 2501
abunmodel <- lm(abundance~Date + Site + sitetype, data=allbugs)  #AIC = 6321
abunmodel <- glm(abundance~Date + Site + sitetype, data=allbugs, family = negative.binomial(4))  #AIC = 
summary(abunmodel)
AIC(abunmodel)
anova(abunmodel)
abun.emm<-emmeans(abunmodel,pairwise~sitetype) 
abun.emm
#results: no difference between natural and green roofs (p=0.35)
abun.cld<-multcomp::cld(abun.emm, alpha = 0.05, Letters = LETTERS)
abun.cld 

#check assumptions
dotchart(allbugs$abundance, main = "abundance") # way to visualize outliers
#clustered towards 0 --- outlier of 7000

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

##diversity linear mixed effects model
divmodel <- lm(diversity~Date + Site + sitetype, data=allbugs)  #AIC = 526
summary(divmodel)
AIC(divmodel)
anova(divmodel)

div.emm<-emmeans(divmodel,pairwise~sitetype) 
div.emm
#results: no difference between natural and green roofs (p=0.6176)
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

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
evenmodel <- lm(evenness~Date + Site + sitetype, data=allbugs)  #AIC = -67
summary(evenmodel)
AIC(evenmodel)
anova(evenmodel) 

even.emm<-emmeans(evenmodel,pairwise~sitetype) 
even.emm
#results: difference between natural and green roofs (p < 0.0001)
even.cld<-multcomp::cld(even.emm, alpha = 0.05, Letters = LETTERS)
even.cld 

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
#KS test: p = 0.1027
#dispersion test: p = 0.6
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
  geom_text(data=rich.cld, aes(y = 25, label = .group))+
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
  geom_text(data=abun.cld, aes(y = 25, label = .group))+
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
  geom_text(data=div.cld, aes(y = 25, label = .group))+
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
  geom_text(data=even.cld, aes(y = 25, label = .group))+
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

#plot NMDS for region
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
legend(0.92,0.68, title=NULL, pch=c(19,17,15), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Natural", "Greenroof"))

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
