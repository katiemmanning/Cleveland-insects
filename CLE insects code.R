#bring in 2019 datasets
bowls19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202019%20-%20Bowl_cleveland.csv",na.strings = NULL)
summary(bowls19)
str(bowls19) 
ramps19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202019%20-%20Ramp_cleveland.csv",na.strings = NULL)
summary(ramps19)
str(ramps19)
sticky19 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202019%20-%20Sticky%20card_cleveland.csv",na.strings = NULL)
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
allbugs19$habitat<-ifelse(allbugs19$Site=="SSH", "Ground-level",
                      ifelse(allbugs19$Site=="DGM", "Ground-level",
                          ifelse(allbugs19$Site=="BFB", "Ground-level", "Greenroof")))
str(allbugs19)

#To obtain richness counts
allbugs19.rowsums <- rowSums(allbugs19[,5:44]>0)
allbugs19$richness <- allbugs19.rowsums

#load vegan
library(vegan)

#calculate Shannon diversity
diversity <-diversity(allbugs19[,5:44])
allbugs19$diversity <-diversity

summary(allbugs19)
str(allbugs19)

#add data subset for green roof 
greenroofbugs19 <- allbugs19[ which(allbugs19$habitat=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofbugs19$design<-ifelse(greenroofbugs19$Site=="EWB", "SE",
                                ifelse(greenroofbugs19$Site=="WSC", "SE", "BE"))
str(greenroofbugs19)

#add data subset for natural sites
naturalbugs19 <- allbugs19[ which(allbugs19$habitat=="Ground-level"), ]
str(naturalbugs19)

######
#bring in 2021 data sets
bowls21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202021%20-%20Bowl.csv",na.strings = NULL)
summary(bowls21)
str(bowls21) 
jars21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202021%20-%20Jar.csv",na.strings = NULL)
summary(jars21)
str(jars21)
sticky21 <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/Insect%20ID%202021%20-%20Sticky%20card.csv",na.strings = NULL)
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
allbugs21$habitat<-ifelse(allbugs21$Site=="SSH", "Ground-level",
                           ifelse(allbugs21$Site=="DGM", "Ground-level",
                                  ifelse(allbugs21$Site=="BFB", "Ground-level", "Greenroof")))
str(allbugs21)

#To obtain richness counts
allbugs21.rowsums <- rowSums(allbugs21[,5:44]>0)
allbugs21$richness <- allbugs21.rowsums

#calculate Shannon diversity
diversity <-diversity(allbugs21[,5:44])
allbugs21$diversity <-diversity

summary(allbugs21)
str(allbugs21)

#add data subset for green roof 
greenroofbugs21 <- allbugs21[ which(allbugs21$habitat=="Greenroof"), ]
#add column for green roof functional intent (stormwater-energy and biodiversity-ecological)
greenroofbugs21$design<-ifelse(greenroofbugs21$Site=="EWB", "SE",
                                  ifelse(greenroofbugs21$Site=="WSC", "SE", "BE"))
str(greenroofbugs21)

#add data subset for natural sites
naturalbugs21 <- allbugs21[ which(allbugs21$habitat=="Ground-level"), ]
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
richmodel <- lmer(richness~Date + habitat + Trap + (1|Site:Replicate), data=allbugs)  #AIC = 1774
summary(richmodel)
AIC(richmodel)
anova(richmodel) 

rich.emm<-emmeans(richmodel,pairwise~habitat) #comparing ground-level vs GR
rich.emm
#results: difference between ground-level (higher) and green roofs (p = 0.03)
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

##diversity linear model
divmodel <- lmer(diversity~Date + habitat + Trap + (1|Site:Replicate), data=allbugs)  #AIC = 548
summary(divmodel)
AIC(divmodel)
anova(divmodel)

div.emm<-emmeans(divmodel,pairwise~habitat) 
div.emm
#results: no difference between ground-level and green roofs (p=0.5287)
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

#######
#ggplot box plots
library (ggplot2)

#site richness by site type
richness.plot<-ggplot(allbugs, aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = richness, fill=Site))+
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

#site diversity by site type
diversity.plot<-ggplot(allbugs, aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot

###
#mush together plots
library(ggpubr) 
allbugs_boxplot <- ggarrange(richness.plot, diversity.plot,
                             #labels = c("A", "B"),
                             ncol = 2, nrow = 1,
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
  #theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.d

#site diversity by site type
diversity.plot.d<-ggplot(greenroofbugs, aes(x = factor(design,level = c("SE","BE")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  #theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.d

###
#mush together plots
library(ggpubr) 
greenroofbugs_boxplot <- ggarrange(richness.plot.d, diversity.plot.d,
                             #labels = c("A", "B", "C", "D"),
                             ncol = 2, nrow = 1,
                             common.legend = TRUE, legend = "bottom")
greenroofbugs_boxplot

pdf("greenroofbugs_boxplot.pdf", height=8, width=8) #height and width in inches
greenroofbugs_boxplot
dev.off()

###

#merge boxplots into one figure
multipanel_boxplot <- ggarrange(allbugs_boxplot, greenroofbugs_boxplot,
                                   labels = c("A","B"),
                                   ncol = 1, nrow = 2,
                                   common.legend = TRUE, legend = "bottom")
multipanel_boxplot

pdf("multipanel_boxplot.pdf", height=8, width=8) #height and width in inches
multipanel_boxplot
dev.off()

#NMDS of insect community 
library (vegan)

#bring in data pooled by date-site
allbugs_pooled <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/allbugs_2019%20and%202021_pooled.csv", na.strings = NULL)

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

#plot NMDS 
#might need to change colors
#8 x 13
plot(NMDS, disp='sites', type="n")
#title(main="Arthropod community composition by site type", cex.main=1.5)
#add ellipsoids with ordiellipse
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
#add data points
points(NMDS, display="sites", select=which(env.matrix$habitat=="Ground-level"),pch=19, col="#E69F00")
points(NMDS, display="sites", select=which(env.matrix$habitat=="Greenroof"), pch=17, col="#009E73")
#add legend
legend(0.92,0.68, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Ground-level", "Greenroof"))

#testing for differences between the groups (GR and ground-level) with adonis aka PERMANOVA
fit<-adonis2(com.matrix ~ habitat, data = env.matrix, permutations = 999, method="jaccard")
fit
#P=0.001 - groups have significantly different compositions

#check assumption of homogeneity of multivariate dispersion using an ANOVA on the multivariate dispersions (betadisper)
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix, method="jaccard")
anova(betadisper(distances_data, env.matrix$habitat))
#P-value = 0.01 -- cannot assume homogeneity of multivariate dispersion

#conclusion: ground-level and green roof habitats present heterogeneity among group dispersion (compositions vary differently) 
            # and have significantly different compositions. 

##

#NMDS of ground-level versus SE green roof
SE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/allbugs_pooled_SE%20and%20ground-level.csv", na.strings=NULL)

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
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
#add data points
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="Ground-level"),pch=19, col="#E69F00")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="SE"), pch=17, col="#009E73")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#E69F00","#009E73"), cex=1.5, legend=c("Natural", "SE"))

#bootstrapping and testing for differences between the groups (SE and ground-level)
fit<-adonis2(com.matrix_SE ~ type, data = env.matrix_SE, permutations = 999, method="jaccard")
fit
#P=0.01 - groups have significantly different compositions

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_SE)
anova(betadisper(distances_data, env.matrix_SE$type))
#P-value = 0.006 -- cannot assume homogeneity of multivariate dispersion

#conclusion: ground-level and SE green roof habitats present heterogeneity among group dispersion (compositions vary differently) 
# and have significantly different compositions. 

#

#NMDS of ground-level versus BE green roof
BE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/allbugs_pooled_BE%20and%20ground-level.csv", na.strings=NULL)

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
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
#add data points
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="Ground-level"),pch=19, col="#009E73")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="BE"), pch=17, col="#CC79A7")
#add legend
#legend(0.5,0.5, title=NULL, pch=c(19,17), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Natural", "BE"))

#bootstrapping and testing for differences between the groups (BE and ground-level)
fit<-adonis2(com.matrix_BE ~ type, data = env.matrix_BE, permutations = 999, method="jaccard")
fit
#P=0.001 - groups have significantly different compositions

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_BE)
anova(betadisper(distances_data, env.matrix_BE$type))
#P-value = 0.20 -- assumes homogeneity of multivariate dispersion

#conclusion: ground-level and BE green roof habitats present homogeneity among group dispersion (compositions vary similarly) 
# but have significantly different compositions. 

#

#merge habitat and mitigation NMDSs into one figure
pdf("design type NMDSs.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS_SE, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="Ground-level"),pch=19, col="#009E73")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="SE"), pch=15, col="#F0E442")
legend(-0.78,1.9, title=NULL, pch=c(19,15), col=c("#009E73","#F0E442"), cex=1.5, legend=c("Ground-level", "SE"))

plot(NMDS_BE, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="Ground-level"),pch=19, col="#009E73")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="BE"), pch=18, col="#CC79A7")
legend(-0.32,1.525, title=NULL, pch=c(19,18), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Ground-level", "BE"))
dev.off()
###

#NMDS of green roof insect community 
library (vegan)

#bring in data pooled by date-site
greenroofbugs_pooled <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/greenroofbugs_2019%20and%202021_pooled.csv", na.strings = NULL)

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
fit<-adonis2(com.matrix_gr ~ design, data = env.matrix_gr, permutations = 999, method="jaccard")
fit
#P=0.6 - groups are compositionally similar 

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_gr)
anova(betadisper(distances_data, env.matrix_gr$design))
#P-value = 0.2024 -- assumes homogeneity of multivariate dispersion

#conclusion: SE and BE green roof habitats present homogeneity among group dispersion (compositions vary similarly) 
# and have similar compositions. 

#

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

#bootstrapping and testing for differences between the groups (GR sites)
fit<-adonis2(com.matrix_gr ~ Site, data = env.matrix_gr, permutations = 999, method="bray")
fit
#P > 0.05  ~0.08

library (pairwiseAdonis)
pairwise.adonis(com.matrix_gr, env.matrix_gr$Site)
#no sig differences

#check assumption of homogeneity of multivariate dispersion 
#P-value greater than 0.05 means assumption has been met
distances_data<-vegdist(com.matrix_gr)
anova(betadisper(distances_data, env.matrix_gr$Site))
#P-value = 0.5663 -- assumes homogeneity of multivariate dispersion

#conclusion: All green roof sites present homogeneity among group dispersion (compositions vary similarly) 
# and have similar compositions. 

###

#merge allbugs and GR NMDSs into one figure and print to PDF
pdf("allbugs+grbugs NMDSs.pdf", height=6.5, width=13)
par(mfrow=c(1,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS, display="sites", select=which(env.matrix$habitat=="Ground-level"),pch=19, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$habitat=="Greenroof"), pch=17, col="#E69F00")
legend(-0.53,1.535, title=NULL, pch=c(19,17), col=c("#009E73","#E69F00"), cex=1.5, legend=c("Ground-level", "Greenroof"))

plot(NMDS_gr, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="BE"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="SE"), pch=15, col="#F0E442")
legend(0.255,1.615, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("BE", "SE"))
dev.off()

##Put all 4 NMDSs together for manuscript

pdf("All NMDSs.pdf", height=12, width=12)
par(mfrow=c(2,2), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(NMDS, disp='sites', type="n")
title(main="A", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#E69F00",kind="sd", conf=0.95, label=FALSE, show.groups = "Greenroof")
ordiellipse(NMDS, env.matrix$habitat, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS, display="sites", select=which(env.matrix$habitat=="Ground-level"),pch=19, col="#009E73")
points(NMDS, display="sites", select=which(env.matrix$habitat=="Greenroof"), pch=17, col="#E69F00")
legend(-0.348,1.475, title=NULL, pch=c(19,17), col=c("#009E73","#E69F00"), cex=1.5, legend=c("Ground-level", "Greenroof"))

plot(NMDS_gr, disp='sites', type="n")
title(main="B", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_gr, env.matrix_gr$design, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="BE"),pch=18, col="#CC79A7")
points(NMDS_gr, display="sites", select=which(env.matrix_gr$design=="SE"), pch=15, col="#F0E442")
legend(0.333,1.55, title=NULL, pch=c(18,15), col=c("#CC79A7","#F0E442"), cex=1.5, legend=c("BE", "SE"))

plot(NMDS_SE, disp='sites', type="n")
title(main="C", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#F0E442",kind="sd", conf=0.95, label=FALSE, show.groups = "SE")
ordiellipse(NMDS_SE, env.matrix_SE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="Ground-level"),pch=19, col="#009E73")
points(NMDS_SE, display="sites", select=which(env.matrix_SE$type=="SE"), pch=15, col="#F0E442")
legend(-0.56,1.82, title=NULL, pch=c(19,15), col=c("#009E73","#F0E442"), cex=1.5, legend=c("Ground-level", "SE"))

plot(NMDS_BE, disp='sites', type="n")
title(main="D", adj = 0.02, line = -2, cex.main=1.5)
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#CC79A7",kind="sd", conf=0.95, label=FALSE, show.groups = "BE")
ordiellipse(NMDS_BE, env.matrix_BE$type, draw="polygon", col="#009E73",kind="sd", conf=0.95, label=FALSE, show.groups = "Ground-level")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="Ground-level"),pch=19, col="#009E73")
points(NMDS_BE, display="sites", select=which(env.matrix_BE$type=="BE"), pch=18, col="#CC79A7")
legend(-0.16,1.468, title=NULL, pch=c(19,18), col=c("#009E73","#CC79A7"), cex=1.5, legend=c("Ground-level", "BE"))
dev.off()

###

#beneficial insect analyses

#import data
P <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/pollinators_2019%20and%202021.csv", na.strings = NULL)
NE <- read.csv("https://raw.githubusercontent.com/katiemmanning/Cleveland-insects/main/Data/natural%20enemies_2019%20and%202021.csv", na.strings = NULL)

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
richmodel.p <- lmer(richness~Date + habitat + Trap + (1|Site:Replicate), data=P)  #AIC = 813
summary(richmodel.p)
AIC(richmodel.p)
anova(richmodel.p) 

rich.emm<-emmeans(richmodel.p,pairwise~habitat) #comparing ground-level vs GR
rich.emm
#results: difference between ground-level (higher) and green roofs (p < 0.0001)
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

##Pollinator diversity linear model
divmodel.p <- lmer(diversity~Date + habitat + Trap + (1|Site:Replicate), data=P)  #AIC = -101
summary(divmodel.p)
AIC(divmodel.p)
anova(divmodel.p)

div.emm<-emmeans(divmodel.p,pairwise~habitat) 
div.emm
#results: difference between ground-level (higher) and green roofs (p=0.0099) 
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

#####

#ggplot box plots
library (ggplot2)

#Pollinators

#site richness by site type
richness.plot.p<-ggplot(P, aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = richness, fill=Site))+
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

#site diversity by site type
diversity.plot.p<-ggplot(P, aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.p

###
#mush together plots
library(ggpubr) 
pollinator_boxplot <- ggarrange(richness.plot.p, diversity.plot.p, 
                             ncol = 2, nrow = 1,
                             common.legend = TRUE, legend = "bottom")
pollinator_boxplot

pdf("pollinator_boxplot.pdf", height=8, width=8) #height and width in inches
pollinator_boxplot
dev.off()

##

##Natural enemy richness linear model
richmodel.n <- lmer(richness~Date + habitat + Trap + (1|Site:Replicate), data=NE)  #AIC = 1104
summary(richmodel.n)
AIC(richmodel.n)
anova(richmodel.n) 

rich.emm<-emmeans(richmodel.n,pairwise~habitat) #comparing ground-level vs GR
rich.emm
#results: difference between ground-level (higher) and green roofs (p = 0.0115)
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

##Natural enemy diversity linear model
divmodel.n <- lmer(diversity~Date + habitat + Trap + (1|Site:Replicate), data=NE)  #AIC = 275
summary(divmodel.n)
AIC(divmodel.n)
anova(divmodel.n)

div.emm<-emmeans(divmodel.n,pairwise~habitat) 
div.emm
#results: difference between ground-level (higher) and green roofs (p=0.0114) 
div.cld<-multcomp::cld(div.emm, alpha = 0.05, Letters = LETTERS)
div.cld 

div.emm.t<-emmeans(divmodel.n,pairwise~Trap) 
div.emm.t
#results: no difference except between bowl-ramp 
div.cld.t<-multcomp::cld(div.emm.t, alpha = 0.05, Letters = LETTERS)
div.cld.t 

#check assumptions
dotchart(NE$diversity, main = "diversity") # way to visualize outliers

with(NE, ad.test(diversity)) #Anderson-darling test for normality (good for small sample sizes), low p-value means assumption is violated
#p-value < 2.2e-16

with(NE, bartlett.test(diversity ~ habitat)) #Bartlett test for homogeneity of variance, low p-value means assumption is violated
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

#####

#ggplot box plots
library (ggplot2)

#Natural enemies

#site richness by site type
richness.plot.n<-ggplot(NE,aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = richness, fill=Site))+
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

#site diversity by site type
diversity.plot.n<-ggplot(NE, aes(x = factor(habitat,level = c("Ground-level","Greenroof")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_brewer(palette="Paired",name="Sites:",
                    breaks=c("BFB", "DGM", "SSH", "EWB", "WSC", "HDB", "SNC"),
                    labels=c("Bedford barren","Dusty goldenrod meadow", "Slate shale hill", "Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.n

###
#mush together plots
library(ggpubr) 
naturalenemy_boxplot <- ggarrange(richness.plot.n, diversity.plot.n, 
                                ncol = 2, nrow = 1,
                                common.legend = TRUE, legend = "bottom")
naturalenemy_boxplot

pdf("naturalenemy_boxplot.pdf", height=8, width=8) #height and width in inches
naturalenemy_boxplot
dev.off()

#

#beneficial insects on green roofs 

#Pollinators
#add data subset for green roof sites
greenroofpollinators <- P[ which(P$habitat=="Greenroof"), ]
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

#####

#ggplot box plots
library (ggplot2)

#Pollinators

#site richness by site type
richness.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(design,level = c("SE","BE")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.p_gr

#site diversity by site type
diversity.plot.p_gr<-ggplot(greenroofpollinators, aes(x = factor(design,level = c("SE","BE")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.p_gr

###
#mush together plots
library(ggpubr) 
pollinator.gr_boxplot <- ggarrange(richness.plot.p_gr, diversity.plot.p_gr, 
                                ncol = 2, nrow = 1,
                                common.legend = TRUE, legend = "bottom")
pollinator.gr_boxplot

pdf("pollinator.gr_boxplot.pdf", height=8, width=8) #height and width in inches
pollinator.gr_boxplot
dev.off()

#

#Natural enemies
#add data subset for green roof sites
greenroofNE <- NE[ which(NE$habitat=="Greenroof"), ]
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

#####

#ggplot box plots
library (ggplot2)

#Natural enemies

#site richness by site type
richness.plot.n_gr<-ggplot(greenroofNE,aes(x = factor(design,level = c("SE","BE")), y = richness, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Richness")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=rich.cld, aes(y = 25, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
richness.plot.n_gr

#site diversity by site type
diversity.plot.n_gr<-ggplot(greenroofNE, aes(x = factor(design,level = c("SE","BE")), y = diversity, fill=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title="", x="", y="Shannon Diversity")+
  #theme (plot.title = element_text(hjust=0.5))+
  #geom_text(data=div.cld.s, aes(y = 2, label = .group))+
  scale_fill_manual(values=c("#B2DF8A","#FDBF6F","#33A02C","#FB9A99"),name="Sites:",
                    breaks=c("EWB", "WSC", "HDB", "SNC"),
                    labels=c("Edgewater beach", "Watershed stewardship center", "Happy dog bike box", "Shaker Lakes nature center"))
diversity.plot.n_gr

###
#mush together plots
library(ggpubr) 
naturalenemy.gr_boxplot <- ggarrange(richness.plot.n_gr, diversity.plot.n_gr, 
                                  ncol = 2, nrow = 1,
                                  common.legend = TRUE, legend = "bottom")
naturalenemy.gr_boxplot

pdf("naturalenemy.gr_boxplot.pdf", height=8, width=8) #height and width in inches
naturalenemy.gr_boxplot
dev.off()

#merge beneficial insects boxplots into one figure
#pollinator
combinedpollinator_boxplot <- ggarrange(pollinator_boxplot, pollinator.gr_boxplot,
                                labels = c("Pollinator",""),
                                ncol = 1, nrow = 2,
                                common.legend = TRUE, legend = "bottom")
combinedpollinator_boxplot

pdf("combinedpollinator_boxplot.pdf", height=8, width=8) #height and width in inches
combinedpollinator_boxplot
dev.off()

#natural enemies
combinedNE_boxplot <- ggarrange(naturalenemy_boxplot, naturalenemy.gr_boxplot,
                                labels = c("Natural enemy",""),
                                ncol = 1, nrow = 2,
                                common.legend = TRUE, legend = "bottom")
combinedNE_boxplot

pdf("combinedNE_boxplot.pdf", height=8, width=8) #height and width in inches
combinedNE_boxplot
dev.off()
