#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(arm)
library(car)

##do not have chlorophyll or lcp for ferns

alldata <- read.csv("raw_data/master_data.csv")
alldata$sla <- with(alldata, 1/LMA)
alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)
alldata$pmass <- with(alldata, P * 10)
alldata$nue <- with(alldata, amass/nmass)
alldata$pue <- with(alldata, amass/pmass)
alldata$narea <- with(alldata, N * LMA)
alldata$parea <- with(alldata, P * LMA)

ferns <- alldata[alldata$family=="Ferns",] 
sela <- alldata[alldata$family=="Selaginella",]
hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

###Use lsmeans to compare slopes

#a vs gs
#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(smatr)
library(plotrix)

##do not have chlorophyll or lcp for ferns

alldata <- read.csv("raw_data/master_data.csv")
alldata$sla <- with(alldata, 1/LMA)
alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)
alldata$pmass <- with(alldata, P * 10)
alldata$nue <- with(alldata, amass/nmass)
alldata$pue <- with(alldata, amass/pmass)
alldata$narea <- with(alldata, N * LMA)
alldata$parea <- with(alldata, P * LMA)

ferns <- alldata[alldata$family=="Ferns",] 
sela <- alldata[alldata$family=="Selaginella",]
hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

library(scales)
ferncol<- alpha("royalblue", .85)
fernline <- "royalblue"
anova(test.ags)
summary(test.ags)
r.squaredGLMM(test.ags)

ags_slopes <- lstrends(test.ags, "habitat", var="gs")
pairs(ags_slopes)
#open & cc differ

#amass vs lma
test.amasslma <- lm(amass ~ LMA * habitat, data=sela2)
anova(test.amasslma)
summary(test.amasslma)

alma_slopes <- lstrends(test.amasslma, "habitat", var="LMA")
pairs(alma_slopes)

#amass vs nmass
test.amassn <- lm(amass ~ nmass * habitat, data=sela2)
anova(test.amassn)
summary(test.amassn)

an_slopes <- lstrends(test.amassn, "habitat", var="nmass")
pairs(an_slopes)

#amass vs pmass
test.amassp <- lm(amass ~ pmass * habitat, data=sela2)
anova(test.amassp)
summary(test.amassp)

ap_slopes <- lstrends(test.amassp, "habitat", var="pmass")
pairs(ap_slopes)

#nue-lma
test.nuelma <- lm(nue ~ LMA * habitat, data=sela2)
anova(test.nuelma)
summary(test.nuelma)

nuelma_slopes <- lstrends(test.nuelma, "habitat", var="LMA")
pairs(nuelma_slopes)

#pue-lma
test.puelma <- lm(pue ~ LMA, data=sela2)
anova(test.puelma)
summary(test.puelma)

nuelma_slopes <- lstrends(test.nuelma, "habitat", var="LMA")
pairs(nuelma_slopes)
