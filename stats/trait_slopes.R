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
library(lmerTest)
library(LMERConvenienceFunctions)

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

ferns <- droplevels(alldata[alldata$family=="Ferns",] )
sela <- droplevels(alldata[alldata$family=="Selaginella",])

hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

###mixed model because of habitat
###Use lsmeans to compare slopes when habitat effect

#a vs gs-------------------------------

#sela
sela.ags <- lmer(asat ~ gs * habitat + (1|species), data=sela2)
anova(test.ags)
summary(test.ags)
r.squaredGLMM(test.ags)

ags_slopes <- lstrends(test.ags, "habitat", var="gs")
pairs(ags_slopes) #not different (shouldnt be)

###ferns
ferns.ags <- lmer(asat ~ gs  + (1|species), data=ferns)
anova(ferns.ags)
summary(ferns.ags)
r.squaredGLMM(ferns.ags)

#amass vs lma -----------
# test.amasslma <- lm(amass ~ LMA * habitat, data=sela2)
# anova(test.amasslma)
# summary(test.amasslma)

#sela
sela.amasslma2 <- lmer(amass ~ LMA * habitat + (1|species), data=sela2)
anova(sela.amasslma2)
summary(sela.amasslma2)
r.squaredGLMM(sela.amasslma2)

emmip(sela.amasslma2, LMA ~ habitat) 
test_lmahabt <- emmeans(sela.amasslma2, pairwise ~ habitat)

alma_slopes <- lstrends(sela.amasslma2, "habitat", var="LMA")
pairs(alma_slopes) #not different (shouldnt be)

#ferns
boxplot(amass ~ species, data = ferns)

ferns.amasslma <- lmer(amass ~ LMA + (1|species), data=ferns)
anova(ferns.amasslma)
summary(ferns.amasslma)
r.squaredGLMM(ferns.amasslma)

#amass vs nmass-------
# test.amassn <- lm(amass ~ nmass * habitat, data=sela2)
# anova(test.amassn)
# summary(test.amassn)

sela.amassn2 <- lmer(amass ~ nmass * habitat + (1|species), data=sela2)
anova(sela.amassn2)
summary(sela.amassn2)
r.squaredGLMM(sela.amassn2)

emmip(sela.amassn2, nmass ~ habitat) 
test_amnm <- emmeans(sela.amassn2, pairwise ~ habitat)
an_slopes <- lstrends(sela.amassn2, "habitat", var="nmass")
pairs(an_slopes)

#ferns
ferns.amassn <- lmer(amass ~ nmass + (1|species), data=ferns)
anova(ferns.amassn)
summary(ferns.amassn)
r.squaredGLMM(ferns.amassn)

ferns.amassn2 <- lm(amass ~ nmass , data=ferns)
anova(ferns.amassn2)

#amass vs pmass -----
sela.amassp <- lm(amass ~ pmass * habitat, data=sela2)
anova(sela.amassp)
# summary(test.amassp)

sela.amassp2 <- lmer(amass ~ pmass * habitat + (1|species), data=sela2)
anova(sela.amassp2)
summary(sela.amassp2)
r.squaredGLMM(sela.amassp2)

emmip(sela.amassp2, pmass ~ habitat) 
test_amnm <- emmeans(sela.amassp2, pairwise ~ habitat)
ap_slopes <- lstrends(sela.amassp2, "habitat", var="pmass")
pairs(ap_slopes)

#ferns
ferns.amassp <- lmer(amass ~ pmass  + (1|species), data=ferns)
anova(ferns.amassp)
summary(ferns.amassp)
r.squaredGLMM(ferns.amassp)

#nue-lma-------

#sela
sela.nuelma <- lm(nue ~ LMA * habitat, data=sela2)
anova(sela.nuelma)
summary(sela.nuelma)

sela.nuelma2 <- lmer(nue ~ LMA * habitat + (1|species), data=sela2)
anova(sela.nuelma2)
summary(sela.nuelma2)
r.squaredGLMM(sela.nuelma2)
emmip(sela.nuelma2, LMA ~ habitat) 
nuelma_slopes <- lstrends(sela.nuelma2, "habitat", var="LMA")
pairs(nuelma_slopes)

#ferns
ferns.nuelma <- lmer(nue ~ LMA + (1|species), data=ferns)
anova(ferns.nuelma)
summary(ferns.nuelma)
r.squaredGLMM(ferns.nuelma)

#pue-lma---------
sela.puelma <- lm(pue ~ LMA*habitat, data=sela2)
anova(sela.puelma)
summary(sela.puelma)

sela.puelma2 <- lmer(pue ~ LMA * habitat + (1|species), data=sela2)
anova(sela.puelma2)
summary(sela.puelma2)
r.squaredGLMM(sela.puelma2)
emmip(sela.puelma2, LMA ~ habitat) 
puelma_slopes <- lstrends(sela.puelma2, "habitat", var="LMA")
pairs(puelma_slopes)

#ferns
ferns.puelma <- lmer(pue ~ LMA + (1|species), data=ferns)
anova(ferns.puelma)
summary(ferns.puelma)
r.squaredGLMM(ferns.puelma)

##gs - sd -----
sela.gssd <- lm(gs ~ sto_dens*habitat, data=sela2)
anova(sela.gssd)
summary(sela.gssd)

sela.gssd2 <- lmer(gs ~ sto_dens * habitat + (1|species), data=sela2)
anova(sela.gssd2)
summary(sela.gssd2)
r.squaredGLMM(sela.gssd2)
emmip(sela.gssd2, sto_dens ~ habitat) 
gssd_slopes <- lstrends(sela.gssd2, "habitat", var="sto_dens")
pairs(gssd_slopes)

#ferns
ferns.gssd <- lmer(gs ~ sto_dens + (1|species), data=ferns) #gs by species big
anova(ferns.gssd)
summary(ferns.gssd)
r.squaredGLMM(ferns.gssd)
