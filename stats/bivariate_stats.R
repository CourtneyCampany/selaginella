##bivariate stats

#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(arm)
library(car)
library(plotrix)

alldata <- read.csv("raw_data/master_data.csv")
alldata$sla <- with(alldata, 1/LMA)
alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)

ferns <- alldata[alldata$family=="Ferns",] 
ferns2 <- ferns[complete.cases(ferns$asat),]

sela <- alldata[alldata$family=="Selaginella",]
hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

### Selaginella models ------

#photo gs
ags_mod <- lmer(asat ~ gs * habitat + (1|species), data=sela2)

  Anova(ags_mod)
  summary(ags_mod)
  r.squaredGLMM(ags_mod)
# R2m       R2c 
# 0.5132483 0.9115119 
#P<o.ooo1

#no interaction with habitat, but.....
  ags_slopes <- lstrends(ags_mod, "habitat", var="gs")
  pairs(ags_slopes)
#not different

#photo-nitro
an_mod <- lmer(amass ~ nmass * habitat + (1|species), data=sela2)

  Anova(an_mod)
  summary(an_mod)
  r.squaredGLMM(an_mod)
# R2m       R2c 
# 0.4271145 0.8026485
#P=o.oo9

#no interaction with habitat, but.....
an_slopes <- lstrends(an_mod, "habitat", var="nmass")
pairs(an_slopes)
#not different

#conductance & stomatal density
gssd_mod <- lmer(gs ~ sto_dens * habitat + (1|species), data=sela2)
  Anova(gssd_mod)
  summary(gssd_mod)
  r.squaredGLMM(gssd_mod)
#not significant


#ferns
ags_mod2 <- lmer(asat ~ gs + (1|species), data=ferns2)
an_mod2 <- lmer(amass ~ nmass  + (1|species), data=ferns2)
gssd_mod2 <- lmer(gs ~ sto_dens  + (1|species), data=ferns2)
