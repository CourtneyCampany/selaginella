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
alldata$pmass <- with(alldata, P * 10)
alldata$nue <- with(alldata, amass/nmass)
alldata$pue <- with(alldata, amass/pmass)

ferns <- alldata[alldata$family=="Ferns",] 
ferns2 <- ferns[complete.cases(ferns$asat),]

sela <- alldata[alldata$family=="Selaginella",]
hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

#remove obvous outliers for PPUE
pue_ferns <- ferns2[ferns2$pue < 400,]
pue_ferns <- pue_ferns[complete.cases(pue_ferns$family),]

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
  ags_slopes <- lstrends(ags_mod, "habitat", var="gs") #obtain slopes
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

##photo - P
ap_mod <- lmer(amass ~ pmass * habitat + (1|species), data=sela2)

Anova(ap_mod)
summary(ap_mod)
r.squaredGLMM(ap_mod)

ap_slopes <- lstrends(ap_mod, "habitat", var="pmass")
pairs(ap_slopes)

#area based
ap_mod_area <- lmer(asat ~ P * habitat + (1|species), data=sela2)
Anova(ap_mod_area)

#conductance & stomatal density
gssd_mod <- lmer(asat ~ sto_dens * habitat + (1|species), data=sela2)
  Anova(gssd_mod)
  summary(gssd_mod)
  r.squaredGLMM(gssd_mod)
#not significant
  
gssl_mod <- lmer(asat ~ sto_length * habitat + (1|species), data=sela2)
  Anova(gssl_mod) 
  
##lma and NNUE & PPUE
pnue_mod_sela <- lmer(nue ~ LMA * habitat+ (1|species), data=sela2)
  Anova(pnue_mod_sela)
  r.squaredGLMM(pnue_mod_sela)
  
  # R2m       R2c 
  # 0.4135190 0.8549372 
  # P < 0.023 = habitat only
  
ppue_mod_sela <- lmer(pue ~ LMA + habitat + (1|species), data=sela2)
  Anova(ppue_mod_sela)
  r.squaredGLMM(ppue_mod_sela)
  
##photo and lma
plma_mod_sela <- lmer(amass ~ LMA * habitat+ (1|species), data=sela2)
  Anova(plma_mod_sela)
  r.squaredGLMM(plma_mod_sela)  
  
alma_slopes <- lstrends(plma_mod_sela, "habitat", var="LMA")
  pairs(alma_slopes)

#ferns-----
ags_mod2 <- lmer(asat ~ gs + (1|species), data=ferns2)
  Anova(ags_mod2)
  summary(ags_mod2)
  r.squaredGLMM(ags_mod2)
  # R2m       R2c 
  # 0.3021335 0.7076270
  #P=0.021
  
an_mod2 <- lmer(amass ~ nmass  + (1|species), data=ferns2)  
  Anova(an_mod2)
  summary(an_mod2)
  r.squaredGLMM(an_mod2)

ap_mod2 <- lmer(amass ~ pmass  + (1|species), data=ferns2)  
  Anova(ap_mod2)
  summary(ap_mod2)
  r.squaredGLMM(ap_mod2)  
  
gssd_mod2 <- lmer(gs ~ sto_dens  + (1|species), data=ferns2)
  Anova(gssd_mod2)
  summary(gssd_mod2)
  r.squaredGLMM(gssd_mod2)

gssl_mod2 <- lmer(gs ~ sto_length  + (1|species), data=ferns2)
  Anova(gssl_mod2)  
  
pnue_mod_fern <- lmer(nue ~ LMA + (1|species), data=ferns2)
  Anova(pnue_mod_fern)
  r.squaredGLMM(pnue_mod_fern)
  
  # R2m       R2c 
  # 0.4545291 0.5192158
  # P < 0.0001
ppue_mod_fern <- lmer(pue ~ LMA  + (1|species), data=pue_ferns)
  Anova(ppue_mod_fern)
  r.squaredGLMM(ppue_mod_fern)
  #R2m       R2c 
  #0.5819061 0.6511018 
  # P < 0.0001
  
 ##photo and lma
  plma_mod_sela2 <- lmer(amass ~ LMA + (1|species), data=ferns)
  Anova(plma_mod_sela2)
  r.squaredGLMM(plma_mod_sela2)
  