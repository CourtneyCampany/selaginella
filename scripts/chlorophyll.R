#chlorophyll extractions

chl <- read.csv("raw_data/chlorophyll.csv")
  #chlorophyll concentrations (mg L-1)
  chl$chlA <- with(chl, 12.7*a664nm - 2.79*a647nm )
  chl$chlB <- with(chl, 20.7*a647nm - 4.62*a664nm )
  chl$chl_tot <- with(chl, 17.9*a647nm + 8.08*a664nm )
  #mass basis chlorophyll content
  chl$chlA_mass <- with(chl, chlA / (mass_g)) #mg chlA / mg leaf
  chl$chlB_mass <- with(chl, chlB / (mass_g*1000)) #mg chlB / mg leaf
  chl$chl_tot_mass <- with(chl, chl_tot / (mass_g*1000))
  #ratio A/b
  chl$chl_AB <- with(chl, chlA_mass / chlB_mass)

habitat <- read.csv("raw_data/treatments.csv")



chl_sela <- merge(chl, habitat)
##there is a bad data point for Sel_art #4-1

chl_clean <- chl_sela[chl_sela$species != "Sel_art" | chl_sela$individual != 4 |
                        chl_sela$sample != 1, ]


library(doBy)
chl_sela_means <- summaryBy(chlA + chlB + chl_tot + chlA_mass + chlB_mass + 
                          chl_AB + chl_tot_mass ~
                          species + individual + habitat, FUN=mean, data=chl_clean,
                          keep.names = TRUE)

#relationship with N and use SLA if needed
alldata <- read.csv("raw_data/master_data.csv")
sela_sla <- alldata[alldata$family == "Selaginella", c(2:3,7,10)]

chl_sela_agg <- merge(chl_sela_means, sela_sla)


plot(chl_tot_mass ~ N, data=chl_sela_agg, col=habitat)
#chl to N ratio?


##basic plotting
boxplot(chlA ~ habitat, data=chl_sela_agg)
boxplot(chlB ~ habitat, data=chl_sela_agg)
boxplot(chlA/chlB ~ habitat, data=chl_sela_agg)

boxplot(chlA_mass ~ habitat, data=chl_sela_agg)
boxplot(chlB_mass ~ habitat, data=chl_sela_agg)
boxplot(chl_AB ~ habitat, data=chl_sela_agg)

boxplot(chl_tot_mass ~ habitat, data=chl_sela_agg)
#stats : is chl differemt by habitat?

source("functions.R")
#selaginella stats
library(visreg)
library(multcomp)
library(car)
library(lme4)


###chlorophyll A ------
chla_mod <- lm(chlA_mass ~ species, data=chl_sela_agg)
visreg(chla_mod)
residualPlot(chla_mod)
qqPlot(chla_mod)
summary(chla_mod)
anova(chla_mod)
#.002 by species F= 4.6874

tukey_chla <- glht(chla_mod, linfct = mcp(species = "Tukey"))
chla_siglets <-cld(tukey_chla)
chla_siglets2 <- chla_siglets$mcletters$Letters

# Sel_anc  Sel_art  Sel_ate  Sel_eur  Sel_oax Sel_swim  Sel_umb 
# "ab"     "ab"     "ab"      "a"      "b"     "ab"      "a" 

min(chl_sela_agg$chlA_mass)
max(chl_sela_agg$chlA_mass)

#habitats (yes)
chla_mod2 <- lm(chlA_mass ~ habitat, data=chl_sela_agg)
summary(chla_mod2)
anova(chla_mod2)
visreg(chla_mod2)
#p=.0097

tukey_chla2 <- glht(chla_mod2, linfct = mcp(habitat = "Tukey"))
chla2_siglets <-cld(tukey_chla2)
chla2_siglets2 <- chla2_siglets$mcletters$Letters

#full_sun      swamp_lowlight understory_midlight 
#"a"                 "b"                "ab" 

##chlA increases with shade swamp > fullsun


###chlorophyll b ------
chlb_mod <- lm(chlB_mass ~ species, data=chl_sela_agg)
visreg(chlb_mod)
residualPlot(chlb_mod)
qqPlot(chlb_mod)
summary(chlb_mod)
anova(chlb_mod)
#.014 by species F= 3.2786

tukey_chlb <- glht(chlb_mod, linfct = mcp(species = "Tukey"))
chlb_siglets <-cld(tukey_chlb)
chlb_siglets2 <- chlb_siglets$mcletters$Letters

min(chl_sela_agg$chlB_mass)
max(chl_sela_agg$chlB_mass)

#habitats (yes)
chlb_mod2 <- lm(chlB_mass ~ habitat, data=chl_sela_agg)
summary(chlb_mod2)
anova(chlb_mod2)
visreg(chlb_mod2)
#p=.01965 , F = 4.454

tukey_chlb2 <- glht(chlb_mod2, linfct = mcp(habitat = "Tukey"))
chlb2_siglets <-cld(tukey_chlb2)
chlb2_siglets2 <- chlb2_siglets$mcletters$Letters

#full_sun      swamp_lowlight understory_midlight 
#"a"                 "b"                "ab" 

##chlB also increases with shade swamp > fullsun

## ratio of AB
chlab_mod <- lm(chl_AB ~ species, data=chl_sela_agg)
visreg(chlab_mod)
residualPlot(chlab_mod)
qqPlot(chlab_mod)
summary(chlab_mod)
anova(chlab_mod)
#<.001 by species F= 8.0768
#only driven by Sel art === inspect data
tukey_chlab <- glht(chlab_mod, linfct = mcp(species = "Tukey"))
chlab_siglets <-cld(tukey_chlab)
chlab_siglets2 <- chlab_siglets$mcletters$Letters

min(chl_sela_agg$chl_AB)
max(chl_sela_agg$chl_AB)

#habitats (yes)
chlab_mod2 <- lm(chl_AB ~ habitat, data=chl_sela_agg)
summary(chlab_mod2)
anova(chlab_mod2)
visreg(chlab_mod2)
##not different

# total chl -------

#habitats (yes)
chltot_mod <- lm(chl_tot_mass ~ habitat, data=chl_sela_agg)
summary(chltot_mod)
anova(chltot_mod)
visreg(chltot_mod)
#p=.0055 , F = 6.1249

tukey_chltot <- glht(chltot_mod, linfct = mcp(habitat = "Tukey"))
chltot_siglets <-cld(tukey_chltot)
chltot2_siglets2 <- chltot_siglets$mcletters$Letters
#full_sun      swamp_lowlight understory_midlight 
#"a"                 "b"                "ab" 