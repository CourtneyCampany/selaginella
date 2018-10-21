#chlorophyll extractions

chl <- read.csv("raw_data/chlorophyll.csv")
  #chlorophyll concentrations (mg L-1)
  chl$chlA <- with(chl, 12.7*a664nm - 2.79*a647nm )
  chl$chlB <- with(chl, 20.7*a647nm - 4.62*a664nm )
  chl$chl_tot <- with(chl, 17.9*a647nm + 8.08*a664nm)
  
extraction <- 3 #ml  
  
  #mass basis chlorophyll content (mg chl / g leaf FW)
  chl$chlA_mass <- with(chl, chlA * extraction / (1000 * mass_g))
  chl$chlB_mass <-  with(chl, chlB * extraction / (1000 * mass_g))
  chl$chl_tot_mass <- with(chl, chl_tot * extraction / (1000 * mass_g))
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

# write.csv(chl_sela_means, "calculated_data/chl_species.csv", row.names=FALSE)

# #relationship with N and use SLA if needed
# alldata <- read.csv("raw_data/master_data.csv")
# sela_sla <- alldata[alldata$family == "Selaginella", c(2:3,7,10)]
# 
# chl_sela_agg <- merge(chl_sela_means, sela_sla)
# 
# plot(chl_tot_mass ~ N, data=chl_sela_agg, col=habitat)
# #chl to N ratio?

#chlorophyll means and se for paper
library(plotrix)
chl_means <- summaryBy(chlA_mass + chlB_mass + chl_AB + chl_tot_mass ~ species, 
                       FUN=c(mean,std.error), data=chl_clean)
write.csv(chl_means, "calculated_data/chlorophyll_means.csv", row.names = FALSE)
#species comparisons:
max(chl_means$chl_tot_mass.mean)
min(chl_means$chl_tot_mass.mean)

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
#.002034 by species F= 4.6874
#r2=.3942

tukey_chla <- glht(chla_mod, linfct = mcp(species = "Tukey"))
chla_siglets <-cld(tukey_chla)
chla_siglets2 <- chla_siglets$mcletters$Letters

tukeys_chla <- HSD.test(chla_mod, "species", group=TRUE)

# Sel_anc  Sel_art  Sel_ate  Sel_eur  Sel_oax Sel_swim  Sel_umb 
# "ab"     "ab"     "ab"      "a"      "b"     "ab"      "a" 

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
#r2=2868

tukey_chlb <- glht(chlb_mod3, linfct = mcp(species = "Tukey"))
chlb_siglets <-cld(tukey_chlb)
chlb_siglets2 <- chlb_siglets$mcletters$Letters

library(agricolae)
comparison <- HSD.test(chlb_mod, "species", group=TRUE)


#outlier removed
boxplot(chlB ~ species, data=chl_sela_agg)
chlb <- chl_sela_agg[chl_sela_agg$species != "Sel_oax" | 
                       chl_sela_agg$individual != 2, ]

chlb_mod3 <- lm(chlB_mass ~ species, data=chlb)
visreg(chlb_mod3)
residualPlot(chlb_mod3)
qqPlot(chlb_mod3)
summary(chlb_mod3)
anova(chlb_mod3)

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

## ratio of AB-------
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


chltot_mod2 <- lm(chl_tot_mass ~ species, data=chl_sela_agg)
visreg(chltot_mod2)
residualPlot(chltot_mod2)
qqPlot(chltot_mod2)
summary(chltot_mod2)
anova(chltot_mod2)
#p=.010 by species F= 3.571
#r2=.31

tukey_chla <- glht(chla_mod, linfct = mcp(species = "Tukey"))
chla_siglets <-cld(tukey_chla)
chla_siglets2 <- chla_siglets$mcletters$Letters

max(chl_sela_agg$chl_tot_mass)
min(chl_sela_agg$chl_tot_mass)
