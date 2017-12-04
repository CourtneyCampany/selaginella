source("scripts/functions.R")
#selaginella stats
library(visreg)
library(multcomp)
library(car)

#pca with selaginella
sela <- read.csv("raw_data/sela_raw.csv")
sela$species <- factor(sela$species, 
              levels=c("Sel_anc","Sel_art","Sel_ate","Sel_eur","Sel_oax","Sel_swim","Sel_umb"))

habitat <- read.csv("raw_data/treatments.csv")

sela <- merge(sela, habitat)
#run annova for each parameter 

sela_agg <- doBy::summaryBy(.~ species, data=sela, FUN=m2, keep.names = TRUE)


# photosynthesis ----------------------------------------------------------
asat_mod <- lm(asat ~ species, data=sela)

visreg(asat_mod)
# residualPlot(asat_mod)
# qqPlot(asat_mod)

summary(asat_mod)
anova(asat_mod)

tukey_asat <- glht(asat_mod, linfct = mcp(species = "Tukey"))
asat_siglets <-cld(tukey_asat)
asat_siglets2 <- asat_siglets$mcletters$Letters

#habitats
asat_mod2 <- lm(asat ~ habitat, data=sela)
summary(asat_mod2)
anova(asat_mod2)

min(sela_agg$asat)
max(sela_agg$asat)

# conductance ----------------------------------------------------------
gs_mod <- lm(gs~ species, data=sela)

visreg(gs_mod)
# residualPlot(gs_mod)
# qqPlot(gs_mod)

summary(gs_mod)
anova(gs_mod)

tukey_gs <- glht(gs_mod, linfct = mcp(species = "Tukey"))
gs_siglets <-cld(tukey_gs)
gs_siglets2 <- gs_siglets$mcletters$Letters

#habitats
gs_mod2 <- lm(gs ~ habitat, data=sela)
summary(gs_mod2)
anova(gs_mod2)

min(sela_agg$gs)
max(sela_agg$gs)

# chlorophyll ----------------------------------------------------------
chl_mod <- lm(chlorophyll ~ species, data=sela)

visreg(chl_mod)
# residualPlot(chl_mod)
# qqPlot(chl_mod)

summary(chl_mod)
anova(chl_mod)

tukey_chl <- glht(chl_mod, linfct = mcp(species = "Tukey"))
chl_siglets <-cld(tukey_chl)
chl_siglets2 <- chl_siglets$mcletters$Letters

#habitats
chl_mod2 <- lm(chlorophyll ~ habitat, data=sela)
summary(chl_mod2)
anova(chl_mod2)
visreg(chl_mod2)
min(sela_agg$chlorophyll)
max(sela_agg$chlorophyll)

# LMA ----------------------------------------------------------
LMA_mod <- lm(LMA ~ species, data=sela)

visreg(LMA_mod)
residualPlot(LMA_mod)
qqPlot(LMA_mod)

summary(LMA_mod)
anova(LMA_mod)

tukey_lma <- glht(LMA_mod, linfct = mcp(species = "Tukey"))
lma_siglets <-cld(tukey_lma)
lma_siglets2 <- lma_siglets$mcletters$Letters

#habitats
lma_mod2 <- lm(LMA ~ habitat, data=sela)
summary(lma_mod2)
anova(lma_mod2)
visreg(lma_mod2)

min(sela_agg$LMA)
max(sela_agg$LMA)

# N ----------------------------------------------------------
n_mod <- lm(N ~ species, data=sela)

visreg(n_mod)
# residualPlot(n_mod)
# qqPlot(n_mod)

summary(n_mod)
anova(n_mod)

tukey_n <- glht(n_mod, linfct = mcp(species = "Tukey"))
n_siglets <-cld(tukey_n)
n_siglets2 <- n_siglets$mcletters$Letters

#habitats
N_mod2 <- lm(N ~ habitat, data=sela)
summary(N_mod2)
anova(N_mod2)
visreg(N_mod2)

min(sela_agg$N)
max(sela_agg$N)

# P ----------------------------------------------------------
p_mod <- lm(P ~ species, data=sela)

visreg(p_mod)
residualPlot(p_mod)
qqPlot(p_mod)

summary(p_mod)
anova(p_mod)

tukey_p <- glht(p_mod, linfct = mcp(species = "Tukey"))
p_siglets <-cld(tukey_lma)
p_siglets2 <- p_siglets$mcletters$Letters

p_mod2 <- lm(P ~ habitat, data=sela)
summary(p_mod2)
anova(p_mod2)
visreg(p_mod2)

min(sela_agg$P)
max(sela_agg$P)

# sto_dens ----------------------------------------------------------
sd_mod <- lm(sto_dens ~ species, data=sela)

visreg(sd_mod)
# residualPlot(sd_mod)
# qqPlot(sd_mod)

summary(sd_mod)
anova(sd_mod)

tukey_sd <- glht(sd_mod, linfct = mcp(species = "Tukey"))
sd_siglets <-cld(tukey_sd)
sd_siglets2 <- sd_siglets$mcletters$Letters

#habitats
sd_mod2 <- lm(sto_dens ~ habitat, data=sela)
summary(sd_mod2)
anova(sd_mod2)
visreg(sd_mod2)

min(sela_agg$sto_dens)
max(sela_agg$sto_dens)

# sto_length ----------------------------------------------------------
sl_mod <- lm(sto_length ~ species, data=sela)

visreg(sl_mod)
# residualPlot(sl_mod)
# qqPlot(sl_mod)

summary(sl_mod)
anova(sl_mod)

tukey_sl <- glht(sl_mod, linfct = mcp(species = "Tukey"))
sl_siglets <-cld(tukey_sl)
sl_siglets2 <- sl_siglets$mcletters$Letters

#habitats
sl_mod2 <- lm(sto_length ~ habitat, data=sela)
summary(sl_mod2)
anova(sl_mod2)
visreg(sl_mod2)

min(sela_agg$sto_length)
max(sela_agg$sto_length)


# ITE ----------------------------------------------------------
ITE_mod <- lm(ITE ~ species, data=sela)
summary(ITE_mod)
visreg(ITE_mod)

anova(ITE_mod)

tukey_ite <- glht(ITE_mod, linfct = mcp(species = "Tukey"))
ite_siglets <-cld(tukey_ite)
ite_siglets2 <- ite_siglets$mcletters$Letters

#habitats
ite_mod2 <- lm(ITE ~ habitat, data=sela)
summary(ite_mod2)
anova(ite_mod2)

min(sela_agg$ITE)
max(sela_agg$ITE)

# C.N ----------------------------------------------------------
cn_mod <- lm(C.N ~ species, data=sela)

visreg(cn_mod)
residualPlot(cn_mod)
qqPlot(cn_mod)

summary(cn_mod)
anova(cn_mod)

tukey_cn <- glht(cn_mod, linfct = mcp(species = "Tukey"))
cn_siglets <-cld(tukey_cn)
cn_siglets2 <- cn_siglets$mcletters$Letters

cn_mod2 <- lm(C.N ~ habitat, data=sela)
summary(cn_mod2)
anova(cn_mod2)
visreg(cn_mod2)


# lcp ----------------------------------------------------------
lcp_mod <- lm(lcp ~ species, data=sela)
summary(lcp_mod)
visreg(lcp_mod)

anova(lcp_mod)

tukey_lcp<- glht(lcp_mod, linfct = mcp(species = "Tukey"))
lcp_siglets <-cld(tukey_lcp)
lcp_siglets2 <- lcp_siglets$mcletters$Letters


lcp_mod <- lm(lcp ~ species, data=sela)

visreg(lcp_mod)
residualPlot(lcp_mod)
qqPlot(lcp_mod)

summary(lcp_mod)
anova(lcp_mod)

tukey_cn <- glht(cn_mod, linfct = mcp(species = "Tukey"))
cn_siglets <-cld(tukey_cn)
cn_siglets2 <- cn_siglets$mcletters$Letters

#habitats
lcp_mod2 <- lm(lcp ~ habitat, data=sela)
summary(lcp_mod2)
anova(lcp_mod2)
visreg(lcp_mod2)

min(sela_agg$lcp)
max(sela_agg$lcp)
