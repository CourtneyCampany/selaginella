#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(smatr)
library(emmeans)

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

palette(rainbow(3)) 

#a vs gs--------
windows()
plot(asat ~ gs, data=sela2, col=habitat, pch=16)
legend("topleft",col=palette(),pch=16,legend=unique(sela2$habitat), bty='n')

asatgs_mod <- lm(asat ~ gs, data=sela)
summary(asatgs_mod)
#r2 = 0.78, p=0.001

#slopes of habitat
asatgs_mod2 <- sma(asat ~ gs * habitat, data=sela2)
summary(asatgs_mod2)
windows()
plot(asatgs_mod2)
plot(asatgs_mod2, which='residual') 
plot(asatgs_mod2, which='qq')

asatgs_mod3 <- lm(asat ~ gs, data=ferns)
summary(asatgs_mod3)

#### amass vs lma--------
plot(amass ~ LMA, data=sela2, col=habitat, pch=16)
amasslma_mod <- lm(amass ~ LMA, data=sela2)
summary(amasslma_mod)
#r2 = 29, p<0.001

#slopes of habitat
amasslma_mod2 <- sma(amass ~ LMA * habitat, data=sela2)
summary(amasslma_mod2)
windows()
plot(amasslma_mod2)
plot(amasslma_mod2, which='residual') 
plot(amasslma_mod2, which='qq')

amasslma_mod3 <- lm(amass ~ LMA, data=ferns)
summary(amasslma_mod3)
plot(amass ~ LMA, data=ferns, col=species, pch=16)


#gs/an vs stomata ------
plot(gs ~ sto_dens, data=alldata, col=family, pch=16)
plot(gs~ sto_dens, data=sela, col=species, pch=16)

#sela
sd_cond_mod<- lm(gs ~ sto_dens ,data=sela)
summary(sd_cond_mod)

sl_cond_mod<- lm(gs ~ sto_length ,data=sela)
summary(sl_cond_mod)

sd_an_mod<- lm(asat ~ sto_dens ,data=sela)
summary(sd_an_mod)

sl_an_mod<- lm(asat ~ sto_length ,data=sela)
summary(sl_an_mod)

#ferns
sd_cond_mod2<- lm(gs ~ sto_dens ,data=ferns)
summary(sd_cond_mod2)
plot(gs ~ sto_dens ,data=ferns)
abline(sd_cond_mod2)

sl_cond_mod2<- lm(gs ~ sto_length ,data=ferns)
summary(sl_cond_mod2)
plot(gs ~ sto_length ,data=ferns)
abline(sl_cond_mod2)

sd_an_mod2<- lm(asat ~ sto_dens ,data=ferns)
summary(sd_an_mod2)
anova(sd_an_mod2)

sl_an_mod2<- lm(asat ~ sto_length ,data=ferns)
summary(sl_an_mod2)


#Water use Efficiency-----

#ITE vs N-----
plot(ITE ~ N, data=alldata, col=family, pch=16)

itenitro_mod <- lm(ITE ~ N, data=sela2) #mass based
summary(itenitro_mod)
plot(ITE ~ N, data=sela2, col=habitat, pch=16)

itenitro_mod2 <- lm(ITE ~ N, data=ferns)
summary(itenitro_mod2)
plot(ITE ~ N, data=ferns,  pch=16)
#not realted to either

itenitro_mod3 <- lm(ITE ~ narea, data=sela2) #mass based
summary(itenitro_mod3)
plot(ITE ~ narea, data=sela2, col=habitat, pch=16)
#postively related to Narea

itenitro_mod4 <- lm(ITE ~ narea, data=ferns)
summary(itenitro_mod4)
plot(ITE ~ narea, data=ferns,  pch=16)
##not realted

plot(ITE ~ nue, data=alldata, col=family, pch=16)
itenue_mod <- lm(ITE ~ nue, data=sela2)
summary(itenue_mod)

itenue_mod2 <- lm(ITE ~ nue, data=ferns)
summary(itenue_mod2)

plot(ITE ~ pue, data=alldata, col=family, pch=16)
itenue_mod <- lm(ITE ~ pue, data=sela2)
summary(itenue_mod)

itepue_mod2 <- lm(ITE ~ pue, data=ferns)
summary(itepue_mod2)


###chlorophyll----- ?HUH?---leave out-----
plot(chlorophyll ~ N, data=sela2, col=habitat)
  chlnitro_mod <- lm(chlorophyll ~ N, data=sela2)
  summary(chlnitro_mod)
  #not significant
  plot(asat ~ chlorophyll, data=sela2, col=habitat)
  chlamass_mod <- lm(asat ~ chlorophyll, data=sela2)
  summary(chlamass_mod)
  
  
#photo vs n/p (add CI and look at habitat relationships)---need area basis???-----
  
plot(asat ~ narea, data=alldata, col=family, pch=16) 
plot(amass ~ nmass, data=alldata, col=family, pch=16) 
  
plot(asat ~ parea, data=alldata, col=family, pch=16) 
plot(amass ~ pmass, data=alldata, col=family, pch=16) 

Nphoto_mod<- lm(asat ~ narea ,data=sela2)
summary(Nphoto_mod) 
#yes
Photo_mod<- lm(asat ~ parea ,data=sela2)
summary(Photo_mod) 
#yes

Nphoto_mod2<- lm(amass ~ nmass ,data=sela2)
summary(Nphoto_mod2) 
#yes
Photo_mod2<- lm(amass ~ pmass ,data=sela2)
summary(Photo_mod2) 

Nphoto_mod3<- lm(amass ~ nmass ,data=ferns)
summary(Nphoto_mod3) 
#yes
Photo_mod3<- lm(amass ~ pmass ,data=ferns)
summary(Photo_mod3) 

Nphoto_mod4<- lm(asat ~ narea ,data=ferns)
summary(Nphoto_mod3) 
#yes
Photo_mod4<- lm(asat ~ parea ,data=ferns)
summary(Photo_mod4)

#across habitas
Nphoto_mod5 <- sma(amass ~ nmass * habitat, data=sela2)
summary(Nphoto_mod5)
windows()
plot(Nphoto_mod5)

Pphoto_mod5 <- sma(amass ~ pmass * habitat, data=sela2)
summary(Pphoto_mod5)
windows()
plot(Pphoto_mod5)

#Resource use effiency and LMA-------
plot(nue~LMA, data=alldata)
boxplot(pue~habitat, sela2) ###remove one outlier
sela3 <- sela2[sela2$pue <400,]

lma_nue_mod<- lm(nue~ LMA, data=sela2)
summary(lma_nue_mod) 

lma_pue_mod<- lm(pue~ LMA, data=sela3)
summary(lma_pue_mod) 

lma_nue_mod2<- lm(nue~ LMA, data=ferns)
summary(lma_nue_mod2) 

lma_pue_mod2<- lm(pue~ LMA, data=ferns)
summary(lma_pue_mod2) 

#across habitas
lma_nue_mod3 <- sma(nue~ LMA * habitat, data=sela2)
summary(lma_nue_mod3)
plot(lma_nue_mod3)

lma_pue_mod3 <- sma(pue~ LMA * habitat, data=sela3)
summary(lma_pue_mod3)
plot(lma_pue_mod3)
