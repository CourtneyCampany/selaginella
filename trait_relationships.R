#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)

##i dont beleive we have light response curves for all ferns
##do not have chlorophyll for ferns

alldata <- read.csv("raw_data/master_data.csv")
  alldata$sla <- with(alldata, 1/LMA)
  alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
  alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)
  alldata$pmass <- with(alldata, P * 10)
  alldata$nue <- with(alldata, amass/nmass)
  alldata$pue <- with(alldata, amass/pmass)
  
sela <- alldata[alldata$family=="Selaginella",]

#chl-N
plot(chlorophyll ~ N, data=sela, col=species, pch=16,ylim=c(0, 25), xlim=c(0,5))
  chlnitro_mod <- lm(chlorophyll ~ N, data=sela)
  summary(chlnitro_mod)
  #not significant
  

#ITE vs N
plot(ITE ~ N, data=alldata, col=family, pch=16)
plot(ITE ~ N, data=sela, col=species, pch=16)
  itenitro_mod <- lm(ITE ~ N, data=alldata)
  summary(itenitro_mod)
  #not significant
  
#gs/an vs stomata
plot(gs ~ sto_dens, data=alldata, col=family, pch=16)
plot(gs~ sto_dens, data=sela, col=species, pch=16)
  stomcond_mod<- lm(gs ~ sto_dens ,data=alldata)
  summary(stomcond_mod)

plot(asat~ sto_dens, data=alldata, col=family, pch=16) 
plot(asat~ sto_dens, data=sela, col=species, pch=16) 
  stomphoto_mod<- lm(asat ~ sto_dens ,data=alldata)
  summary(stomphoto_mod)  

#Physiology not related to stomatal anatomy and 
  
#photo vs n/p (add CI and look at habitat relationships)---need area basis???
plot(asat~ N, data=alldata, col=family, pch=16) 
plot(asat~ N, data=sela, col=species, pch=16) 
  Nphoto_mod<- lm(asat ~ N ,data=sela)
  summary(Nphoto_mod) 
  
  
plot(asat~ P, data=alldata, col=family, pch=16) 
plot(asat~ P, data=sela, col=species, pch=16) 
  Pphoto_mod<- lm(asat ~ P ,data=sela)
  summary(Pphoto_mod) 
  
  

plot(ITE~nue, data=alldata, col=family, pch=16) 
#not related...is this important to mention.
