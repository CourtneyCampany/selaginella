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


##mass based
plot(amass~ nmass, data=sela2, col=trtcols2[habitat], pch=16, ylim=c(0, 1200), xlim=c(0, 50))


plot(amass~ LMA, data=sela2, col=trtcols2[habitat], pch=16, ylim=c(0, 1200), xlim=c(0, 18))
plot(amass~ LMA, data=ferns,  pch=16, ylim=c(0, 1200), xlim=c(10, 30))

plot(amass~ LMA, data=alldata, col=family, pch=16)

plot(pmass~ LMA, data=sela2, col=trtcols2[habitat], pch=16, ylim=c(0, 7), xlim=c(0, 18))


##area based
plot(asat~ N, data=sela2, col=trtcols2[habitat], pch=16, xlim=c(0,5), ylim=c(0,10))

windows()
par(mfrow=c(2,1))
plot(LMA~ habitat, data=sela2[sela2$species != "Sel_ate",])

plot(LMA~species, data=sela2)
