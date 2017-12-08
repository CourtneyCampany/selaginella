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


#amass relationshsip  
lma_pnue_mod<- lm(nue~ LMA, data=alldata)
summary(lma_pnue_mod)  
anova(lma_pnue_mod)

# plot(amass~ nmass, data=alldata, col=family, pch=16)
# plot(amass~ pmass, data=alldata, col=species, pch=16)

# windows(7,7)
png(filename = "output/lma_nue.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2), las=1,cex.axis=0.8)
plot(nue~ LMA, data=alldata, xlab=lmalab, ylab=nuelab,ylim=c(0, 36), xlim=c(0, 26), type='n')
legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,  pch=16, bty='n', inset=.01)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(nue~ LMA, data=alldata, pch=16, col=familycols[family], cex=1.5)
dev.off()

#can add pue if we want
# plot(pue~ LMA, data=alldata, col=family, pch=16)
lma_ppue_mod<- lm(pue~ LMA, data=alldata)
summary(lma_pnue_mod)  
anova(lma_pnue_mod)

# par(mar=c(5,5,2,2), las=1,cex.axis=0.8)
# plot(pue~ LMA, data=alldata, xlab=lmalab, ylab="PPUE", xlim=c(0, 26), type='n')
# legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,  pch=16, bty='n', inset=.01)
# predline(lma_ppue_mod, col="grey20",lwd=2, lty=2)
# points(pue~ LMA, data=alldata, pch=16, col=familycols[family], cex=1.5)

##could test if the slopes are different between groups, could also look at slopes between habitats
