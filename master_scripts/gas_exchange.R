#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(lme4)

##i dont beleive we have light response curves for all ferns
##do not have chlorophyll for ferns

alldata <- read.csv("raw_data/master_data.csv")
alldata$sla <- with(alldata, 1/LMA)
alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)
alldata$pmass <- with(alldata, P * 10)
alldata$nue <- with(alldata, amass/nmass)
alldata$pue <- with(alldata, amass/pmass)

habitat <- read.csv("raw_data/treatments.csv")

alldata <- merge(alldata, habitat)

pue_dat <- alldata[alldata$pue < 400,]

#### nue/pue models with lma -----

#amass relationshsip  
lma_pnue_mod<- lm(nue~ LMA, data=alldata)
# summary(lma_pnue_mod)  
# anova(lma_pnue_mod)

lma_ppue_mod<- lm(pue~ LMA, data=pue_dat)
summary(lma_ppue_mod)  
# anova(lma_ppue_mod)

###mixed effect with lma & habitat
pnue_mod <- lmer(nue ~ LMA * habitat + (1|species), data=alldata)
  library(arm)
  library(car)
  Anova(pnue_mod)

ppue_mod <- lmer(pue ~ LMA * habitat + (1|species), data=pue_dat)
  Anova(ppue_mod)


###plotting ------
library(scales)
famcols2 <- alpha(familycols, .75)

png(filename = "output/lma_npue.png", width = 11, height = 8.5, units = "in", res= 400)

# windows(7,7)
par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), oma=c(6,6,1,1))

par(mar=c(0,0,0,0), xpd=TRUE)
plot(nue~ LMA, data=alldata, ylim=c(0, 36), xlim=c(0, 26), type='n', xaxt='n')
legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,  pch=16, 
       bty='n', inset=.01, cex=1, pt.cex=1.25)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(nue~ LMA, data=alldata, pch=16, col=famcols[family], cex=1.5)
axis(1, labels=FALSE, tcl=.5)
mtext(side=2, at=18, line=3,text=nuelab, xpd=TRUE, las=3, cex=1)
text('A', x=0, y=35, cex=1.25)

par(mar=c(0,0,0,0),xpd=TRUE )
plot(pue~ LMA, data=pue_dat, xlab=lmalab, ylab="", xlim=c(0, 26), ylim=c(0, 350),type='n')
predline(lma_ppue_mod, col="grey20",lwd=2, lty=2)
points(pue~ LMA, data=pue_dat, pch=16, col=famcols[family], cex=1.5)
mtext(side=2, at=210, line=3,text=puelab, xpd=TRUE, las=3, cex=1)
mtext(side=1, at=13, line=3,text=lmalab, xpd=TRUE, las=1, cex=1)
text('B', x=0, y=345, cex=1.25)

dev.off()
