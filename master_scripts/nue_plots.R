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
  
#remove obvous outliers
pue_dat <- alldata[alldata$pue < 400,]
pue_dat <- pue_dat[complete.cases(pue_dat$family),]

#split ferns and sela to add habitat
ferns <- pue_dat[pue_dat$family == "Ferns",]
sela <- pue_dat[pue_dat$family == "Selaginella",]  
  
habitat <- read.csv("raw_data/treatments.csv")

sela2 <- merge(sela, habitat)

#### nue/pue models with lma -----

#amass relationshsip  
lma_pnue_mod<- lm(nue~ LMA, data=alldata)
  # summary(lma_pnue_mod)
  # anova(lma_pnue_mod)

lma_ppue_mod<- lm(pue~ LMA, data=pue_dat)
  # summary(lma_ppue_mod)
  # anova(lma_ppue_mod)

###mixed effect with lma & habitat
library(arm)
library(car)
library(MuMin)
  
pnue_mod <- lmer(nue ~ LMA + (1|species), data=alldata)
  Anova(pnue_mod)
  r.squaredGLMM(pnue_mod)

  # R2m       R2c 
  # 0.5509523 0.8086354 
  # P < 0.0001
ppue_mod <- lmer(pue ~ LMA  + (1|species), data=pue_dat)
  Anova(ppue_mod)
  r.squaredGLMM(ppue_mod)

  # R2m       R2c 
  # 0.6648742 0.6648742 
  # P < 0.0001
  
##plotbits

nuecols <- c("white", "black")
library(scales)
nuecols2 <- c(alpha(nuecols[1], .85), alpha(nuecols[2], .85))
  
###plotting ------

png(filename = "output/lma_npue.png", width = 11, height = 8.5, units = "in", res= 400)

# windows(7,7)
par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), oma=c(6,6,1,1))

par(mar=c(0,0,0,0), xpd=TRUE)
plot(nue~ LMA, data=alldata, ylim=c(0, 36), xlim=c(0, 26), type='n', xaxt='n')
legend("topright", legend=c("Ferns", "Selaginella"), pt.bg=nuecols,  pch=21, 
       bty='n', inset=.01, cex=1.25, pt.cex=1.25)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(nue~ LMA, data= ferns, pch=21,bg=nuecols2[1], cex=1.5)
points(nue~ LMA, data=sela, pch=21, bg=nuecols2[2], cex=1.5)
axis(1, labels=FALSE, tcl=.5)
mtext(side=2, at=18, line=3,text=nuelab, xpd=TRUE, las=3, cex=1)
text('A', x=0, y=35, cex=1.25)

text(1, 6, expression(paste(R[cond]^{"2"}," = "," 0.55")))
text(1, 1.5, expression(paste(R[marg]^{"2"}," = "," 0.81")))

par(mar=c(0,0,0,0),xpd=TRUE )
plot(pue~ LMA, data=pue_dat, xlab=lmalab, ylab="", xlim=c(0, 26), ylim=c(0, 350),type='n')
predline(lma_ppue_mod, col="grey20",lwd=2, lty=2)
points(pue~ LMA, data= ferns, pch=21,bg=nuecols2[1], cex=1.5)
points(pue~ LMA, data=sela, pch=21, bg=nuecols2[2], cex=1.5)
mtext(side=2, at=210, line=3,text=puelab, xpd=TRUE, las=3, cex=1)
mtext(side=1, at=13, line=3,text=lmalab, xpd=TRUE, las=1, cex=1)
text('B', x=0, y=345, cex=1.25)

text(1, 60, expression(paste(R[cond]^{"2"}," = "," 0.66")))
text(1, 15, expression(paste(R[marg]^{"2"}," = "," 0.66")))

dev.off()
