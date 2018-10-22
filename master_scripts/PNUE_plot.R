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
  alldata$nue <- with(alldata, amass/nmass)


#split ferns and sela to add habitat
ferns <- alldata[alldata$family == "Ferns",]
sela <- alldata[alldata$family == "Selaginella",]  
  
habitat <- read.csv("raw_data/treatments.csv")

sela2 <- merge(sela, habitat)

#### nue/pue models with lma -----

#amass relationshsip  
lma_pnue_mod<- lm(nue~ LMA, data=alldata)
  # summary(lma_pnue_mod)
  # anova(lma_pnue_mod)

##plotbits

nuecols <- c("white", "black")
library(scales)
nuecols2 <- c(alpha(nuecols[1], .85), alpha(nuecols[2], .85))
  
###plotting ------

# png(filename = "output/lma_pnue.png", width = 11, height = 8.5, units = "in", res= 400)


jpeg(filename = "output/manuscript_figures/Figure_5.jpeg",
     width = 8.4, height =8.4, units = "in", res= 300)

# setEPS()
# postscript("output/manuscript_figures/Figure_5.eps")

# windows(7,7)
par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(5,5,1,1))

plot(nue~ LMA, data=alldata, ylim=c(0, 32), xlim=c(0, 26),
     xlab=lmalab, ylab=nuelab2, type='n')
legend("topright", legend=c("Ferns", "Selaginella"), pt.bg=nuecols,  pch=21, 
       bty='n', inset=.01, cex=1.25, pt.cex=1.25)
predline2(lma_pnue_mod, col="grey20",lwd=2, lty=2)
#dont use transparnecy for .eps (nuecols2)
points(nue~ LMA, data= ferns, pch=21,bg=nuecols[1], cex=1.5)
points(nue~ LMA, data=sela, pch=21, bg=nuecols[2], cex=1.5)

text(2, 5, expression(paste(R[cond]^{"2"}," = "," 0.55")))
text(2, 3, expression(paste(R[marg]^{"2"}," = "," 0.81")))
text(2, 1, expression(paste(italic(P)," < "," 0.001")))

dev.off()
