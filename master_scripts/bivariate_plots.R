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

library(scales)
ferncol<- alpha("royalblue", .85)
fernline <- "royalblue"

###models -----
open <- sela2[sela2$habitat == "full_sun",]
closed <- sela2[sela2$habitat == "understory_midlight",]
swamp <- sela2[sela2$habitat == "swamp_lowlight",]

#ferns
sdcond_mod_fern<- lm(gs ~ sto_dens ,data=ferns)
asatgs_mod_fern <- lm(asat ~ gs, data=ferns)
namass_mod_fern<- lm(amass ~ nmass ,data=ferns)
#sela-habitats
asatgs_mod1 <- sma(asat ~ gs , data=open)
asatgs_mod2 <- sma(asat ~ gs , data=closed)
asatgs_mod3 <- sma(asat ~ gs , data=swamp)

gssd_mod1 <- sma(gs ~ sto_dens , data=open)
gssd_mod2 <- sma(gs ~ sto_dens , data=closed)
gssd_mod3 <- sma(gs ~ sto_dens , data=swamp)

an_mod1 <- sma(amass ~ nmass, data=open)
an_mod2 <- sma(amass ~ nmass, data=closed)
an_mod3 <- sma(amass ~ nmass, data=swamp)

### large panel plots (l=sela, r=ferns)-----
# pdf("output/traits.pdf", width = 8, height = 12)
png(filename = "output/traits.png", width = 8, height = 10, units = "in", res= 400)

layout(matrix(c(1:6), 3, 2) )

par(las=1,  mgp=c(3,1,0), cex.lab=1.25)

par(mar=c(5,5,1,0))
plot(asat~ gs, data=sela2, col=trtcols2[habitat], pch=16, ylab=anetlab, xlab="", ylim=c(0,9),
     xlim=c(0,.25), cex=1.5)
mtext(side=1, text=condlab, line=3.5,at=.25)
text("A", x=0, y=9, cex=1.25)
title(expression(italic(underline(Selaginella))), line=-1)
legend("bottomright",col=trtcols,pch=16,legend=trtlab,inset=.01,  bty='n',cex=1.25)
ablineclip(asatgs_mod1, col=trtcols[1], lwd=2, x1=0.02719342, x2=0.12231778, lty=1)
ablineclip(asatgs_mod2, col=trtcols[2], lwd=2, x1=0.03366758, x2=0.16664840, lty=2)
ablineclip(asatgs_mod3, col=trtcols[3], lwd=2, x1=0.04869654, x2=0.08344741, lty=1)

par(mar=c(5,5,1,0))
plot(gs ~ sto_dens, col=trtcols2[habitat], pch=16, data=sela2, xlim=c(0, 125),
     ylim=c(0, 0.25), ylab=condlab, xlab="", cex=1.5)
mtext(side=1, text=denslab, line=3.5,at=125)
text("C", x=0, y=.25, cex=1.25)
ablineclip(gssd_mod1, col=trtcols[1], lwd=2, x1=73.04348, x2=134.32432, lty=1)
ablineclip(gssd_mod2, col=trtcols[2], lwd=2, x1=19.34783, x2=53.78378, lty=2)
ablineclip(gssd_mod3, col=trtcols[3], lwd=2, x1=52.17391, x2=75., lty=1)

par(mar=c(5,5,1,0))
plot(amass ~ nmass  ,col=trtcols2[habitat], pch=16, data=sela2,  ylim=c(0,1000), ylab=amasslab,
     xlim=c(0,55),xlab="", cex=1.5)
mtext(side=1, text=nmasslab, line=3.5,at=55)
text("E", x=0, y=1000, cex=1.25)
ablineclip(an_mod1, col=trtcols[1], lwd=2, x1=10.7, x2=20.9, lty=1)
ablineclip(an_mod2, col=trtcols[2], lwd=2, x1=23.9, x2=44.9, lty=2)
ablineclip(an_mod3, col=trtcols[3], lwd=2, x1=19.0, x2=28.0, lty=1)

par(mar=c(5,0,1,1))
plot(asat ~ gs, data=ferns, col=ferncol, pch=16,yaxt="n", xlab="", ylim=c(0, 9),
     xlim=c(0,.25), ylab="", cex=1.5)
ablineclip(asatgs_mod_fern, col=fernline, lwd=2, x1=0.02358104, x2=0.23880560, lty=2)
text("B", x=0, y=9, cex=1.25)
title(expression(underline(Ferns)),cex=1.25, line=-1)

par(mar=c(5,0,1,1))
plot(gs ~ sto_dens  ,col=ferncol, pch=16, data=ferns, xlim=c(0, 125),
     ylim=c(0, 0.25), yaxt='n', ylab="", xlab="", cex=1.5)
ablineclip(sdcond_mod_fern, col=fernline, lwd=2, x1=min(ferns$sto_dens), x2=39.57, lty=2)
text("D", x=0, y=.25, cex=1.25)

par(mar=c(5,0,1,1))
plot(amass ~ nmass  ,col=ferncol, pch=16, data=ferns,  ylim=c(0,1000), ylab="",
     xlim=c(0,55), xlab="",yaxt='n', cex=1.5)
ablineclip(namass_mod_fern, col=fernline, lwd=2, x1=24.2, x2=47.5, lty=2)
text("F", x=0, y=1000, cex=1.25)

# dev.copy2pdf(file= "output/traits.pdf")
dev.off()
