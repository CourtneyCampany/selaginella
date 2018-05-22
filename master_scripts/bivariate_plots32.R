#bivariate relationships between traits
source("functions.R")
source("master_scripts/plot_objects.R")
#Fern vs Sela stats
library(visreg)
library(multcomp)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(arm)
library(car)
library(plotrix)

alldata <- read.csv("raw_data/master_data.csv")
alldata$sla <- with(alldata, 1/LMA)
alldata$amass <- with(alldata, (asat*1000) * sla) ####nmols CO2 g s
alldata$nmass <- with(alldata, N*10) #mg g-1 (g g-1 = .01 (1%) and 1000)

ferns <- alldata[alldata$family=="Ferns",] 
ferns2 <- ferns[complete.cases(ferns$asat),]

sela <- alldata[alldata$family=="Selaginella",]
hab <- read.csv("raw_data/treatments.csv")
sela2 <- merge(sela, hab)

###models ------
ags_mod <- lmer(asat ~ gs * habitat + (1|species), data=sela2)
ags_mod_simp <- lm(asat ~ gs, data=sela2)

an_mod <- lmer(amass ~ nmass * habitat + (1|species), data=sela2)
an_mod_simp <- lm(amass ~ nmass, data=sela2)

#ferns
asatgs_mod_fern <- lm(asat ~ gs, data=ferns)
namass_mod_fern<- lm(amass ~ nmass ,data=ferns)

## plot bits------
library(scales)
ferncol<- alpha("royalblue", .85)
fernline <- "royalblue"

##ploting-------
windows()

layout(matrix(c(1:4), nrow=2, ncol=2) )
par(las=1,  mgp=c(3,1,0), cex.lab=1.25)

#photo vs gs
par(mar=c(5,5,1,0))
plot(asat ~ gs, data=sela2, ylab=anetlab, xlab="", ylim=c(0,9),
     xlim=c(0,.27), type='n')
predline(ags_mod_simp, col="grey20",lwd=2, lty=2)
points(asat ~ gs, data=sela2, col=trtcols2[habitat], pch=16, cex=1.5)
legend("bottomright",col=trtcols,pch=16,legend=trtlab,inset=.01,  bty='n',
       cex=1)
text("A", x=0, y=9, cex=1.25)
title(expression(italic(underline(Selaginella))), line=-1)
mtext(side=1, text=condlab, line=3.5,at=.25)

#amass v nmass
par(mar=c(5,5,1,0))
plot(amass ~ nmass, data=sela2, ylim=c(0,1000), ylab=amasslab,
     xlim=c(0,55), xlab="")
predline(an_mod_simp, col="grey20",lwd=2, lty=2)
points(amass ~ nmass, data=sela2, col=trtcols2[habitat], pch=16, cex=1.5)
text("C", x=0, y=1000, cex=1.25)
mtext(side=1, text=nmasslab, line=3.5,at=55)

#fern: a gs
par(mar=c(5,0,1,5))
plot(asat ~ gs, data=ferns, yaxt="n", xlab="", ylim=c(0, 9),
     xlim=c(0,.27), ylab="")
predline(asatgs_mod_fern, col="grey20",lwd=2)
ablineclip(asatgs_mod_fern, col=fernline, lwd=2, x1=0.02358104, x2=0.23880560)
points(asat ~ gs, data=ferns, col=ferncol, pch=16, cex=1.5)
text("B", x=0, y=9, cex=1.25)
title(expression(underline(Ferns)),cex=1.25, line=-1)

#fern - amass nmass
par(mar=c(5,0,1,5))
plot(amass ~ nmass, data=ferns,  ylim=c(0,1000), ylab="",
     xlim=c(0,55), xlab="",yaxt='n', type='n')
predline(namass_mod_fern, col="grey20",lwd=2)
ablineclip(namass_mod_fern, col=fernline, x1=24.2, x2=47.5, lwd=2)
points(amass ~ nmass  ,col=ferncol, pch=16, data=ferns, cex=1.5)
text("D", x=0, y=1000, cex=1.25)


dev.copy2pdf(file= "output/traits2.pdf")
dev.off()



# windows(10,6)
# 
# par(las=1,  mfrow=c(1,2),mgp=c(3,1,0), cex.lab=1.25, mar=c(5,5,1,1))
# 
# #photo vs gs
# plot(asat ~ gs, data=sela2, ylab=anetlab, xlab=condlab, ylim=c(0,9),
#      xlim=c(0,.26), type='n')
# predline(ags_mod_simp, col="grey20",lwd=2)
# predline(asatgs_mod_fern, col="grey20",lwd=2)
# ablineclip(asatgs_mod_fern, col=fernline, lwd=2,
#            x1=min(ferns2$gs), x2=max(ferns2$gs))
# points(asat ~ gs, data=sela2, col=trtcols2[habitat], pch=16, cex=1.5)
# legend("bottomright",col=c(trtcols,fernline) ,pch=16,
#        legend=c(trtlab, "Ferns"),inset=.01,  bty='n',cex=1)
# text("A", x=0, y=9, cex=1.25)
# 
# points(asat ~ gs, data=ferns, col=ferncol, pch=16, cex=1.5)
# 
# #amass v nmass
# plot(amass ~ nmass, data=sela2, ylim=c(0,1000), ylab=amasslab,
#      xlim=c(0,55), xlab=nmasslab)
# predline(an_mod_simp, col="grey20",lwd=2)
# predline(namass_mod_fern, col="grey20",lwd=2)
# ablineclip(namass_mod_fern, col=fernline, 
#            x1=min(ferns2$nmass), x2=max(ferns2$nmass),
#             lwd=2)
# points(amass ~ nmass, data=sela2, col=trtcols2[habitat], pch=16, cex=1.5)
# text("B", x=0, y=1000, cex=1.25)
# points(amass ~ nmass  ,col=ferncol, pch=16, data=ferns, cex=1.5)
# 
# dev.copy2pdf(file= "output/traits3.pdf")
# dev.off()
