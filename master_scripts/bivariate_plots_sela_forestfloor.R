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

##select only matched habitats
sela2 <- sela2[sela2$habitat == "understory_midlight",]
sela2 <- droplevels(sela2)

###models now do not have habitat comparisons ------
ags_mod <- lmer(asat ~ gs + (1|species), data=sela2)
Anova(ags_mod)
summary(ags_mod)
ags_mod_simp <- lm(asat ~ gs, data=sela2)

an_mod <- lmer(amass ~ nmass  + (1|species), data=sela2)
Anova(an_mod)
summary(an_mod)
an_mod_simp <- lm(amass ~ nmass, data=sela2)

#ferns
ags_mod_fern <- lmer(asat ~ gs + (1|species), data=ferns)
Anova(ags_mod_fern)
asatgs_mod_fern_simp <- lm(asat ~ gs, data=ferns)

an_mod_fern <- lmer(amass ~ nmass  + (1|species), data=ferns)
Anova(an_mod_fern)
namass_mod_fern_simp<- lm(amass ~ nmass ,data=ferns)

#r2 values
r.squaredGLMM(ags_mod)
r.squaredGLMM(an_mod)
r.squaredGLMM(ags_mod_fern)
r.squaredGLMM(an_mod_fern)

## plot bits------
library(scales)

##ploting-------
# windows()

# png(filename = "output/bivariate.png", width = 8, height = 9, 
#     units = "in", res= 400)

jpeg(filename = "output/manuscript_figures/Figure_4_onlyforestfloor.jpeg",
     width = 8.4, height = 8.4, units = "in", res= 300)

layout(matrix(c(1:4), nrow=2, ncol=2))
par(las=1,  mgp=c(3,1,0), cex.lab=1.25)

#photo vs gs
par(mar=c(5,5,1,0))
plot(asat ~ gs, data=sela2, ylab=anetlab, xlab="", ylim=c(0,9),
     xlim=c(0,.27), type='n')
predline(ags_mod_simp, col="grey20",lwd=2, lty=2)
points(asat ~ gs, data=sela2, col=trtcols2[3], pch=16, cex=1.5)
legend("bottomright",col=trtcols,pch=16,legend=trtlab,inset=.01,  bty='n',
       cex=1)
text("A", x=0, y=9, cex=1.25)
title(expression(italic(underline(Selaginella))), line=-1)
mtext(side=1, text=condlab, line=3.5,at=.25)

text(.04, 6.75, expression(paste(R[cond]^{"2"}," = "," 0.40")))
text(.04, 5.9, expression(paste(R[marg]^{"2"}," = "," 0.90")))

#amass v nmass
par(mar=c(5,5,1,0))
plot(amass ~ nmass, data=sela2, ylim=c(0,1000), ylab=amasslab,
     xlim=c(0,55), xlab="")
# predline(an_mod_simp, col="grey20",lwd=2, lty=2) #not significant
points(amass ~ nmass, data=sela2, col=trtcols2[3], pch=16, cex=1.5)
text("C", x=0, y=1000, cex=1.25)
mtext(side=1, text=nmasslab, line=3.5,at=55)

# text(8, 750, expression(paste(R[cond]^{"2"}," = "," 0.43")))
# text(8, 650, expression(paste(R[marg]^{"2"}," = "," 0.81")))

#fern: a gs
par(mar=c(5,0,1,5))
plot(asat ~ gs, data=ferns, yaxt="n", xlab="", ylim=c(0, 9),
     xlim=c(0,.27), ylab="")
predline(asatgs_mod_fern_simp, col="grey20",lwd=2, lty=2)
# ablineclip(asatgs_mod_fern, col=fernline, lwd=2, x1=0.02358104, x2=0.23880560)
points(asat ~ gs, data=ferns, col=trtcols2[3], pch=16, cex=1.5)
text("B", x=0, y=9, cex=1.25)
title(expression(underline(Ferns)),cex=1.25, line=-1)

text(.04, 6.75, expression(paste(R[cond]^{"2"}," = "," 0.30")))
text(.04, 5.9, expression(paste(R[marg]^{"2"}," = "," 0.71")))

#fern - amass nmass
par(mar=c(5,0,1,5))
plot(amass ~ nmass, data=ferns,  ylim=c(0,1000), ylab="",
     xlim=c(0,55), xlab="",yaxt='n', type='n')
# predline(namass_mod_fern, col="grey20",lwd=2, lty=2)
# ablineclip(namass_mod_fern, col=fernline, x1=24.2, x2=47.5, lwd=2)
points(amass ~ nmass  ,col=trtcols2[3], pch=16, data=ferns, cex=1.5)
text("D", x=0, y=1000, cex=1.25)

# text(10, 800, expression(paste(R[cond]^{"2"}," = "," 0.43")))
# text(10, 700, expression(paste(R[marg]^{"2"}," = "," 0.81")))

# dev.copy2pdf(file= "output/traits2.pdf")
dev.off()

