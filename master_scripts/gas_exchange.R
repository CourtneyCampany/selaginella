source("master_scripts/plot_objects.R")
source("functions.R")
library(doBy)
library(scales)
#gas exchange date for seligenalla and other ferns

amax <- read.csv("raw_data/gas_exchange.csv") 
# treat <- read.csv("raw_data/treatments.csv")
#   amax <- merge(amax, treat)

# calcualte Amass ---------------------------------------------------------

#need LMA
lma <- read.csv("raw_data/leaf_anatomy.csv")
  lma <- lma[,c(1:3,5)]
  lma$sla <- 1/lma$lma_µgpermm2 #m2/ug

lma_agg <- summaryBy(sla ~ species, data=lma, FUN=mean, keep.names = TRUE)
#merge with photo and use sla to calculate amass (watch units)
amass <- merge(amax ,lma)
  amass$amass <- with(amass, photo * sla *1000) #umols co2/ug 

# amass versus nitro ------------------------------------------------------

#merge leaf nitro on a mass balance

chem <- read.csv("raw_data/leaf_chemistry.csv")
  chem$nmass <- with(chem, (mass_ug * (percN/100))*1000) #mg/g???
  chem$pmass <- with(chem, (mass_ug * (percP/100))*1000)
  
amass_chem <- merge(amass, chem, all = TRUE)  
amass_chem2 <- amass_chem[,c(1:8, 10,16:17)]
amass_chem3 <- amass_chem2[complete.cases(amass_chem2),]
amass_chem3$pnue2 <- with(amass_chem3, amass/nmass)
amass_chem3$pnue <- with(amass_chem3, photo/percN)
amass_chem3$ppue2 <- with(amass_chem3, amass/pmass)

#plotobjects

#plot sela photo vs chem habitats (wont work now, have to susbet sela)--------

plot(amass ~ nmass, data=amass_chem, col=trtcols2[habitat], pch=16, 
     ylim=c(0, 1200), xlim=c(0,16))
legend("topleft", trtlab, pch=16, col=palette(), bty='n', inset=.01)

plot(amass ~ pmass, data=amass_chem, col=trtcols2[habitat], pch=16, 
     ylim=c(0, 1200), xlim=c(0, 1.6))
legend("topleft", trtlab, pch=16, col=palette(), bty='n', inset=.01)


amass_chem$habitat <- factor(amass_chem$habitat, levels=c("full_sun","understory_midlight",
                                                          "understory_lowlight","swamp_lowlight"))
# write.csv(amass_chem, "calculated_data/sela_photo_chem.csv", row.names = FALSE)

boxplot(percN ~ habitat, data=amass_chem, col=trtcols)
boxplot(percP ~ habitat, data=amass_chem, col=trtcols)
boxplot(cn_ratio ~ habitat, data=amass_chem, col=trtcols)
boxplot(lma_µgpermm2 ~ habitat, data=amass_chem, col=trtcols)
boxplot(photo ~ habitat, data=amass_chem, col=trtcols)
boxplot(cond ~ habitat, data=amass_chem, col=trtcols)
boxplot(chlorophyll_mgperl ~ habitat, data=amass_chem, col=trtcols)


#phot vs chem on a mass basis (ferns vs sela)--------
# plot(amass ~ nmass, data=amass_chem3, col=familycols[family], pch=16)
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#       pch=16, bty='n', inset=.01)

# plot(amass ~ pmass, data=amass_chem3, col=familycols[family], pch=16, xlim=c(0, 1.8),ylim=c(0, 1200))
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01)


#PNUE--------
#delete one outlier
amass_chem4 <- droplevels(amass_chem3[amass_chem3$pnue2 < 20,])

library(scales)
lma_pnue_mod<- lm(pnue2~ lma_µgpermm2 ,data=amass_chem4)

boxplot(amass/nmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, 
        ylab="PNUE")
boxplot(amass/pmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, 
        ylab="PPUE")

windows(8,6)
par(mar=c(5,5,1,1), mgp=c(3,1,0),cex.axis=0.8, las=1)
plot(pnue2 ~ lma_µgpermm2, data=amass_chem4, type='n',
     xlim=c(0, 25),xlab=lmalab, ylab=nuelab, ylim=c(-1, 12.5))
legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,
       pch=16, bty='n', inset=.01, cex=1.25)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(pnue2 ~ lma_µgpermm2, data=amass_chem4, col=famcols[family], 
       cex=1.5,pch=16)
dev.copy2pdf(file= "output/nue_fig.pdf")
dev.off()


#figure out units then makes this figure
# plot(ppue ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], type='n')
# legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01, cex=1.25)
# predline(lma_ppue_mod, col="grey20",lwd=2, lty=2)
# points(pnue2 ~ lma_µgpermm2, data=amass_chem4, col=familycols[family], 
#        cex=1.5,pch=16)

# plot(amass/pmass ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], pch=16)



