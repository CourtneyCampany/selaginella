source("master_scripts/plot_objects.R")
source("functions.R")
#gas exchange date for seligenalla and other ferns

amax <- read.csv("raw_data/gas_exchange.csv") 
# treat <- read.csv("raw_data/treatments.csv")
#   amax <- merge(amax, treat)

# calcualte Amass ---------------------------------------------------------

#need LMA
lma <- read.csv("raw_data/leaf_anatomy.csv")
  lma <- lma[,c(1:3,5)]
  lma$sla <- 1/lma$lma_µgpermm2 #m2/ug
library(doBy)
lma_agg <- summaryBy(sla ~ species, data=lma, FUN=mean, keep.names = TRUE)
#merge with photo and use sla to calculate amass (watch units)
amass <- merge(amax ,lma)
  amass$amass <- with(amass, photo * sla *1000) #umols co2/ug 

# amass versus nitro ------------------------------------------------------

#merge leaf nitro on a mass balance

chem <- read.csv("raw_data/leaf_chemistry.csv")
  chem$nmass <- with(chem, mass_ug * percN)
  chem$pmass <- with(chem, mass_ug * percP)
  
amass_chem <- merge(amass, chem, all = TRUE)  
# write.csv(amass_chem, "calculated_data/sela_photo_chem.csv", row.names = FALSE)
amass_chem2 <- amass_chem[,c(1:9, 11,17:18)]
amass_chem3 <- amass_chem2[complete.cases(amass_chem2),]
amass_chem3$pnue2 <- with(amass_chem3, amass/nmass)
amass_chem3$pnue <- with(amass_chem3, photo/percN)
  
#plot sela photo vs chem habitats (wont work now, have to susbet sela)--------

# boxplot(percN ~ habitat, data=amass_chem, col=trtcols)
# boxplot(percP ~ habitat, data=amass_chem, col=trtcols)
# boxplot(cn_ratio ~ habitat, data=amass_chem, col=trtcols)
# boxplot(lma_µgpermm2 ~ habitat, data=amass_chem, col=trtcols)
# boxplot(photo ~ habitat, data=amass_chem, col=trtcols)
# boxplot(cond ~ habitat, data=amass_chem, col=trtcols)
# boxplot(chlorophyll_mgperl ~ habitat, data=amass_chem, col=trtcols)


#phot vs chem on a mass basis (ferns vs sela)--------
familycols <- c("cornflowerblue", "forestgreen")

plot(amass ~ nmass, data=amass_chem3, col=familycols[family], pch=16, 
  xlim=c(0, 18), ylim=c(0,1200))
legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
      pch=16, bty='n', inset=.01)

# plot(amass ~ pmass, data=amass_chem3, col=familycols[family], pch=16, xlim=c(0, 1.8),ylim=c(0, 1200))
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01)


#PNUE--------
#delete one outlier
amass_chem4 <- droplevels(amass_chem3[amass_chem3$pnue2 < 200,])

library(scales)
lma_pnue_mod<- lm(pnue2~ lma_µgpermm2 ,data=amass_chem4)

# boxplot(amass/nmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, ylab="PNUE")
# boxplot(amass/pmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, ylab="PPUE")

windows(7,7)
par(mar=c(5,5,2,2), las=1)
plot(pnue2 ~ lma_µgpermm2, data=amass_chem4, type='n', ylab="NUE",xlab=lmalab,
     ylim=c(-5, 125), xlim=c(0, 27.5))
legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,
       pch=16, bty='n', inset=.01)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(pnue2 ~ lma_µgpermm2, data=amass_chem4, col=familycols[family], pch=16)

dev.copy2pdf(file= "output/nue_lma.pdf")
dev.off()

# plot(pnue ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], pch=16)
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01)

# plot(amass/pmass ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], pch=16)



