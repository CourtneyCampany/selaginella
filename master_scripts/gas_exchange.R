source("master_scripts/plot_objects.R")
source("functions.R")
#gas exchange date for seligenalla and other ferns

amax <- read.csv("raw_data/gas_exchange.csv") 
#for now we dont have leaf mass for ferns, so only sela here
amax_sela <- amax[amax$family == "Selaginella",]

# calcualte Amass ---------------------------------------------------------

#need LMA
lma <- read.csv("raw_data/leaf_anatomy.csv")
  lma$sla <- 1/lma$lma_gpercm2 
  
library(doBy)
lma_agg <- summaryBy(sla ~ species, data=lma, FUN=mean, keep.names = TRUE)
#merge with photo and use sla to calculate amass (watch units)
amass <- merge(amax_sela ,lma)
  amass$amass <- with(amass, (photo *1000) * (sla/1000)) ####start here and get units right
  #nmols co2/g s
  
# amass versus nitro ------------------------------------------------------

#merge leaf nitro on a mass balance

chem <- read.csv("raw_data/leaf_chemistry.csv")
chem_sela <- chem[chem$family == "Selaginella",]

  
amass_chem <- merge(amass, chem_sela, all = TRUE)  
  amass_chem$nmass <- with(amass_chem, leaf_mass_g * percN)
  amass_chem$pmass <- with(amass_chem, leaf_mass_g * percP)

# write.csv(amass_chem, "calculated_data/sela_photo_chem.csv", row.names = FALSE)
# amass_chem2 <- amass_chem[,c(1:9, 14:18)]#can remove pmass to add one more row
amass_chem3 <- amass_chem[complete.cases(amass_chem),]
amass_chem3$nue <- with(amass_chem3, amass/nmass)
  
#phot vs chem on a mass basis (ferns vs sela)------------------
familycols <- c("cornflowerblue", "forestgreen")

plot(amass ~ nmass, data=amass_chem3, col=familycols[family], pch=16)
legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
      pch=16, bty='n', inset=.01)

# plot(amass ~ pmass, data=amass_chem3, col=familycols[family], pch=16, xlim=c(0, 1.8),ylim=c(0, 1200))
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01)

plot(amass ~ nmass, data=amass_chem3[amass_chem3$species != "Sel_eur",], 
     col=familycols[family], pch=16)
legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
       pch=16, bty='n', inset=.01)

#PNUE--------
#delete one outlier
# amass_chem4 <- droplevels(amass_chem3[amass_chem3$nue < 200,])

library(scales)
lma_pnue_mod<- lm(nue~ lma_gpercm2 ,data=amass_chem3)

# boxplot(amass/nmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, ylab="PNUE")
# boxplot(amass/pmass ~ family, data=amass_chem4, col=familycols, outline=FALSE, ylab="PPUE")

windows(7,7)
par(mar=c(5,5,2,2), las=1)
plot(nue ~ lma_gpercm2, data=amass_chem3, type='n', ylab="NUE",xlab=lmalab, 
     ylim=c(-.1, .6),xim=c(0,20))
# legend("topright", legend=c("Ferns", "Selaginella"), col=[species],
#        pch=16, bty='n', inset=.01)
predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(nue ~ lma_gpercm2, data=amass_chem3, col=species, pch=16)

dev.copy2pdf(file= "output/nue_lma.pdf")
dev.off()

# plot(pnue ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], pch=16)
# legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
#        pch=16, bty='n', inset=.01)

# plot(amass/pmass ~ lma_µgpermm2, data=amass_chem3, col=familycols[family], pch=16)



