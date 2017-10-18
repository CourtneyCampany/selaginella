source("master_scripts/plot_objects.R")
#gas exchange date for seligenalla and other ferns

amax <- read.csv("raw_data/gas_exchange.csv") #does not have ferns
treat <- read.csv("raw_data/treatments.csv")
  amax <- merge(amax, treat)
amax_sela <- droplevels(amax[amax$family == "Selaginella",])

# calcualte Amass ---------------------------------------------------------

#need LMA
lma <- read.csv("raw_data/leaf_anatomy.csv")
  lma <- lma[,c(1:3,5)]
  lma$sla <- 1/lma$lma_µgpermm2 #m2/ug
library(doBy)
lma_agg <- summaryBy(sla ~ species, data=lma, FUN=mean, keep.names = TRUE)
lma_sela <- lma[lma$family == "Selaginella",]

#merge with photo and use sla to calculate amass (watch units)
amass <- merge(amax_sela ,lma_sela)
  amass$amass <- with(amass, photo * sla *1000) #umols co2/ug 

# amass versus nitro ------------------------------------------------------

#merge leaf nitron on a mass balance

chem <- read.csv("raw_data/leaf_chemistry.csv")
  chem$nmass <- with(chem, mass_ug * percN)
  chem$pmass <- with(chem, mass_ug * percP)
chem_sela <- chem[chem$family == "Selaginella",]
  
amass_chem <- merge(amass, chem_sela, all = TRUE)  
  
#plot photo vs chem-------------- (need habitats)

plot(amass ~ nmass, data=amass_chem, col=trtcols2[habitat], pch=16, ylim=c(0, 1200), xlim=c(0,16))
legend("topleft", trtlab, pch=16, col=palette(), bty='n', inset=.01)

plot(amass ~ pmass, data=amass_chem, col=trtcols2[habitat], pch=16, ylim=c(0, 1200), xlim=c(0, 1.6))
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
