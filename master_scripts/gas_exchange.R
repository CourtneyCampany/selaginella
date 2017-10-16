source("master_scripts/plot_objects.R")
#gas exchange date for seligenalla and other ferns

# photo <- read.csv("raw_data/gas_exchange.csv")
# photo2 <- read.csv("raw_data/leaf_gasexchange.csv")
# photo2_low <- photo2[photo2$light_umol == 50,]

amax <- read.csv("raw_data/gas_exchange_selaginella.csv") #does not have ferns

# basic plotting ----------------------------------------------------------------

#raw data from low light (50umols)
boxplot(photo ~ family, data=amax)
boxplot(cond ~ family, data=amax)
boxplot(photo/cond ~ family, data=amax)
#selaginella lower photo and cond than ferns

# sela <- droplevels(photo2_low[photo2_low$family == "Selaginella",])

# windows(12,6)

windows(4,6)
par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1, mgp=c(3.5,1,0))

par(mar=c(0,5,2,1))
boxplot(photo ~ species, sela,ylab=photolab, names=FALSE, 
        outline=FALSE, ylim=c(0,8), xaxt="n")
text(x=5.5, y=8, "(a)", cex=1)

par(mar=c(5,5,0,1))
boxplot(cond~ species, sela, ylab=condlab, ylim=c(0, .15),
        outline=FALSE,xaxt="n")
axis(1,at=1:5, labels=unique(sela$species), las=3)
text(x=5.5, y=.15, "(b)", cex=1, las=3)

# par(mar=c(5,5,0,1))
# boxplot(Ci ~ Species, photo, ylab=cilab, outline=FALSE, ylim=c(0,500))
# text(x=7.5, y=475, "(c)", cex=1.5)


# calcualte Amass ---------------------------------------------------------

#need LMA

lma <- read.csv("raw_data/leaf_anatomy.csv")
  lma <- lma[,c(1:3,5)]
  lma$sla <- 1/lma$lma_µgpermm2 #m2/ug
library(doBy)
lma_agg <- summaryBy(sla ~ species, data=lma, FUN=mean, keep.names = TRUE)
lma_sela <- lma[lma$family == "Selaginella",]

#merge with photo and use sla to calculate amass (watch units)
amass <- merge(amax ,lma_sela)
  amass$amass <- with(amass, photo * sla *1000) #umols co2/ug 


# amass versus nitro ------------------------------------------------------

#merge leaf nitron on a mass balance

chem <- read.csv("raw_data/leaf_chemistry.csv")
  chem$nmass <- with(chem, mass_ug * percN)
  chem$pmass <- with(chem, mass_ug * percP)
chem_sela <- chem[chem$family == "Selaginella",]
  
amass_chem <- merge(amass, chem_sela, all = TRUE)  
  
#plot photo vs chem-------------- (need habitats)

plot(amass ~ nmass, data=amass_chem, col=species, pch=16, ylim=c(0, 1200), xlim=c(0,16))

plot(amass ~ pmass, data=amass_chem, col=species, pch=16, ylim=c(0, 1200), xlim=c(0, 1.6))

