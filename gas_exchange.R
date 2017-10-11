source("master_scripts/plot_objects.R")
#gas exchange date for seligenalla and other ferns

photo <- read.csv("raw_data/gas_exchange.csv")
photo2 <- read.csv("raw_data/leaf_gasexchange.csv")
photo2_low <- photo2[photo2$light_umol == 50,]

# basic plotting ----------------------------------------------------------------

#raw data from low light (50umols)
boxplot(photo ~ family, data=photo2_low)
boxplot(cond ~ family, data=photo2_low)
boxplot(photo/cond ~ family, data=photo2_low)
#selaginella lower photo and cond than ferns

sela <- droplevels(photo2_low[photo2_low$family == "Selaginella",])

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

#merge with photo and use sla to calculate amass (watch units) (different individial, use mean sla)
amass <- merge(photo2_low ,lma_agg)
  amass$amass <- with(amass, photo * sla *1000) #umols co2/ug 



# amass versus nitro ------------------------------------------------------

#merge leaf nitron on a mass balance

chem <- read.csv("calculated_data/leaf_chem_means.csv")
  
amass_chem <- merge(amass, chem, all = TRUE)  
  

plot(amass ~ nmass, data=amass_chem, col=species, pch=16)

#just selaginella
sela <- amass_chem[amass_chem$family == "Selaginella",]

plot(amass ~ nmass, data=sela, col=species, pch=16)
plot(amass ~ pmass, data=sela, col=species, pch=16, xlim=c(0,1.25),ylim=c(0,600))
