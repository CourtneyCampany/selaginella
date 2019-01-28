
source("master_scripts/plot_objects.R")

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


###figure for job talk--------

gradient <- colorRampPalette(c("orange","darkgreen"))
palette(gradient(3))
trtcols <- palette(gradient(3))

sela2$habitat<-factor(sela2$habitat, 
                      levels=c("full_sun", "understory_midlight", "swamp_lowlight"))

boxlabs <- c("Open canopy","Closed canopy", "Swamp")

# windows()
jpeg(filename = "jobtalk/nitro.jpeg",
     width = 7, height = 7, units = "in", res= 400)
par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))
boxplot(N ~ habitat, data=sela2, ylim=c(0,4.5),col=trtcols,outline=FALSE,
        ylab= "Foliar N content (%)", xaxt='n')
axis(1, at=1:3, labels = boxlabs, cex.axis=1.25)

dev.off()
