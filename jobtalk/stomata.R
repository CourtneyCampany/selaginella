source("master_scripts/plot_objects.R")
source("functions.R")
library(doBy)
library(vioplot)

#stomatal figures between ferns and selaginella
habitat <- read.csv("raw_data/treatments.csv")

# read and format stomatal data' ------------------------------------------

stom <- read.csv("raw_data/leaf_anatomy.csv")
  stom_family <-doBy::summaryBy(. ~ species, data=stom, FUN=mean)
  stom$species <- factor(stom$species, levels=c("Sel_art","Sel_ate","Sel_eur",
                                                "Sel_swim","Sel_umb","Sel_anc",
                                                "Sel_oax","Bul_port","Cyc_semi",
                                                "Dip_stri","Lom_jap","Thy_cur"))
  # stom <- merge(stom, habitat, all=TRUE)
  # stom$habitat <- as.character(stom$habitat)
  # stom$habitat <- ifelse(stom$family == "Ferns", "understory_midlight", stom$habitat)

stomsize <- read.csv("raw_data/stomatal_size.csv")
stomsize$stomarea_mm2 <- with(stomsize, (length_um *width_um )/1000)
stomsize2 <- doBy::summaryBy(length_um + width_um + stomarea_mm2~ 
                               family+species+ind,FUN=mean, data=stomsize, 
                             keep.names = TRUE)
stomsize2$species <- factor(stomsize2$species, 
                            levels=c("Sel_art","Sel_ate","Sel_eur",
                                     "Sel_swim","Sel_umb","Sel_anc",
                                     "Sel_oax","Bul_port","Cyc_semi",
                                     "Dip_stri","Lom_jap","Thy_cur"))
  # stomsize2 <- merge(stomsize2, habitat, all=TRUE)
  # stomsize2$habitat <- as.character(stomsize2$habitat)
  # stomsize2$habitat <- ifelse(stomsize2$family == "Ferns", "understory_midlight", 
  #                             stomsize2$habitat)
  # stomsize2$habitat <- as.factor(stomsize2$habitat)

sela_sl <- stomsize2[stomsize2$family == "Selaginella", "length_um"]
fern_sl <- stomsize2[stomsize2$family == "Ferns", "length_um"]

sela_sd <- stom[stom$family == "Selaginella", "stomatadensity_mm2"]
fern_sd <- stom[stom$family == "Ferns", "stomatadensity_mm2"]

##order of species from plotting from editor comments (by habitats)
specieslabs2 <- c("Sel_art", "Sel_ate", "Sel_eur", "Sel_swim", "Sel_umb", "Sel_anc", 
             "Sel_oax", "Bul_port", "Cyc_semi" ,"Dip_stri" ,"Lom_jap" , "Thy_cur")


#### 2 panel figure ------

jpeg(filename = "jobtalk/stomata.jpeg",
      width = 10, height = 6, units = "in", res= 400)

 # windows(10,6)
 par(mfrow=c(1,2), las=1,mgp=c(3,1,0), cex.axis=1, cex.lab=1.25)
 
 #sd fern vs sela
 par(mar=c(4,5,1,0))
 
 plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,140), xaxt='n', ylab=denslab, 
      xlab="")
 vioplot(sela_sd, fern_sd,at=1:2 ,add=TRUE,
         col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
 axis(1, labels = c("Selaginella", "Ferns"), at=1:2, cex.axis=1.25)
 text(x=.55, y=138, "A", cex=1.5)

 #sl fern vs sela
 par(mar=c(4,0,1,5))
 
 plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,65), xaxt='n', yaxt='n', ylab="",
      xlab="")
 vioplot(sela_sl, fern_sl, at=1:2 ,add=TRUE,
         col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
 axis(1, labels = c("Selaginella", "Ferns"), at=1:2, cex.axis=1.25)
 axis(4,  tcl=.5)
 text(par("usr")[2]+.4,par("usr")[4]-20, cex=1.25,
      "Stomatal Length  (um)", srt = -90, xpd = TRUE, pos = 4)
 text(x=.55, y=64, "B", cex=1.5)

dev.off()


###stomata dens - sela

gradient <- colorRampPalette(c("orange","darkgreen"))
palette(gradient(3))
trtcols <- palette(gradient(3))

habitat <- read.csv("raw_data/treatments.csv")

stom <- merge(stom, habitat)

stom$habitat<-factor(stom$habitat, 
                      levels=c("full_sun", "understory_midlight", "swamp_lowlight"))

boxlabs <- c("Open canopy","Closed canopy", "Swamp")

# windows()
jpeg(filename = "jobtalk/stodens.jpeg",
     width = 7, height = 7, units = "in", res= 400)
par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))
boxplot(stomatadensity_mm2 ~ habitat, data=stom,col=trtcols,outline=FALSE,
        ylab= denslab, xaxt='n', ylim=c(0, 150))
axis(1, at=1:3, labels = boxlabs, cex.axis=1.25)

dev.off()
