source("master_scripts/plot_objects.R")
source("functions.R")
library(doBy)

#stomatal figures between ferns and selaginella

# read and format stomatal data' ------------------------------------------

stom <- read.csv("raw_data/leaf_anatomy.csv")
stom_family <-doBy::summaryBy(. ~ species, data=stom, FUN=mean)
stom$species <- factor(stom$species, levels=c("Sel_anc","Sel_art","Sel_ate",
                                              "Sel_eur","Sel_oax","Sel_swim",
                                              "Sel_umb","Bul_port","Cyc_semi",
                                              "Dip_stri","Lom_jap","Thy_cur"))

stomsize <- read.csv("raw_data/stomatal_size.csv")
stomsize$stomarea_mm2 <- with(stomsize, (length_um *width_um )/1000)
stomsize2 <- doBy::summaryBy(length_um + width_um + stomarea_mm2~ 
                               family+species+ind,FUN=mean, data=stomsize, 
                             keep.names = TRUE)
stomsize2$species <- factor(stomsize2$species, 
                            levels=c("Sel_anc","Sel_art","Sel_ate",
                            "Sel_eur","Sel_oax","Sel_swim",
                            "Sel_umb","Bul_port","Cyc_semi",
                            "Dip_stri","Lom_jap","Thy_cur"))


specieslabs2 <- c("Sel_anc","Sel_art","Sel_ate","Sel_eur","Sel_oax","Sel_swim",
                  "Sel_umb","Bul_port","Cyc_semi","Dip_stri","Lom_jap","Thy_cur")


# examine species means -----------------------------------------------------------
stoma_agg <- doBy::summaryBy(stomatadensity_numbpermm2 ~ species, data=stom, 
                             FUN=c(mean, se))


####inset figure ----------
png(filename = "output/stomata.png", width = 11, height = 8.5, units = "in", res= 400)

# windows()
par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), oma=c(5.5,0,0,0))
#stomatal density
par(mar=c(0,5,1,1))
boxplot(stomatadensity_mm2 ~ species, data=stom, ylab=denslab,
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 130))
axis(1, at=c(1:7, 9:13), labels=FALSE, tcl=.5)

#stomatal length
par(mar=c(0,5,0,1), xpd=TRUE)
boxplot(length_um ~ species, data=stomsize2, ylab="Stomatal Length  (um)", 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 70))
axis(1, at=c(1:7, 9:13), labels=FALSE)
mtext(side=1, at=1:7, line=1,text=specieslabs2[1:7], xpd=TRUE, las=2, cex=1.25)
mtext(side=1, at=9:13, line=1,text=specieslabs2[8:12], xpd=TRUE, las=2, cex=1.25)

# text(cex=1.25, x=c(1:7, 9:13)+.2, y=8.5, labels=specieslabs2, xpd=TRUE, srt=90, pos=2)

#insets are more complicated with panel figure, add them now
par(fig=c(0.65, 0.95, 0.7,0.95), mar=c(2,2,0,0), mgp=c(1,.25,0), new=T,
    cex=1 ,las=1, tcl=-.2)
boxplot(stomatadensity_mm2 ~ family, data=stom, ylab="", xlab="",
        outline=FALSE, ylim=c(0, 125))

par(fig=c(0.15, 0.45, 0.23,0.48), mar=c(2,2,0,0),mgp=c(1,.25,0),new=T, las=1, tcl=-.2)
boxplot(length_um ~ family, data=stomsize2, ylab="", xlab="",ylim=c(0,65),
        outline=FALSE)


# dev.copy2pdf(file= "output/sl_fig.pdf")
 dev.off()

#### 4 panel figure ------
 
 windows(8,8)
 par(mfrow=c(2,2), las=1,mgp=c(3,1,0),oma=c(5,5,1,1))
 
 #stomatal density
 par(mar=c(0,0,0,0))
 boxplot(stomatadensity_mm2 ~ species, data=stom, xaxt='n', outline=FALSE, 
         at=c(1:7, 9:13), ylim=c(0, 130))
 axis(1, at=c(1:7, 9:13), labels=FALSE, tcl=.5)
 mtext(side=2, at=65, line=3,text=denslab, xpd=TRUE, las=3, cex=1)
 

 #sd fern vs sela
 par(mar=c(0,0,0,0),cex=1)
 boxplot(stomatadensity_mm2 ~ family, data=stom, ylab="", xlab="",yaxt='n',xaxt='n',
         outline=FALSE, ylim=c(0, 130))
 axis(4, labels=FALSE, tcl=.5)
 
 #stomatal length
 par(mar=c(0,0,0,0), xpd=TRUE)
 boxplot(length_um ~ species, data=stomsize2,xaxt='n', 
         outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 65))
 axis(1, at=c(1:7, 9:13), labels=FALSE)
 mtext(side=1, at=1:7, line=1,text=specieslabs2[1:7], xpd=TRUE, las=2, cex=1)
 mtext(side=1, at=9:13, line=1,text=specieslabs2[8:12], xpd=TRUE, las=2, cex=1)
 mtext(side=2, at=32.5, line=3,text="Stomatal Length  (um)", xpd=TRUE, las=3, cex=1)
 
 #sl fern vs sela
 par(mar=c(0,0,0,0), xpd=TRUE)
 boxplot(length_um ~ family, data=stomsize2, ylab="", xlab=stomsize2$family,
         ylim=c(0,65),yaxt='n',
         outline=FALSE)
 axis(4, labels=FALSE, tcl=.5)
 # dev.copy2pdf(file= "output/test.pdf")
 # dev.off()
