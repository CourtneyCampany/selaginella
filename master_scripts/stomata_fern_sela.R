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


#stomatal density
windows(8,6)
par(mar=c(5,5,2,2), las=1)
boxplot(stomatadensity_numbpermm2 ~ species, data=stom, ylab=denslab, 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 130))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-20, labels=specieslabs2, xpd=TRUE, srt=90)

par(fig=c(0.6, 0.9, 0.6,0.9), mar=c(2,2,0,0),mgp=c(1,1,0),new=T, 
    cex=1 ,las=1,  cex.axis=1, cex.lab=1, tcl=-.25)
boxplot(stomatadensity_numbpermm2 ~ family, data=stom, ylab="", xlab="",
        outline=FALSE, ylim=c(0, 125))
dev.copy2pdf(file= "output/sd_fig.pdf")
dev.off()

#stomatal length
windows(8,6)
par(mar=c(5,5,2,2), las=1)
boxplot(length_um ~ species, data=stomsize2, ylab="Stomatal Length  (um)", 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 70))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-12, labels=specieslabs2, xpd=TRUE, srt=90)

par(fig=c(0.15, 0.45, 0.6,0.9), mar=c(2,2,0,0),mgp=c(1,1,0),new=T, 
    cex=1 ,las=1,  cex.axis=1, cex.lab=1, tcl=-.25)
boxplot(length_um ~ family, data=stomsize2, ylab="", xlab="",ylim=c(0,60),
        outline=FALSE)

dev.copy2pdf(file= "output/sl_fig.pdf")
dev.off()
