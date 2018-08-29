source("master_scripts/plot_objects.R")
source("functions.R")
library(doBy)
library(vioplot)

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


specieslabs2 <- c("Sel_anc","Sel_art","Sel_ati","Sel_eur","Sel_oax","Sel_sp",
                  "Sel_umb","Bul_port","Cyc_semi","Dip_stri","Lom_jap","Thy_cur")

sela_sl <- stomsize2[stomsize2$family == "Selaginella", "length_um"]
fern_sl <- stomsize2[stomsize2$family == "Ferns", "length_um"]

sela_sd <- stom[stom$family == "Selaginella", "stomatadensity_mm2"]
fern_sd <- stom[stom$family == "Ferns", "stomatadensity_mm2"]

#### 4 panel figure ------
# png(filename = "output/stomata.png", width = 11, height = 8.5, units = "in", res= 400)
 
# jpeg(filename = "output/manuscript_figures/Figure_3.jpeg", 
#       width = 8.4, height = 8.4, units = "in", res= 300)

setEPS()
postscript("output/manuscript_figures/Figure_3.eps")

 # windows(8,8)
 par(mfrow=c(2,2), las=1,mgp=c(3,1,0),oma=c(6,5,1,1))
 
 #stomatal density
 par(mar=c(0,0,0,0))
 boxplot(stomatadensity_mm2 ~ species, data=stom, ylab="", xlab="",yaxt='n',xaxt='n',
         outline=FALSE, at=c(1:7, 9:13), ylim=c(0, 135), 
         whisklwd=2,whisklty=1,staplelty = 0)
 axis(1, at=c(1:7, 9:13), labels=FALSE, tcl=.5)
 axis(2, labels=TRUE)
 mtext(side=2, at=65, line=3,text=denslab, xpd=TRUE, las=3, cex=1)
 text(x=.7, y=133, "A", cex=1.5)
 
 #sd fern vs sela
 par(mar=c(0,0,0,0))
 
 plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,135), xaxt='n', yaxt='n', 
      ylab="", xlab="")
 vioplot(sela_sd, fern_sd,at=1:2 ,add=TRUE,
         col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
 axis(2, labels=FALSE, tcl=.25)
 
 # boxplot(stomatadensity_mm2 ~ family, data=stom, ylab="", xlab="",yaxt='n',xaxt='n',
 #         outline=FALSE, ylim=c(0, 130), 
 #         whisklwd=2,whisklty=1,staplelty = 0)
 axis(2, labels=FALSE, tcl=.25)
 axis(1, labels=FALSE, tcl=.25)
 text(x=.55, y=133, "B", cex=1.5)

 #stomatal length
 par(mar=c(0,0,0,0), xpd=TRUE)
 boxplot(length_um ~ species, data=stomsize2,xaxt='n', 
         outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 65), 
         whisklwd=2,whisklty=1,staplelty = 0)
 axis(1, at=c(1:7, 9:13), labels=FALSE)
 mtext(side=1, at=1:7, line=1,text=specieslabs2[1:7], xpd=TRUE, las=2, cex=1)
 mtext(side=1, at=9:13, line=1,text=specieslabs2[8:12], xpd=TRUE, las=2, cex=1)
 mtext(side=2, at=32.5, line=3,text="Stomatal Length  (um)", xpd=TRUE, las=3, cex=1)
 text(x=.7, y=62.5, "C", cex=1.5)
 
 #sl fern vs sela
 par(mar=c(0,0,0,0), xpd=TRUE)
 
 plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,65), xaxt='n', yaxt='n', ylab="", xlab="")
 vioplot(sela_sl, fern_sl, at=1:2 ,add=TRUE,
         col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
 axis(2, labels=FALSE, tcl=.25)
 axis(1, labels = c("Selaginella", "Ferns"), at=1:2)
 text(x=.55, y=62.5, "D", cex=1.5)
 # boxplot(length_um ~ family, data=stomsize2, ylab="", xlab=stomsize2$family,
 #         ylim=c(0,65),yaxt='n', outline=FALSE, 
 #         whisklwd=2,whisklty=1,staplelty = 0)

 
# dev.copy2pdf(file= "output/stomata.pdf")
dev.off()

####inset figure ----------
# png(filename = "output/stomata.png", width = 11, height = 8.5, units = "in", res= 400)
# 
# # windows()
# par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), oma=c(5.5,0,0,0))
# #stomatal density
# par(mar=c(0,5,1,1))
# boxplot(stomatadensity_mm2 ~ species, data=stom, ylab=denslab,
#         xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 130))
# axis(1, at=c(1:7, 9:13), labels=FALSE, tcl=.5)
# 
# #stomatal length
# par(mar=c(0,5,0,1), xpd=TRUE)
# boxplot(length_um ~ species, data=stomsize2, ylab="Stomatal Length  (um)", 
#         xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 70))
# axis(1, at=c(1:7, 9:13), labels=FALSE)
# mtext(side=1, at=1:7, line=1,text=specieslabs2[1:7], xpd=TRUE, las=2, cex=1.25)
# mtext(side=1, at=9:13, line=1,text=specieslabs2[8:12], xpd=TRUE, las=2, cex=1.25)
# 
# # text(cex=1.25, x=c(1:7, 9:13)+.2, y=8.5, labels=specieslabs2, xpd=TRUE, srt=90, pos=2)
# 
# #insets are more complicated with panel figure, add them now
# par(fig=c(0.65, 0.95, 0.7,0.95), mar=c(2,2,0,0), mgp=c(1,.25,0), new=T,
#     cex=1 ,las=1, tcl=-.2)
# boxplot(stomatadensity_mm2 ~ family, data=stom, ylab="", xlab="",
#         outline=FALSE, ylim=c(0, 125))
# 
# par(fig=c(0.15, 0.45, 0.23,0.48), mar=c(2,2,0,0),mgp=c(1,.25,0),new=T, las=1, tcl=-.2)
# boxplot(length_um ~ family, data=stomsize2, ylab="", xlab="",ylim=c(0,65),
#         outline=FALSE)
# 
# 
# # dev.copy2pdf(file= "output/sl_fig.pdf")
# dev.off()
# 
#  