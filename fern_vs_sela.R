source("master_scripts/plot_objects.R")
library(doBy)

##sela vs ferns---- read in gas exchange, leaf chemistry and leaf anatomy-----
gas_exchange <- read.csv("raw_data/gas_exchange.csv")
#photo_family <-summaryBy(. ~ family, data=gas_exchange, FUN=c(mean,se))
#order species for plotting
gas_exchange$species <- factor(gas_exchange$species, levels=c("Sel_anc","Sel_art","Sel_ate",
                                                        "Sel_eur","Sel_oax","Sel_swim","Sel_umb",
                                                        "Bul_port","Cyc_semi","Dip_stri","Thy_cur"))

specieslabs <- c("Sel_anc","Sel_art","Sel_ate","Sel_eur","Sel_oax","Sel_swim",
                  "Sel_umb","Bul_port","Cyc_semi","Dip_stri","Thy_cur")

chem <- read.csv("raw_data/leaf_chemistry.csv")
#chem2 <- droplevels(chem[, -c(3,4)])
#chem_family <-summaryBy(. ~ family, data=chem2, FUN=se)
chem$species <- factor(chem$species, levels=c("Sel_anc","Sel_art","Sel_ate",
                                              "Sel_eur","Sel_oax","Sel_swim",
                                              "Sel_umb","Bul_port","Cyc_semi",
                                              "Dip_stri","Lom_jap","Thy_cur"))

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


stomsize2$species <- factor(stomsize2$species, levels=c("Sel_anc","Sel_art","Sel_ate",
                                              "Sel_eur","Sel_oax","Sel_swim",
                                              "Sel_umb","Bul_port","Cyc_semi",
                                              "Dip_stri","Lom_jap","Thy_cur"))


specieslabs2 <- c("Sel_anc","Sel_art","Sel_ate","Sel_eur","Sel_oax","Sel_swim",
                "Sel_umb","Bul_port","Cyc_semi","Dip_stri","Lom_jap","Thy_cur")


#this starts the combined dataset for ferns/sela with stomatal traits-----
# library(plyr)
# chem_agg <- ddply(chem2, "family", summarise,
#                N    = sum(!is.na(percP)),
#                mean = mean(percP, na.rm=TRUE),
#                sd   = sd(percP, na.rm=TRUE),
#                se   = sd / sqrt(N)
# )


# boxplots ------------------------------------------------------------

#photosyntheis
par(mar=c(5,5,1,1), mgp=c(2.5,1,0))
boxplot(photo ~ species, data=gas_exchange, ylab=photolab, xaxt='n', outline=FALSE, at=c(1:7, 9:12))
text(cex=1, x=c(1:7, 9:12), y=0, labels=specieslabs, xpd=TRUE, srt=90)
axis(1, at=c(1:7, 9:12), labels=FALSE)

#stomatal conductance
boxplot(cond ~ species, data=gas_exchange, ylab=condlab, xaxt='n', outline=FALSE,at=c(1:7, 9:12))
text(cex=1, x=c(1:7, 9:12), y=0, labels=specieslabs, xpd=TRUE, srt=90)
axis(1, at=c(1:7, 9:12), labels=FALSE)

#c:n ratio
boxplot(cn_ratio ~ species, data=chem, ylab="C:N", xaxt='n', outline=FALSE,
        at=c(1:7, 9:13), ylim=c(0, 40))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-10.5, labels=specieslabs2, xpd=TRUE, srt=90)

#nitrogen
boxplot(percN ~ species, data=chem, ylab="Leaf Nitrogen (%)", xaxt='n', outline=FALSE,
        at=c(1:7, 9:13), ylim=c(0, 5))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-13.5, labels=specieslabs2, xpd=TRUE, srt=90)

#phosphorus
boxplot(percP ~ species, data=chem, ylab="Leaf Phosphorus (%)", xaxt='n', outline=FALSE,
        at=c(1:7, 9:13), ylim=c(0, .5))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-.17, labels=specieslabs2, xpd=TRUE, srt=90)

#lma
boxplot(lma_µgpermm2 ~ species, data=stom, ylab=lmalab, 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 25))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-8, labels=specieslabs2, xpd=TRUE, srt=90)

#stomatal density
boxplot(stomatadensity_numbpermm2 ~ species, data=stom, ylab=denslab, 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 130))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-40, labels=specieslabs2, xpd=TRUE, srt=90)

#stomatal length
boxplot(length_um ~ species, data=stomsize2, ylab="Stomatal Length  (um)", 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 70))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-22, labels=specieslabs2, xpd=TRUE, srt=90)

#stomatal area
boxplot(stomarea_mm2 ~ species, data=stomsize2, ylab=stomarealab, 
        xaxt='n', outline=FALSE,at=c(1:7, 9:13), ylim=c(0, 2))
axis(1, at=c(1:7, 9:13), labels=FALSE)
text(cex=1, x=c(1:7, 9:13), y=-22, labels=specieslabs2, xpd=TRUE, srt=90)
