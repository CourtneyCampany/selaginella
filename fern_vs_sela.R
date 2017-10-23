source("master_scripts/plot_objects.R")
library(doBy)

##sela vs ferns

sumfun <- function(x, ...){
  c(m=mean(x, ...),l=length(x))
}

se <- function(x) sd(x)/sqrt(length(na.omit(x)))


gas_exchange <- read.csv("raw_data/gas_exchange.csv")
#order species for plotting
gas_exchange$species <- factor(gas_exchange$species, levels=c("Sel_anc","Sel_art","Sel_ate",
                                                        "Sel_eur","Sel_oax","Sel_swim","Sel_umb",
                                                        "Bul_port","Cyc_semi","Dip_stri","Thy_cur"))


photo_family <-summaryBy(. ~ family, data=gas_exchange, FUN=c(mean,se))

chem <- read.csv("raw_data/leaf_chemistry.csv")
chem2 <- droplevels(chem[, -c(3,4)])
chem_family <-summaryBy(. ~ family, data=chem2, FUN=se)

#this starts the combined dataset for ferns/sela with stomatal tratis
stom <- read.csv("raw_data/leaf_anatomy.csv")
stom_family <-summaryBy(. ~ family, data=stom, FUN=c(mean,se))
  
library(plyr)
chem_agg <- ddply(chem2, "family", summarise,
               N    = sum(!is.na(percP)),
               mean = mean(percP, na.rm=TRUE),
               sd   = sd(percP, na.rm=TRUE),
               se   = sd / sqrt(N)
)



# boxplots ----------------------------------------------------------------
par(mar=c(5,4,1,1), mgp=c(2.5,1,0))
boxplot(photo ~ species, data=gas_exchange, ylab=photolab, xaxt='n', outline=FALSE, at=c(1:7, 9:12))
text(cex=1, x=c(1:7, 9:12), y=0, labels=unique(gas_exchange$species), xpd=TRUE, srt=45)
axis(1, at=c(1:7, 9:12), labels=FALSE)

boxplot(cond ~ species, data=gas_exchange, ylab=condlab, xaxt='n', outline=FALSE,at=c(1:7, 9:12))
text(cex=1, x=c(1:7, 9:12), y=0, labels=unique(gas_exchange$species), xpd=TRUE, srt=45)
axis(1, at=c(1:7, 9:12), labels=FALSE)


#make a means dataframe and a series of boxplots with raw data
