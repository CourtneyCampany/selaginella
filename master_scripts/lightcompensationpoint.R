source("master_scripts/plot_objects.R")
#calculate light saturation point for selaginella
library(plyr)
library(magrittr)

lrc_sela <- read.csv("raw_data/selaginella_lightcurve.csv")
  lrc_sela$id <- paste(lrc_sela$species, lrc_sela$sample, sep="-")
  
lrc_fern <- read.csv("raw_data/fern_lightcurve.csv")
  lrc_fern$id <- paste(lrc_fern$species, lrc_fern$sample, sep="-")


# lcp function ------------------------------------------------------------
lcp_func <- function(x) {
  linearpts <- x[x$PARi <= 15,]
  lcp_mod <- lm(Photo ~ PARi, data=linearpts)
  lcp <- (-coef(lcp_mod)[1])/coef(lcp_mod)[2]
  return(lcp)
}

# calculate lcp -----------------------------------------------------------

#sela
curves_sela <- split(lrc_sela, lrc_sela$id)
  lcp_sela <- ldply(curves_sela, lcp_func %>% as.data.frame  %>% rbind.fill)
  names(lcp_sela)[1:2] <- c("id", "lcp")
  lcp_sela$species <- gsub("-([0-9])", "", lcp_sela$id)
  lcp_sela$family <- "Selaginella"

#fern
curves_fern<- split(lrc_fern, lrc_fern$id)
lcp_fern <- ldply(curves_fern, lcp_func %>% as.data.frame  %>% rbind.fill)
  names(lcp_fern)[1:2] <- c("id", "lcp")
  lcp_fern$species <- gsub("-([0-9])", "", lcp_fern$id)
  lcp_fern$family <- "Ferns"

# plot lcp ----------------------------------------------------------------
lcp_family <- rbind(lcp_sela, lcp_fern)
  lcp_family$species <- factor(lcp_family$species, levels=c("sel_anc","sel_art","sel_ate","sel_eur",
                                                            "sel_oax","sel_swi","sel_umb",
                                                            "cyc_sem","dip_str"))
  
lrclabs <- c("sel_anc","sel_art","sel_ate","sel_eur","sel_oax","sel_swi","sel_umb","cyc_sem","dip_str")

par(mar=c(5,4,1,1), mgp=c(2.5,1,0))
boxplot(lcp~ species, data=lcp_family, outline=FALSE, ylim=c(-1, 15), at=c(1:7, 9:10), 
        ylab= lcplab, xaxt='n')
axis(1, at=c(1:7, 9:12), labels=FALSE)
text(cex=1, x=c(1:7, 9:10), y=-3.5, labels=lrclabs,xpd=TRUE, srt=60)

dev.off()

#inspects all lcp plots-----------
pdf("output/lcp_sela_plots.pdf", width = 7, height = 7)
l_ply(curves_sela,  function(x){ 
  y <- x[x$PARi <= 15,]
  lcp_mod <- lm(Photo ~ PARi, data=y)
  par(mar=c(5,5,1,1)) 
  plot(y$PARi,y$Photo, ylim=c(-2,4),cex=1.5,xlim=c(0,25),
       xlab=parlab, ylab=anetlab)
  abline(lcp_mod)
  abline(h=0)
  abline(v=0)}, .print=TRUE)
dev.off()

pdf("output/lcp_fern_plots.pdf", width = 7, height = 7)
l_ply(curves_fern,  function(x){ 
  y <- x[x$PARi <= 15,]
  lcp_mod <- lm(Photo ~ PARi, data=y)
  par(mar=c(5,5,1,1)) 
  plot(y$PARi,y$Photo, ylim=c(-2,4),cex=1.5,xlim=c(0,25),
       xlab=parlab, ylab=anetlab)
  abline(lcp_mod)
  abline(h=0)
  abline(v=0)}, .print=TRUE)
dev.off()
