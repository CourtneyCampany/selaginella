source("master_scripts/plot_objects.R")
#calculate light saturation point for selaginella
library(plyr)
library(magrittr)

lrc <- read.csv("raw_data/selaginella_lightcurve.csv")
  lrc$id <- paste(lrc$species, lrc$sample, sep="-")


# lcp function ------------------------------------------------------------
lcp_func <- function(x) {
  linearpts <- x[x$PARi <= 15,]
  lcp_mod <- lm(Photo ~ PARi, data=linearpts)
  lcp <- (-coef(lcp_mod)[1])/coef(lcp_mod)[2]
  return(lcp)
}

# calculate lcp -----------------------------------------------------------

#use raw data to calculate lcp for all lrc
curves_ls <- split(lrc, lrc$id)
lcp_sela <- ldply(curves_ls, lcp_func %>% as.data.frame  %>% rbind.fill)
names(lcp_sela)[1:2] <- c("id", "lcp")
lcp_sela$species <- gsub("-([0-9])", "", lcp_sela$id)


# plot lcp ----------------------------------------------------------------

par(mar=c(5,4,1,1), mgp=c(2.5,1,0))
boxplot(lcp~ species, data=lcp_sela, outline=FALSE, ylim=c(-.5, 15),
        ylab=lcplab, xaxt='n')
text(cex=1, x=c(1:7), y=-3.5, labels=unique(lcp_sela$species), 
     xpd=TRUE, srt=45)
axis(1, at=c(1:7, 9:12), labels=FALSE)
dev.off()

#inspects all lcp plots-----------
pdf("lcpplots.pdf", width = 7, height = 7)
l_ply(curves_ls,  function(x){ 
  y <- x[x$PARi <= 15,]
  lcp_mod <- lm(Photo ~ PARi, data=y)
  par(mar=c(5,5,1,1)) 
  plot(y$PARi,y$Photo, ylim=c(-2,4),cex=1.5,xlim=c(0,25),
       xlab=parlab, ylab=anetlab)
  abline(lcp_mod)
  abline(h=0)
  abline(v=0)}, .print=TRUE)
dev.off()
