source("master_scripts/plot_objects.R")
# testing fitting of light response curves for seliganella

lrc <- read.csv("raw_data/selaginella_lightcurve.csv")
  lrc$id <- paste(lrc$species, lrc$sample, sep="-")

# test <- lrc[lrc$species == "sel_art" & lrc$sample == "2",]
# PARlrc<-test$PARi
# photolrc<-test$Photo
#curvelrc<-data.frame(PARlrc,photolrc)
  # par(mar=c(5,5,1,1))
  # plot(PARlrc,photolrc, ylim=c(-2,max(photolrc)+2),cex=1.5,xlab=parlab,
  #      ylab=anetlab)

parphoto_func <- function(x){
  PARlrc<-x$PARi 
  photolrc<-x$Photo 
  curvelrc<-data.frame(PARlrc,photolrc)
}

curves_ls <- split(lrc, lrc$id)
curves2_ls <- lapply(curves_ls, parphoto_func)

#inspects all lrc plots
library(plyr)
pdf("plots.pdf", width = 7, height = 7)
l_ply(curves2_ls,  function(x){ par(mar=c(5,5,1,1)) 
  plot(x$PARlrc,x$photolrc, ylim=c(-2,10),cex=1.5,xlim=c(0,1000),
      xlab=parlab, ylab=anetlab) }, .print=TRUE)
dev.off()


#now we have a list of light response curves to send to function....

##fit nls with 4 parameters (theta, aqy, rd, amax (amax and rd taken from data)
curve.nlslrc <- nls(photolrc ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2
                               -4*AQY*theta*Am*PARlrc))-Rd,
                               start=list(Am=(max(photolrc)-min(photolrc)),
                               AQY=0.05,Rd=-min(photolrc),theta=1)) 

summary(curve.nlslrc)

lrcnls_func <- function(x){nls(x$photolrc ~ (1/(2*theta))*(AQY*x$PARlrc+Am
                          -sqrt((AQY*x$PARlrc+Am)^2-4*AQY*theta*Am*x$PARlrc))-Rd,
                          start=list(Am=(max(x$photolrc)-min(x$photolrc)),
                          AQY=0.05,Rd=-min(x$photolrc),theta=1)) 
                          }

fit <- lrcnls_func(curvelrc)
summary(fit)
#testing works, now get it to work for most curves

lrc_fits <- lapply(curves2_ls[30], lrcnls_func)
summary(lrc_fits[[1]])
#this works 

# test curve fits ---------------------------------------------------------


# calculate lsp -----------------------------------------------------------
#subset of list (29:35)
lrc_fits <- lapply(curves2_ls[29:35], lrcnls_func)
onecurve <- lrc_fits[[1]]

lsp_func<-function(x) {(1/(2*summary(onecurve)$coef[4,1]))*
    (summary(onecurve)$coef[2,1]*x+summary(onecurve)$coef[1,1]-
    sqrt((summary(onecurve)$coef[2,1]*x+summary(onecurve)$coef[1,1])^2-4*
    summary(onecurve)$coef[2,1]*summary(onecurve)$coef[4,1]*
    summary(onecurve)$coef[1,1]*x))-summary(onecurve)$coef[3,1]-
    (0.75*summary(onecurve)$coef[1,1])+0.75*(summary(onecurve)$coef[3,1])}

uniroot(lsp_func,c(0,500))$root #LCPT

# calculate lcp -----------------------------------------------------------
lrc_fits <- lapply(curves2_ls[29:35], lrcnls_func)
onecurve <- lrc_fits[[1]]

lcpt_func<-function(x) {(1/(2*summary(onecurve)$coef[4,1]))*(summary(onecurve)$coef[2,1]
                        *x+summary(onecurve)$coef[1,1]-sqrt((summary(onecurve)$coef[2,1]
                        *x+summary(onecurve)$coef[1,1])^2-4*summary(onecurve)$coef[2,1]
                        *summary(onecurve)$coef[4,1]*summary(onecurve)$coef[1,1]*x))
                        -summary(onecurve)$coef[3,1]}

uniroot(lcpt_func,c(0,1000))$root #LCPT




##curve fit w/ points--------------------------------------------------------------

plot(PARlrc,photolrc,cex=1.5,xlab=parlab, ylab=anetlab, ylim=c(-2,max(photolrc)+2))
#plot curve using nls functions
curve((1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
      *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
      *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
      *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
      -summary(curve.nlslrc)$coef[3,1],lwd=2,col="blue",add=T)


# ---Solve for light compensation point (LCPT), PPFD where Anet=0 ----------------
lcpt_func<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
                *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
                -summary(curve.nlslrc)$coef[3,1]}

uniroot(lcpt_func,c(-50,50))$root #LCPT

# ---Solve for light saturation point (LSP), PPFD where 75% of Amax is achieved 
#    (75% is arbitrary - cutoff could be changed)
lsp_func<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*
    (summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-
    sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*
    summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*
    summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1]-
    (0.75*summary(curve.nlslrc)$coef[1,1])+0.75*(summary(curve.nlslrc)$coef[3,1])}


lsp <- uniroot(lsp_func,c(0,1000))$root #LSP 


