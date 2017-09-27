source("plot_objects.R")
# testing fitting of light response curves for seliganella

lrc <- read.csv("raw_data/selaginella_lightcurve.csv")

test <- lrc[lrc$species == "sel_art" & lrc$sample == "2",]


PARlrc<-lrc$PARi 
photolrc<-lrc$Photo 

curvelrc<-data.frame(PARlrc,photolrc)


par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc, ylim=c(-2,max(photolrc)+2),cex=2,xlab=parlab, ylab=anetlab)


curve.nlslrc = nls(photolrc ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))
                              -Rd,start=list(Am=(max(photolrc)-min(photolrc)),
                              AQY=0.05,Rd=-min(photolrc),theta=1)) 

summary(curve.nlslrc)

##curve fit w/ points

plot(PARlrc,photolrc,cex=2,xlab=parlab, ylab=anetlab, ylim=c(-2,max(photolrc)+2),
     cex.lab=1.2,cex.axis=1.5,cex=2)

curve((1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
      *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
      *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
      *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
      -summary(curve.nlslrc)$coef[3,1],lwd=2,col="blue",add=T)


# ---Solve for light compensation point (LCPT), PPFD where Anet=0 ---
lcpt_func<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
                *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
                -summary(curve.nlslrc)$coef[3,1]}

lcpt <- uniroot(lcpt_func,c(0,50))$root #LCPT

# ---Solve for light saturation point (LSP), PPFD where 75% of Amax is achieved 
#    (75% is arbitrary - cutoff could be changed)
lsp_func<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
                *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
                -summary(curve.nlslrc)$coef[3,1]-(0.75*summary(curve.nlslrc)$coef[1,1])+0.75
  *(summary(curve.nlslrc)$coef[3,1])}

lsp <- uniroot(lsp_func,c(0,1000))$root #LSP 


