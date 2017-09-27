# November 2013, JM Heberling (jmheberling@gmail.com)
# Modeling light response curves (aka A/Q or LRC)
# Model of Marshall & Biscoe (1980) J Exp Bot 31:29-39 

# See Lachapelle & Shipley (2012) Ann Bot 109: 1149-1157 for an alternative
# Amax can alternatively be estimated as Amax (max gross photo) = Asat (Anet at saturating light) + Rd (Anet at PAR = 0)
#---

 # Read in text file from Licor 6400
lrc<- read.csv("sample_lrc.txt",sep="",skip=16)

lrc<na.omit(lrc) #remove lines with remarks (*check notebook/file for comments)


# ---Inspect and graph raw data (A vs. PPFD) ---

PARlrc<-lrc$PARi #PAR (aka PPFD or Q)
photolrc<-lrc$Photo #net photosynthetic rate (Anet)

curvelrc<-data.frame(PARlrc,photolrc)
curvelrc # *inspect raw data and check notebook (data reasonable or need edited/discarded?)

par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=1.5)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2.5,cex=1.5)

# --- Nonlinear least squares regression (non-rectangular hyperbola)
# 4 parameter model: Amax (max gross photosytnthetic rate), Rd (dark respiration), 
#                    AQY (apparent quantum yield), Theta (curvature parameter, dimensionless) ---
# Another option is to fit AQY (initial slope) and Rd (y-intercept) separately 
# using linear regression on data points that are not light-saturated, 
# then use those model fits in the non-linear model to parameterize Amax and the curve parameter (theta). 
# However, this requires user to subjectively decide which points are not light saturated (initial linear portion of curve). 
# For more or Rd estimation see protocol text. 
# Depending on data, quantile regression can be implemented through nlrq()

curve.nlslrc = nls(photolrc ~ (1/(2*theta))*(AQY*PARlrc+Am-sqrt((AQY*PARlrc+Am)^2-4*AQY*theta*Am*PARlrc))
                              -Rd,start=list(Am=(max(photolrc)-min(photolrc)),
                              AQY=0.05,Rd=-min(photolrc),theta=1)) 

summary(curve.nlslrc) #summary of model fit

# ---Graph raw data with modeled curve---

par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
plot(PARlrc,photolrc,xlab="", ylab="", ylim=c(-2,max(photolrc)+2),cex.lab=1.2,cex.axis=1.5,cex=2)
mtext(expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")"),side=1,line=3.3,cex=2)
mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2,cex=2)
curve((1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1],lwd=2,col="blue",add=T)

# ---Solve for light compensation point (LCPT), PPFD where Anet=0 ---
x<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]
                *x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]
                *summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))
                -summary(curve.nlslrc)$coef[3,1]}

uniroot(x,c(0,50))$root #LCPT

# ---Solve for light saturation point (LSP), PPFD where 75% of Amax is achieved (75% is arbitrary - cutoff could be changed)
x<-function(x) {(1/(2*summary(curve.nlslrc)$coef[4,1]))*(summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1]-(0.75*summary(curve.nlslrc)$coef[1,1])+0.75*(summary(curve.nlslrc)$coef[3,1])}

uniroot(x,c(0,1000))$root #LSP 

