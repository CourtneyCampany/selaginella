library(scales)
#plot objects
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
amasslab <- expression(italic(A)[mass]~~(n*mol~g^-1~s^-1))
trmmollab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))
cilab <- expression(italic(C)[i]~~(ppm))
# specieslab <- expression(italic(Bolbitis~portoricensis), italic())


templab <- expression(Temperature~~(degree*C))
dewlab <- expression(Dew~Point~~(degree*C))
vpdlab <- expression(VPD~~(kPa))

parlab <- expression(PPFD~~(mu*mol~photons~m^-2~s^-1))
lcplab <- expression(LCP~~(mu*mol~photons~m^-2~s^-1))
anetlab <- expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")")

denslab <- expression(Stomatal~Density~~(mm^-2))
stomarealab <- expression(Stomatal~Area~~(mm^2))
stomlengthlab <- expression(Stomatal~Length~~(mu*m^2))

lmalab <- expression(LMA~~(g~m^-2))
nuelab <- expression(atop(PNUE,
                          (mu*mol~CO[2]~g~N^-1~s^-1)))

nuelab2 <- expression(PNUE~~(mu*mol~CO[2]~g~N^-1~s^-1))

puelab <- expression(atop(PPUE, 
                          (mu*mol~CO[2]~g~P^-1~s^-1)))

nmasslab<- expression(Leaf~Nitrogen[mass]~~(mg~g^-1))
narealab <- expression(Leaf~Nitrogen[area]~~(mg~m^-2))

#treatment colors
gradient <- colorRampPalette(c("orange", "darkgreen"))
palette(gradient(3))
trtcols <- palette(gradient(3))
trtlab <- c("Open canopy","Closed canopy","Swamp")
#levels in alpha order, 1,3,2
trtcols1 <- c(trtcols[1],trtcols[3],trtcols[2])
trtcols2 <- c(alpha(trtcols[1], .85), alpha(trtcols[3], .85), alpha(trtcols[2], .85))

familycols <- c("cornflowerblue","forestgreen")
famcols <- c(alpha("cornflowerblue",.85),alpha("forestgreen",.85))

# standard error function--------------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))
