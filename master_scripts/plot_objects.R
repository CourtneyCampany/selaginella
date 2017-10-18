#plot objects
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
trmmollab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))
cilab <- expression(italic(C)[i]~~(ppm))
# specieslab <- expression(italic(Bolbitis~portoricensis), italic())


templab <- expression(Temperature~~(degree*C))
dewlab <- expression(Dew~Point~~(degree*C))
vpdlab <- expression(VPD~~(kPa))

parlab <- expression("PPFD ("*mu*"mol photons "*m^-2*s^-1*")")
anetlab <- expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")")

#treatment colors
gradient <- colorRampPalette(c("orange", "forestgreen"))
palette(gradient(4))
trtcols <- palette(gradient(4))
trtlab <- c("full_sun","understory_midlight","understory_lowlight","swamp_lowlight")
trtcols2 <- c(trtcols[1], trtcols[4], trtcols[3], trtcols[2])


# standard error function--------------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))