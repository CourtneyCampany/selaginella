##micro climate data for site conditions for selaginella

clim <- read.csv("raw_data/microclimate.csv", stringsAsFactors = FALSE)
library(lubridate)
clim$datetime <- mdy_hm(clim$datetime)
clim$site <- as.factor(clim$site)
library(plantecophys)
clim$vpd <- RHtoVPD(RH=clim$rh_perc, TdegC=clim$temp_C, Pa=101)


# plot objects ------------------------------------------------------------

cols <- c("red3","cornflowerblue","gray15","forestgreen")

startday <- min(clim$datetime)
enddate <- max(clim$datetime)
xAT <- seq(from=startday,to=enddate, by="hour")
sites <- unique(clim$site)

# plotting ----------------------------------------------------------------
windows(7,10)
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0),
    lwd=2)

#1: temp
par(mar=c(0,5,0,1))
par(lwd=2)
plot(temp_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(20, 32),
     type='l',col="red3",lwd=2, lty=1,  xlab="", ylab=templab)
points(temp_C ~ datetime, data=clim[clim$site=="sel_eur",], col="cornflowerblue", type='l')
points(temp_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="gray15", type='l')
points(temp_C ~ datetime, data=clim[clim$site=="sel_umb",], col="forestgreen", type='l')
axis.POSIXct(1, at=xAT, format = "%H:%M:%S", label=FALSE)
legend("topright",col=cols,lty=1,legend=sites,inset=.01,  bty='n',cex=1.1)

#2. dewpoint
par(mar=c(0,5,0,1))
plot(dewpoint_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(10, 30),
     type='l',col="red3",lwd=2, lty=1,  xlab="", ylab=dewlab)
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_eur",], col="cornflowerblue", type='l')
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="gray15", type='l')
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_umb",], col="forestgreen", type='l')
axis.POSIXct(1, at=xAT, format = "%H:%M:%S", label=FALSE)

#3. vpd
par(mar=c(0,5,0,1))
plot(vpd ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(0, 1.5),
     type='l',col="red3",lwd=2, lty=1,  xlab="", ylab=vpdlab)
points(vpd ~ datetime, data=clim[clim$site=="sel_eur",], col="cornflowerblue", type='l')
points(vpd ~ datetime, data=clim[clim$site=="sel_oxa",], col="gray15", type='l')
points(vpd ~ datetime, data=clim[clim$site=="sel_umb",], col="forestgreen", type='l')
axis.POSIXct(1, at=xAT, format = "%H:%M:%S")

