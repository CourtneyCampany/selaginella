source("master_scripts/plot_objects.R")
##micro climate data for site conditions for selaginella

clim <- read.csv("raw_data/microclimate.csv", stringsAsFactors = FALSE)
  library(lubridate)
  clim$datetime <- mdy_hm(clim$datetime, tz='UTC')
  clim$site <- as.factor(clim$site)
  library(plantecophys)
  clim$vpd <- RHtoVPD(RH=clim$rh_perc, TdegC=clim$temp_C, Pa=101)


# plot objects ------------------------------------------------------------
startday <- min(clim$datetime)
enddate <- max(clim$datetime)
enddate2 <- as.POSIXct("2011-07-01 14:00:00", tz="UTC")
# xAT <- seq(from=startday,to=enddate, by="hour", tz="UTC")
xAT2 <- seq(from=startday,to=enddate2, length.out = 10, tz="UTC")

cols <- c("red3","gray15","cornflowerblue","forestgreen")
sites <- c("Open Canopy/Full Sun", "Closed Canopy/Mid Light", "Closed Canopy/Low Light","Swamp/Low Light")

# plot all data----------------------------------------------------------------
windows(7,10)
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0),
    lwd=2)

#1: temp
par(mar=c(0,5,0,1))
par(lwd=2)
plot(temp_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(20, 32),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=templab)
points(temp_C ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
points(temp_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
points(temp_C ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", labels=FALSE)
legend("topright",col=cols,lty=1,legend=sites,inset=.01,  bty='n',cex=1.1)

#2. dewpoint
par(mar=c(0,5,0,1))
plot(dewpoint_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(10, 30),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=dewlab)
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", labels=FALSE)

#3. vpd
par(mar=c(4,5,0,1))
plot(vpd ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(0, 1.5),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=vpdlab, xaxt='n')
points(vpd ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
points(vpd ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
points(vpd ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", las=3)


# plot one day ------------------------------------------------------------

friday <- clim[clim$datetime >= "2011-06-30 02:00:00" & clim$datetime <= "2011-06-30 18:00:00",]

startfri <- min(friday$datetime)
endfri <- max(friday$datetime)
# enddate2 <- as.POSIXct("2011-07-01 14:00:00", tz="UTC")
xAT_fri <- seq(from=startfri,to=endfri, by="hour", tz="UTC")

windows(7,10)
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0),
    lwd=2)

#1: temp
par(mar=c(0,5,0,1))
par(lwd=2)
plot(temp_C ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(22, 32),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=templab, xaxt='n')
  points(temp_C ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(temp_C ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(temp_C ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", labels=FALSE)
legend("topright",col=cols,lty=1,legend=sites,inset=.01,  bty='n',cex=1.1)

#2. dewpoint
par(mar=c(0,5,0,1))
plot(dewpoint_C ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(20, 30),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=dewlab,xaxt='n')
  points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", labels=FALSE)

#3. vpd
par(mar=c(4,5,0,1))
plot(vpd ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(0, 0.5),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=vpdlab, xaxt='n')
  points(vpd ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(vpd ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(vpd ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", las=3)

