source("master_scripts/plot_objects.R")
##micro climate data for site conditions for selaginella

clim <- read.csv("raw_data/microclimate.csv", stringsAsFactors = FALSE)
  library(lubridate)
  clim$datetime <- mdy_hm(clim$datetime, tz='UTC')
  clim$site <- as.factor(clim$site)
  library(plantecophys)
  clim$vpd <- RHtoVPD(RH=clim$rh_perc, TdegC=clim$temp_C, Pa=101)
  
  
#max diff for discussion among habitats
# open_max_t <- max(clim[clim$site == "sel_umb", 4])
# open_max_vpd <- max(clim[clim$site == "sel_umb", 7])
# 
# open_cc_t <- max(clim[clim$site == "sel_anc", 4])
# open_cc_vpd <- max(clim[clim$site == "sel_anc", 7])
# 
# open_swamp_t <- max(clim[clim$site == "sel_oxa", 4])
# open_swamp_vpd <- max(clim[clim$site == "sel_oxa", 7])
# 
# tdiff <- (open_max_t - (open_cc_t+open_swamp_t)/2) / open_max_t
# vdiff <- (open_max_vpd - (open_cc_vpd+open_swamp_vpd)/2) / open_max_vpd

# plot objects ------------------------------------------------------------
startday <- min(clim$datetime)
enddate <- max(clim$datetime)
enddate2 <- as.POSIXct("2011-07-01 14:00:00", tz="UTC")
# xAT <- seq(from=startday,to=enddate, by="hour", tz="UTC")
xAT2 <- seq(from=startday,to=enddate2, length.out = 10, tz="UTC")

cols <- c("red3","gray15","cornflowerblue","forestgreen")
cols2 <- c("red3","cornflowerblue","forestgreen")
sites <- c("Open Canopy/Full Sun",  "Closed Canopy/Low Light","Swamp/Low Light")

# plot all data----------------------------------------------------------------
# windows(7,10)
# par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0),
#     lwd=2)
# 
# #1: temp
# par(mar=c(0,5,0,1))
# par(lwd=2)
# plot(temp_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(20, 32),
#      type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=templab)
# points(temp_C ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
# points(temp_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
# points(temp_C ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
# axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", labels=FALSE)
# legend("topright",col=cols,lty=1,legend=sites2,inset=.01,  bty='n',cex=1.1)
# 
# #2. dewpoint
# par(mar=c(0,5,0,1))
# plot(dewpoint_C ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(10, 30),
#      type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=dewlab)
# points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
# points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
# points(dewpoint_C ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
# axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", labels=FALSE)
# 
# #3. vpd
# par(mar=c(4,5,0,1))
# plot(vpd ~ datetime, data=clim[clim$site=="sel_anc",], ylim=c(0, 1.5),
#      type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=vpdlab, xaxt='n')
# points(vpd ~ datetime, data=clim[clim$site=="sel_eur",], col="gray15", type='l')
# points(vpd ~ datetime, data=clim[clim$site=="sel_oxa",], col="forestgreen", type='l')
# points(vpd ~ datetime, data=clim[clim$site=="sel_umb",], col="red3", type='l')
# axis.POSIXct(1, at=xAT2, format = "%H:%M:%S", las=3)

# plot one day ------------------------------------------------------------

friday <- clim[clim$datetime >= "2011-06-30 02:00:00" & clim$datetime <= "2011-06-30 18:00:00",]

startfri <- min(friday$datetime)
endfri <- max(friday$datetime)

startcex <- strptime("2011-06-30 05:45:00", tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
# enddate2 <- as.POSIXct("2011-07-01 14:00:00", tz="UTC")
xAT_fri <- seq(from=startfri,to=endfri, by="hour", tz="UTC")

#omit closed canopy-open (messed up humiity values)
#png(filename = "output/airvars.png", width = 11, height = 8.5, units = "in", res= 400)

# jpeg(filename = "output/manuscript_figures/Figure_1.jpeg", 
#      width = 6.8, height = 9, units = "in", res= 300)

setEPS()
postscript("output/manuscript_figures/Figure_1..eps")

# windows(7,10)
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0),
    lwd=2)

#1: temp
par(mar=c(0,5,0,1))
par(lwd=2)
plot(temp_C ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(22, 32),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=templab, xaxt='n')
  #points(temp_C ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(temp_C ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(temp_C ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", labels=FALSE)
legend("topright",col=cols2,lty=1,legend=sites,inset=.01,  bty='n',cex=1.25)
text("A", x=startcex, y= 31.5, cex=2)

#2. dewpoint
par(mar=c(0,5,0,1))
plot(dewpoint_C ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(20, 30),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=dewlab,xaxt='n')
  #points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(dewpoint_C ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", labels=FALSE)
text("B", x=startcex, y= 29.5, cex=2)

#3. vpd
par(mar=c(4,5,0,1))
plot(vpd ~ datetime, data=friday[friday$site=="sel_anc",], ylim=c(0, 0.5),
     type='l',col="cornflowerblue",lwd=2, lty=1,  xlab="", ylab=vpdlab, xaxt='n')
  #points(vpd ~ datetime, data=friday[friday$site=="sel_eur",], col="gray15", type='l')
  points(vpd ~ datetime, data=friday[friday$site=="sel_oxa",], col="forestgreen", type='l')
  points(vpd ~ datetime, data=friday[friday$site=="sel_umb",], col="red3", type='l')
axis.POSIXct(1, at=xAT_fri, format = "%H:%M:%S", las=3)
text("C", x=startcex, y= 0.46, cex=2)
#text was 1.25 and legend was 1.1 

# dev.copy2pdf(file= "output/airvars.pdf")
dev.off()
