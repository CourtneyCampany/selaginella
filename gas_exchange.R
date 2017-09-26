#gas exchange date for seligenalla and other ferns

photo <- read.csv("raw_data/gas_exchange.csv")


# plot objects ------------------------------------------------------------




# plotting ----------------------------------------------------------------
# windows(12,6)

windows(7,10)
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0))

par(mar=c(0,5,2,1))
boxplot(Photo ~ Species, photo,ylab=photolab, names=FALSE, 
        outline=FALSE, ylim=c(0,8), xaxt="n")
text(x=7.5, y=7.5, "(a)", cex=1.5)

par(mar=c(0,5,0,1))
boxplot(Cond~ Species, photo, ylab=condlab, names=FALSE, ylim=c(0, .15),
        outline=FALSE, xaxt="n")
text(x=7.5, y=.14, "(b)", cex=1.5)

par(mar=c(5,5,0,1))
boxplot(Ci ~ Species, photo, ylab=cilab, outline=FALSE, ylim=c(0,500))
text(x=7.5, y=475, "(c)", cex=1.5)