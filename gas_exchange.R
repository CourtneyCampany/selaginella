source("master_scripts/plot_objects.R")
#gas exchange date for seligenalla and other ferns

photo <- read.csv("raw_data/gas_exchange.csv")

photo2 <- read.csv("raw_data/leaf_gasexchange.csv")
photo2_low <- photo2[photo2$light_umol == 50,]

# plotting ----------------------------------------------------------------

#raw data from low light (50umols)
boxplot(photo ~ family, data=photo2_low)
boxplot(cond ~ family, data=photo2_low)
boxplot(photo/cond ~ family, data=photo2_low)
#selaginella lower photo and cond than ferns

sela <- droplevels(photo2_low[photo2_low$family == "Selaginella",])

# windows(12,6)

windows(4,6)
par(mfrow=c(2,1), las=1, cex.axis=1, cex.lab=1, mgp=c(3.5,1,0))

par(mar=c(0,5,2,1))
boxplot(photo ~ species, sela,ylab=photolab, names=FALSE, 
        outline=FALSE, ylim=c(0,8), xaxt="n")
text(x=5.5, y=8, "(a)", cex=1)

par(mar=c(5,5,0,1))
boxplot(cond~ species, sela, ylab=condlab, ylim=c(0, .15),
        outline=FALSE,xaxt="n")
axis(1,at=1:5, labels=unique(sela$species), las=3)
text(x=5.5, y=.15, "(b)", cex=1, las=3)

# par(mar=c(5,5,0,1))
# boxplot(Ci ~ Species, photo, ylab=cilab, outline=FALSE, ylim=c(0,500))
# text(x=7.5, y=475, "(c)", cex=1.5)