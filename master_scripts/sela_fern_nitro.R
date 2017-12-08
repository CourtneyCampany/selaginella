source("functions.R")
source("master_scripts/plot_objects.R")
library(scales)
#Here we plot functional traits of sela vs larger fern dataset

fern_cr <- read.csv("raw_data/ferns_costa_rundel.csv")

sela_chem <- read.csv("raw_data/leaf_chemistry.csv")
sela_stom <- read.csv("raw_data/leaf_anatomy.csv")


#need selaginella means
sela_chem_agg <- doBy::summaryBy(. ~ family + species, data=sela_chem, FUN=c(mean, se))
sela_stom_agg <-doBy::summaryBy(. ~ family + species, data=sela_stom, FUN=c(mean, se))

#merge selaginella data sets
sela_agg <- merge(sela_chem_agg[,c(1:2, 5, 12)], sela_stom_agg[,c(1:2, 6,12)])


# plotting ----------------------------------------------------------------
terr <- fern_cr[fern_cr$habitat=="terrestrial" | fern_cr$habitat == "hemi-epiphyte",]

png(filename = "output/lma_nitro.png", width = 11, height = 8.5, units = "in", res= 400)
#windows(7,7)
par(mar=c(5,5,2,2), las=1,cex.axis=0.8,)
plot(percN.mean ~ lma_gpercm2.mean, data=sela_agg, xlim=c(0, 55), ylim=c(0, 6.5), 
    col=famcols[family], xlab=lmalab, ylab="Leaf Nitrogen (%)", type='n')
with(sela_agg, arrows(lma_gpercm2.mean, percN.mean, lma_gpercm2.mean, 
                      percN.mean+percN.se,col=famcols[family],
                      angle=90, length=0.03, cex=1.5))
with(sela_agg, arrows(lma_gpercm2.mean, percN.mean, lma_gpercm2.mean, 
                      percN.mean-percN.se,col=famcols[family],
                      angle=90, length=0.03, cex=1.5))
points(percN.mean ~ lma_gpercm2.mean, data=sela_agg,pch=16,
        col=famcols[family], cex=1.5)

with(sela_agg, arrows(lma_gpercm2.mean, percN.mean, lma_gpercm2.mean+lma_gpercm2.se, 
                      percN.mean,col=famcols[family],
                      angle=90, length=0.03, cex=1.5))
with(sela_agg, arrows(lma_gpercm2.mean, percN.mean, lma_gpercm2.mean-lma_gpercm2.se, 
                      percN.mean,col=famcols[family],
                      angle=90, length=0.03, cex=1.5))

points((percN_mgperg)/10 ~ lma_gpercm, data=terr,pch=21, bg=famcols[1], cex=1.5)

legend("topleft", legend=c("Selaginella","Ferns", "Ferns-Watkins et al. 2007"), 
       col=c(familycols[2],familycols[1],"black"),pt.bg=familycols[1],
       pch=c(16, 16, 21), bty='n', inset=.01, cex=.9)

# dev.copy2pdf(file= "output/fern_sela_survey.pdf")
dev.off()

