source("functions.R")
#Here we plot functional traits of sela vs larger fern dataset

fern_cr <- read.csv("raw_data/ferns_costa_rundel.csv")

sela_chem <- read.csv("raw_data/leaf_chemistry.csv")
sela_stom <- read.csv("raw_data/leaf_anatomy.csv")


#need selaginella means
sela_chem_agg <- doBy::summaryBy(. ~ family + species, data=sela_chem, FUN=c(mean, se))
sela_stom_agg <-doBy::summaryBy(. ~ family + species, data=sela_stom, FUN=c(mean, se))

sela_agg <- merge(sela_chem_agg[,c(1:2, 5, 12)], sela_stom_agg[,c(1:2, 4,9)])


famcols <- c("forestgreen", "cornflowerblue")
plot(percN.mean ~ lma_µgpermm2.mean, data=sela_agg, xlim=c(0, 55), ylim=c(0, 6.5), 
     pch=16,col=famcols[family])
points((percN_mgg.1)/10 ~ lma_gcm.2, data=fern_cr[fern_cr$habitat=="terrestrial",],
       pch=16, col="black")
