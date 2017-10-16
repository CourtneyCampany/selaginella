#stomata density and gas exchange

stom <- read.csv("raw_data/leaf_anatomy.csv")
cond <- read.csv("raw_data/gas_exchange_selaginella.csv")


stom_sela <- stom[stom$family == "Selaginella",]
sela <- droplevels(merge(stom_sela, cond, all = TRUE))


plot(cond ~ stomataldensity_controlarea, col=species, data=sela, pch = 16, ylim=c(0, .25), xlim=c(0, 550))
plot(cond ~ stomatadensity_numbpermm2, col=species, data=sela, pch = 16, ylim=c(0, .25), xlim=c(0, 200))
plot(cond ~ stomatalarea_mm2, col=species, data=sela, pch = 16, ylim=c(0, .25), xlim=c(0, .8))



boxplot(stomataldensity_controlarea ~ species, data=sela)
