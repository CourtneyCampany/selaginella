source("master_scripts/plot_objects.R")
#stomata density and gas exchange

stom <- read.csv("raw_data/leaf_anatomy.csv")
treat <- read.csv("raw_data/treatments.csv")
  stom <- merge(stom, treat)

cond <- read.csv("raw_data/gas_exchange_selaginella.csv")


stomcond <- droplevels(merge(stom, cond, all = TRUE))


# plotting ----------------------------------------------------------------


plot(cond ~ stomataldensity_controlarea, col=trtcols2[habitat], data=stomcond, pch = 16, 
     ylim=c(0, .25), xlim=c(0, 550))
legend("topleft", trtlab, pch=16, col=palette(), bty='n', inset=.01)



plot(cond ~ stomatadensity_numbpermm2, col=trtcols[habitat], data=stomcond, pch = 16, 
     ylim=c(0, .25), xlim=c(0, 160))
legend("topright", trtlab, pch=16, col=palette(), bty='n', inset=.01)

plot(cond ~ stomatalarea_mm2, col=trtcols[habitat], data=stomcond, pch = 16, 
     ylim=c(0, .25), xlim=c(0, .8))


stomcond$habitat <- factor(amass_chem$habitat, levels=c("full_sun","understory_midlight",
                                                          "understory_lowlight","swamp_lowlight"))

boxplot(stomataldensity_controlarea ~ habitat, data=stomcond, col=trtcols)
boxplot(stomatadensity_numbpermm2 ~ habitat, data=stomcond, col=trtcols)
boxplot(stomatalarea_mm2 ~ habitat, data=stomcond, col=trtcols)
