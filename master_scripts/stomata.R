source("master_scripts/plot_objects.R")
#stomata density and gas exchange

stom <- read.csv("raw_data/leaf_anatomy.csv")
cond <- read.csv("raw_data/gas_exchange.csv")


stomcond <- droplevels(merge(stom, cond, all = TRUE))
stomcond2 <- stomcond[complete.cases(stomcond),]

# plotting ----------------------------------------------------------------
stomcond_mod<- lm(cond ~ stomatadensity_numbpermm2 ,data=stomcond2)
stomphoto_mod<- lm(photo ~ stomatadensity_numbpermm2 ,data=stomcond2)

familycols <- c("cornflowerblue", "forestgreen")

plot(cond ~ stomatadensity_numbpermm2, col=familycols[family], data=stomcond2, pch = 16,
     ylim=c(0, .3), xlim=c(0,150))
legend("topright", legend=c("Ferns", "Selaginella"), col=familycols,
       pch=16, bty='n', inset=.01)

plot(photo ~ stomatadensity_numbpermm2, col=familycols[family], data=stomcond2, pch = 16,
     ylim=c(0,9), xlim=c(0, 120))
legend("topleft", legend=c("Ferns", "Selaginella"), col=familycols,
       pch=16, bty='n', inset=.01)

