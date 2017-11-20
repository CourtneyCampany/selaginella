#this script plots chlorphyll content and leaf N

chem <- read.csv("raw_data/leaf_chemistry.csv")
chem_sela <- chem[chem$family == "Selaginella",]

chem_agg <- doBy::summaryBy(chlorophyll_mgperl+percN ~ species + family, 
                            data=chem, FUN=mean, keep.names=TRUE)


plot(chlorophyll_mgperl ~ percN, data=chem_sela, col=species, pch=16,ylim=c(0, 25), xlim=c(0,5))


chlnitro_mod <- lm(chlorophyll_mgperl ~ percN, data=chem_sela)
summary(chlnitro_mod)
#not significant
