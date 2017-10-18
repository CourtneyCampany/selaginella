##this script looks at leaf chemistry

chem <- read.csv("raw_data/leaf_chemistry.csv")
  chem$nmass <- with(chem, mass_ug * percN)
  chem$pmass <- with(chem, mass_ug * percP)

library(doBy)
chem_agg <- summaryBy(. ~ family + species, data=chem , FUN=mean, keep.names = TRUE)
chem_agg2 <- chem_agg[,c(1:2, 4:8, 10:11)]
write.csv(chem_agg2, "calculated_data/leaf_chem_means.csv", row.names = FALSE)

# plotting ----------------------------------------------------------------    
   
boxplot(percN ~ family, data=chem)
boxplot(cn_ratio ~ family, data=chem)
boxplot(percP ~ family, data=chem)
#selaginella has lower N and P with higher CNration than ferns on average

sela <- droplevels(chem[chem$family=="Selaginella",])
boxplot(percN ~ species, data=sela)
boxplot(cn_ratio ~ species, data=sela)
boxplot(percP ~ species, data=sela)
#there are some distinct differences in leaf chem within selaginella
#can we see if this is due to habitat?


photo_chem$A_mass <- with(photo_chem, Photo*sla*1000) #amax on a mass basis on nanomoles