##this script looks at leaf chemistry

chem <- read.csv("raw_data/leaf_chemistry.csv")

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