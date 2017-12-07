#Fern vs Sela stats
library(visreg)
library(multcomp)

##i dont beleive we have light response curves for all ferns
##do not have chlorophyll for ferns

alldata <- read.csv("raw_data/master_data.csv")

sela <- alldata[alldata$family == "Selaginella",]
ferns <- alldata[alldata$family == "Ferns",]

# photosynthesis ----------------------------------------------------------
t.test(ferns$asat, sela$asat)
####not different
boxplot(asat ~ family, data=alldata)

# conductance ----------------------------------------------------------
t.test(ferns$gs, sela$gs)
####higher gs on Ferns
boxplot(gs ~ family, data=alldata)

# LMA ----------------------------------------------------------
t.test(ferns$LMA, sela$LMA)
####ferns thicker
boxplot(LMA ~ family, data=alldata)

# N ----------------------------------------------------------
t.test(ferns$N, sela$N)
####higher N conent in Ferns
boxplot(N ~ family, data=alldata)

# P ----------------------------------------------------------
t.test(ferns$P, sela$P)
####higher P content in Ferns
boxplot(P ~ family, data=alldata)

# sto_dens ----------------------------------------------------------
t.test(ferns$sto_dens, sela$sto_dens)
####more dense stomata in Selaginella
boxplot(sto_dens ~ family, data=alldata)

# sto_length ----------------------------------------------------------
t.test(ferns$sto_length, sela$sto_length)
####shorter stomata in Selaginella
boxplot(sto_length ~ family, data=alldata)

# ITE ----------------------------------------------------------
t.test(ferns$ITE, sela$ITE)
###ITE not different
boxplot(ITE ~ family, data=alldata)

# C.N ----------------------------------------------------------
t.test(ferns$C.N, sela$C.N)
####higher CN in Sela
boxplot(C.N ~ family, data=alldata)
