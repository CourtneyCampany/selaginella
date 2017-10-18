source("master_scripts/plot_objects.R")
library(doBy)
##sela vs ferns

sumfun <- function(x, ...){
  c(m=mean(x, ...),l=length(x))
}


se <- function(x) sd(x)/sqrt(length(na.omit(x)))



gas_exchange <- read.csv("raw_data/gas_exchange.csv")
photo_family <-summaryBy(. ~ family, data=gas_exchange, FUN=c(mean,se))

chem <- read.csv("raw_data/leaf_chemistry.csv")
chem2 <- droplevels(chem[, -c(3,4)])
chem_family <-summaryBy(. ~ family, data=chem2, FUN=se)

#this starts the combined dataset for ferns/sela with stomatal tratis
stom <- read.csv("raw_data/leaf_anatomy.csv")
stom_family <-summaryBy(. ~ family, data=stom, FUN=c(mean,se))
  
library(plyr)
chem_agg <- ddply(chem2, "family", summarise,
               N    = sum(!is.na(percP)),
               mean = mean(percP, na.rm=TRUE),
               sd   = sd(percP, na.rm=TRUE),
               se   = sd / sqrt(N)
)

