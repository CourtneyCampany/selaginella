##fern water use efficiency

alldata <- read.csv("raw_data/master_data.csv")


ferns <- alldata[alldata$family ==  "Ferns", c(1:3, 14)]
ferns2 <- ferns[complete.cases(ferns),]


library(doBy)

fernagg <- summaryBy(ITE ~ species, data=ferns2, FUN=mean)

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

fernse <- summaryBy(ITE ~ species, data=ferns2, FUN=se)
