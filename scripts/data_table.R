source("functions.R")
## datatable

sela <- read.csv("calculated_data/sela_photo_chem.csv")
sela2 <- read.csv("raw_data/sela_raw.csv")


stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
m2 <- function(x) mean(x, na.rm=TRUE)

sela_agg <- doBy::summaryBy(.~species, data=sela, FUN=c(m2, stderr))
sela2_agg <- doBy::summaryBy(.~species, data=sela2, FUN=c(m2, stderr))


write.csv(sela2_agg, "calculated_data/data_table.csv", row.names = FALSE)
