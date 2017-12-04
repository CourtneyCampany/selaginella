stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
m2 <- function(x) mean(x, na.rm=TRUE)