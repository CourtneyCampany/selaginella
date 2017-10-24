#functions for project
sumfun <- function(x, ...){
  c(m=mean(x, ...),l=length(x))
}

se <- function(x) sd(x)/sqrt(length(na.omit(x)))