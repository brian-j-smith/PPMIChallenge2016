## Set up working directory and environment
setwd("H:\\Projects\\PPMI")
remove(list=objects())


## PPMI source datasets
load("Data\\PPMI.RData")


## Required analysis libraries
library("plyr")


## Project-specific functions
seq.names <- function(x, from, to) {
  vals <- names(x)
  idx <- match(c(from, to), vals)
  vals[seq(idx[1], idx[2])]
}
