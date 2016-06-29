## NOTE: The R programs in this project require that the working directory be
## NOTE: set to the root directory in which the repository is cloned.  Set the
## NOTE: working directory apart from this file or the other programs prior to
## NOTE: running them.


## Remove all objects from the working environment
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
