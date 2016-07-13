## NOTE: The R programs in this project require that the working directory be
## NOTE: set to the root directory in which the repository is cloned.  Set the
## NOTE: working directory apart from this file or the other programs prior to
## NOTE: running them.


## Remove all objects from the working environment
remove(list=objects())


## PPMI source datasets
load("Data/Enroll.RData")
load("Data/Imaging.RData")
load("Data/MedHx.RData")
load("Data/Motor.RData")
load("Data/NonMotor.RData")
load("Data/PPMI.RData")
load("Data/Subjects.RData")


## Required analysis libraries
library(plyr)
library(caret)

library(doSNOW)
library(parallel)
registerDoSNOW(makeCluster(max(detectCores() - 1, 1)))


## Install required caret packages
if(!require(RANN)) install.packages("RANN")
if(!require(e1071)) install.packages("e1071")


## Project-specific functions

auc.change <- function(x, time) {
  n <- ncol(x)
  timediff <- diff(time)
  
  stopifnot(n == length(time))
  stopifnot(all(timediff > 0))
  
  auc <- 0
  for(i in seq(2, n, by=1)) {
    xmin <- pmin(x[,i], x[,i-1])
    auc <- auc + timediff[i-1] * (0.5 * abs(x[,i] - x[,i-1]) + xmin - x[,1])
  }
  auc
}


dropfactors <- function(data) {
  i <- 1
  while(i <= length(data)) {
    x <- data[[i]]
    if(is.factor(x) && nlevels(x) < 2) {
      data[[i]] <- NULL
    } else {
      i <- i + 1
    }
  }
  data
}


join.ppmi <- function(..., by=NULL, subset, select, na.add=FALSE) {
  X <- join_all(list(...), by=by)
  Xsub <- droplevels(do.call(base::subset, list(X, subset=substitute(subset),
                                                select=substitute(select))))
  f <- colwise(function(x) if(na.add && is.factor(x)) addNA(x, ifany=TRUE) else x)
  f(Xsub)
}


model.data <- function(fo, data, method=NULL, ...) {
  mf <- model.frame(fo, data, na.action=na.pass)
  x <- model.matrix(fo, mf)
  if(attr(terms(mf), "intercept")) x <- subset(x, select=-`(Intercept)`)
  y <- model.response(mf)

  idx <- complete.cases(y)
  x <- subset(x, idx)
  y <- subset(y, idx)
  
  if(length(method)) {
    pp <- preProcess(x, method=method, ...)
    x <- predict(pp, x)
  }
  
  list(x=x, y=y)
}


seq.names <- function(x, from, to) {
  vals <- names(x)
  idx <- match(c(from, to), vals)
  vals[seq(idx[1], idx[2])]
}
