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
load("Data/PDMedUse.RData")
load("Data/PPMI.RData")
load("Data/Subjects.RData")
load("Data/Biospecimen.RData")


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


join.ppmi <- function(..., by=NULL, subset, select, na.add=FALSE, ST2V=FALSE) {
  X <- join_all(list(...), by=by)
  if(ST2V) {
    X <- ddply(X, .(patno), mutate, event_id = ST2V(event_id, infodt))
  }
  Xsub <- droplevels(do.call(base::subset, list(X, subset=substitute(subset),
                                                select=substitute(select))))
  f <- colwise(function(x) if(na.add && is.factor(x)) addNA(x, ifany=TRUE) else x)
  f(Xsub)
}


model.data <- function(fo, data, method=NULL, prop.na=0.20, ...) {
  mf <- model.frame(fo, data, na.action=na.pass)
  x <- model.matrix(fo, mf)
  if(attr(terms(mf), "intercept")) x <- subset(x, select=-`(Intercept)`)
  y <- model.response(mf)

  idx1 <- complete.cases(y)
  idx2 <- apply(x, 2, function(x) mean(is.na(x)) <= prop.na)
  x <- subset(x, idx1, idx2)
  y <- subset(y, idx1)
  
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


ST2V <- function(event_id, infodt) {
  i <- which(event_id == "BL")
  j <- which(event_id == "ST")
  if(length(i) && length(j)) {
    d <- c(1, 12) %*%
      (matrix(as.numeric(unlist(strsplit(infodt, "/"))), nrow=2) -
         as.numeric(unlist(strsplit(infodt[i], "/"))))
    
    visits <- c(V01=3, V02=6, V03=9, V04=12, V05=18, V06=24, V07=30, V08=36,
                V09=42, V10=48, V11=54, V12=60)
    lwr <- visits - 1
    upr <- visits + 1
    
    vid <- sapply(d, function(d) {
      k <- which(lwr <= d & d <= upr)
      if(length(k)) names(k) else NA
    })
    
    v <- vid[j]
    if(!(v %in% event_id)) event_id[j] <- v
  }
  event_id
}
