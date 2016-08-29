## NOTE: The R programs in this project require that the working directory be
## NOTE: set to the root directory in which the repository is cloned.  Set the
## NOTE: working directory apart from this file or the other programs prior to
## NOTE: running them.


## Remove all objects from the working environment
remove(list=objects())


## Project version
VERSION <- "1.0.0"


## PPMI source datasets
load("Data/PPMI.RData")
load("Data/Biospecimen.RData")
load("Data/Enroll.RData")
load("Data/Imaging.RData")
load("Data/MedHx.RData")
load("Data/Motor.RData")
load("Data/NonMotor.RData")
load("Data/PDMedUse.RData")
load("Data/Subjects.RData")


## Required analysis libraries
if(!require(plyr)) install.packages("plyr")
if(!require(caret)) install.packages("caret")
if(!require(doSNOW)) install.packages("doSNOW")

library(plyr)
library(caret)

library(doSNOW)
library(parallel)
registerDoSNOW(makeCluster(max(detectCores() - 1, 1)))

if(!require(shiny)) install.packages("shiny")
if(!require(DT)) install.packages("DT")
if(!require(ggvis)) install.packages("ggvis")
if(!require(visreg)) install.packages("visreg")
if(!require(gam)) install.packages("gam")

library(gam)

library(shiny)
library(DT)
library(ggvis)


## Install required caret packages
if(!require(RANN)) install.packages("RANN")
if(!require(e1071)) install.packages("e1071")
if(!require(earth)) install.packages("earth")


## Project-specific functions
source("Programs/Utils.R")

## Required analysis libraries
using(plyr)
using(caret)

using(doSNOW)
using(parallel)
registerDoSNOW(makeCluster(max(detectCores() - 1, 1)))

using(shiny)
using(DT)
using(ggvis)


## Install required caret packages
add(RANN)
add(e1071)
