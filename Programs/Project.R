## NOTE: The R programs in this project require that the working directory be
## NOTE: set to the root directory in which the repository is cloned.  Set the
## NOTE: working directory apart from this file or the other programs prior to
## NOTE: running them.


## Remove all objects from the working environment
remove(list=objects())


## Project version
VERSION <- "1.1.0"


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


## Project-specific functions
source("Programs/Utils.R")


## Required analysis libraries
using(plyr)
using(caret)

using(doSNOW)
using(parallel)
registerDoSNOW(makeCluster(max(detectCores() - 1, 1)))

using(shiny)
using(shinyBS)
using(DT)
using(ggvis)
using(gam)

## Install required caret packages
add(RANN)
add(e1071)