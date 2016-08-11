## Global variables

preProcMethod <- c("knnImpute")


## Train control functions

trControlCV <- function(...) {
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    ...
  )
}


## Selection by filtering control functions

caretSBFP <- function(alpha = 0.05) {
  funcs <- caretSBF
  funcs$filter <- eval(bquote(function(score, x, y) score <= .(alpha)))
  funcs
}

sbfControlCV <- function(alpha = 0.05, ...) {
  sbfControl(
    functions = caretSBFP(alpha),
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    ...
  )
}


## Recursive feature elimination control functions

rfeControlCV <- function(...) {
  rfeControl(
    functions = caretFuncs,
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    ...
  )
}


## Model-fitting engine

modelfit <- function(formula, data, dataMethods=c("zv", "nzv"),
                     ImpMethod="knnImpute", trMethods=NULL, sbfMethods=NULL,
                     rfeMethods=NULL, trControl=trControlCV(),
                     sbfControl=sbfControlCV(0.20), rfeControl=rfeControlCV(),
                     tuneGrids=list(), tuneLengths=list(), prop.na = 0.2, ...) {
  Train <- list()
  SBF <- list()
  RFE <- list()

  ModelData <- model.data(formula, data, method=dataMethods, prop.na = prop.na)
  
  ## Training
  for(trMethod in trMethods) {
    tuneLength <- tuneLengths[[trMethod]]
    if(is.null(tuneLength)) tuneLength <- 3
    
    Train[[trMethod]] <- try(train(
      ModelData$x, ModelData$y,
      method = trMethod,
      preProcess = ImpMethod,
      trControl = trControl,
      tuneGrid = tuneGrids[[trMethod]],
      tuneLength = tuneLength
    ), TRUE)
  }

  ## Selection by filtering
  for(sbfMethod in sbfMethods) {
    SBF[[sbfMethod]] <- try(sbf(
      ModelData$x, ModelData$y,
      method = sbfMethod,
      preProcess = ImpMethod,
      sbfControl = sbfControl
    ), TRUE)
  }
  
  ## Recursive feature extraction (backward selection)
  for(rfeMethod in rfeMethods) {
    RFE[[rfeMethod]] <- try(rfe(
      ModelData$x, ModelData$y,
      method = rfeMethod,
      preProcess = ImpMethod,
      rfeControl = rfeControl
    ), TRUE)
  }
  
  list(Train=Train, SBF=SBF, RFE=RFE)
}
