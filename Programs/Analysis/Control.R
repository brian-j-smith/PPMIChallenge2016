## Global variables

cv.number <- 10
cv.repeats <- 5


## Train control functions

trSeeds <- function(seed, M = 100) {
  if(!missing(seed)) set.seed(seed)
  B <- cv.number * cv.repeats
  x <- sample.int(100000, B * M + 1, replace=TRUE)
  c(split(x[-1], 1:B), x[1])
}

trControlCV <- function(seed, ...) {
  trainControl(
    method = "repeatedcv",
    number = cv.number,
    repeats = cv.repeats,
    seeds = trSeeds(seed),
    ...
  )
}


## Selection by filtering control functions

caretSBFP <- function(alpha = 0.05) {
  funcs <- caretSBF
  funcs$filter <- eval(bquote(function(score, x, y) score <= .(alpha)))
  funcs
}

sbfSeeds <- function(seed) {
  if(!missing(seed)) set.seed(seed)
  B <- cv.number * cv.repeats
  sample.int(100000, B + 1, replace=TRUE)
}

sbfControlCV <- function(alpha = 0.05, seed, ...) {
  sbfControl(
    functions = caretSBFP(alpha),
    method = "repeatedcv",
    number = cv.number,
    repeats = cv.repeats,
    seeds = sbfSeeds(seed),
    ...
  )
}


## Recursive feature elimination control functions

rfeSeeds <- function(seed, P = 100) {
  trSeeds(seed, M = P)
}

rfeControlCV <- function(seed, ...) {
  rfeControl(
    functions = caretFuncs,
    method = "repeatedcv",
    number = cv.number,
    repeats = cv.repeats,
    seeds = rfeSeeds(seed),
    ...
  )
}


## Model-fitting engine

modelfit <- function(formula, data, dataMethods=c("zv", "nzv"),
                     ImpMethod="knnImpute", trMethods=NULL, sbfMethods=NULL,
                     rfeMethods=NULL, trControl=trControlCV(seed=seed),
                     sbfControl=sbfControlCV(0.05, seed=seed),
                     rfeControl=rfeControlCV(seed=seed), tuneGrids=list(),
                     tuneLengths=list(), prop.na = 0.2, seed, ...) {
  Train <- list()
  SBF <- list()
  RFE <- list()

  ModelData <- model.data(formula, data, method=dataMethods, prop.na = prop.na, ...)
  
  ## Training
  for(trMethod in trMethods) {
    args <- list(
      ModelData$x, ModelData$y,
      method = trMethod,
      preProcess = ImpMethod,
      trControl = trControl,
      tuneGrid = tuneGrids[[trMethod]],
      tuneLength = tuneLengths[[trMethod]]
    )
    if(is.null(args$tuneLength)) args$tuneLength <- 3
    if(trMethod == "rf") args$importance <- TRUE
    Train[[trMethod]] <- try(do.call(train, args), TRUE)
  }

  ## Selection by filtering
  for(sbfMethod in sbfMethods) {
    args <- list(
      ModelData$x, ModelData$y,
      method = sbfMethod,
      preProcess = ImpMethod,
      sbfControl = sbfControl
    )
    if(sbfMethod == "rf") args$importance <- TRUE
    SBF[[sbfMethod]] <- try(do.call(sbf, args), TRUE)
  }
  
  ## Recursive feature extraction (backward selection)
  for(rfeMethod in rfeMethods) {
    args <- list(
      ModelData$x, ModelData$y,
      method = rfeMethod,
      preProcess = ImpMethod,
      rfeControl = rfeControl
    )
    if(rfeMethod == "rf") args$importance <- TRUE
    RFE[[rfeMethod]] <- try(do.call(rfe, args), TRUE)
  }
  
  list(Train=Train, SBF=SBF, RFE=RFE)
}
