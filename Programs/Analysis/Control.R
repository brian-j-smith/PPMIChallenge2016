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


## Recursive feature selection control objects

rfeControlCV <- function(functions, ...) {
  rfeControl(
    functions = functions,
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    ...
  )
}