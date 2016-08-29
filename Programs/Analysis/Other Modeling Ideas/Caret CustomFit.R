# Fun with Caret 
require(caret)
require(lme4)

glmModelInfo <- getModelInfo(model = 'glm', regex = F)[[1]]
names(glmModelInfo)

glmModelInfo$fit
# x is baseline data (n x p)
# The outcome vector y will be made up of n blocks of maximum length T, each corresponding to outcome information for a single individual.  

outcome_data <- getOutcomeMeasurements('np1total', Motor)
y <- outcome_data[,3]
Time <- outcome_data[,4]
id <- outcome_data[,1]

customFit <- function(x, y, id, Time, wts, param, lev, last, classProbs, ...) {
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  lmer(y ~ Time)
  
  dat$.outcome <- y
  if(length(levels(y)) > 2) stop("glm models can only use 2-class outcomes")
  
  theDots <- list(...)
  if(!any(names(theDots) == "family"))
  {
    theDots$family <- if(is.factor(y)) binomial() else gaussian()
  }
  
  ## pass in any model weights
  if(!is.null(wts)) theDots$weights <- wts
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat), theDots)
  
  out <- do.call("glm", modelArgs)
  ## When we use do.call(), the call infformation can contain a ton of
  ## information. Inlcuding the contenst of the data. We eliminate it.
  out$call <- NULL
  out
}
