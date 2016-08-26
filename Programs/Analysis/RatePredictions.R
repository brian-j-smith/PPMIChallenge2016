# Author: Ryan Peterson
# Date created : 8/11/16 
# Description: Find Prediction intervals from rates

source('Programs/project.R')
source('Programs/Analysis/BaselineData.R')

load('Programs/Analysis/Models/tempRatesFits.RData')
load('Programs/Analysis/Models/rates.RData')
load('Programs/Analysis/Models/AllRatesFits.RData')
load('Programs/Analysis/Models/Rate_outVars.RData')
load('Programs/Analysis/Models/controlMethods.RData')

(Results <- getWithinSampleError(AllRatesFits))

save(Results, file = 'H:\\PPMI Data Challenge 2016 SourceTree\\Programs\\Analysis\\Models\\Results.RData')

(Results <- getWithinSampleError(AllRatesFits))
SortedResults <- Results[order(Results$cvRsquared, decreasing = T),]

# Calculate Model Accuracy ------------------------------------------------

## General prediction accuracy from rates models

allFits <- list()
for(outVar in outVars) {
  allFits <- c(allFits, AllRatesFits24[[outVar]]$Train)
}

length(allFits)
names(allFits) <- paste(rep(outVars, each = length(trMethods)), trMethods, sep = '.')

(resamps <- resamples(allFits))
(RatesModelingSummary <- summary(resamps))
save(RatesModelingSummary, file = 'Programs\\Analysis\\Models\\RatesModelingSummary.RData')

print(dotplot(resamps, metric = 'Rsquared', scales = list(cex = .5)))

## Get best fits for each outcome using Brian's function, plot
bestFits <- list() 
for(outVar in outVars) {
  bestFits <- c(bestFits, bestModel(allFits, outVar))
}
length(bestFits) # should be equal to dimension of outVar
names(bestFits) <- outVars

(resamps <- resamples(bestFits))
(RatesBestModelingSummary <- summary(resamps))
print(dotplot(resamps, metric = 'Rsquared', scales = list(cex = .9)))


## Final outvars
final.outvars <- c(outVars[c(1,3,5,7,8,10)], "aiputamen.absolute", "quip_total.absolute",
                   "gds_total.absolute", 'stai_total.absolute', 'scopa_total.absolute')
finalFits <- list()
for(outVar in final.outvars) {
  finalFits <- c(finalFits, AllRatesFits24[[outVar]]$Train)
}

length(finalFits)
names(finalFits) <- paste(rep(final.outvars, each = length(trMethods)), trMethods, sep = '.')

(resamps <- resamples(finalFits))
print(dotplot(resamps, metric = 'Rsquared', scales = list(cex = .8)))
(FinalRatesModelingSummary <- summary(resamps))
save(FinalRatesModelingSummary, file = 'Programs\\Analysis\\Models\\FinalRatesModelingSummary.RData')


# Prediction Intervals ----------------------------------------------------

## Initialize prediction confidence level
prediction_int <- .95

## Function for null predictions
find_null_PI <- function(x, level,...) {
  qq <- (1-level)/2
  quantile(x, c(qq, 1-qq),...)
}

(null_PIs <- apply(rates[-1], 2, find_null_PI, level = prediction_int, na.rm = T))


## Extract prediction intervals

Dataset <- join(BaselinePD, rates, by = "patno")

## Accept user input
outcome.input <- 'np1total'
scale.input <- 'absolute'
modeling.fw.input <- 'glmnet'

## match to model in allFits
outVar <- paste(outcome.input, scale.input, modeling.fw.input, sep = '.')
model.out <- allFits[[outVar]]
# Create data to find prediction for (can be user input)

newPatients <- model.out$trainingData[!(names(model.out$trainingData) == '.outcome')]

# point estimate
(p <- predict(model.out, newdata = newPatients, na.action = na.pass))

# prediction interval?
(rmse.cv <- min(model.out$results$RMSE))

actual <- Dataset[paste(outcome.input, scale.input, sep = '.')]
actual <- actual[,1]

(rmse.null <- sqrt(mean((actual - mean(actual, na.rm = T))^2, na.rm = T)))
(rmse.overfit <- sqrt(mean((actual - p)^2, na.rm = T))) # Not cross-validated

plot(varImp(model.out))

getPredictions <- function(x, rmse.cv, rmse.null, level, dec.places = 2, print = F) {
  z <- -qnorm((1-level)/2)
  preds <- cbind(x,  
             x - z * rmse.cv,
             x + z * rmse.cv,
             x - z * rmse.null,
             x + z * rmse.null)
  colnames(preds) <- c('Point Est', 'CV.lb', 'CV.ub', 'Null.lb', 'Null.ub')
  if(print) print(round(preds, dec.places))
  return(as.data.frame(preds))
}

pp <- getPredictions(p, rmse.cv, rmse.null, .95, 2, T)

coverage.cv <- ifelse(pp$CV.lb < actual & actual < pp$CV.ub, 1, 0)
mean(coverage.cv)

coverage.null <- ifelse(pp$Null.lb < actual & actual < pp$Null.ub, 1, 0)
mean(coverage.null)

