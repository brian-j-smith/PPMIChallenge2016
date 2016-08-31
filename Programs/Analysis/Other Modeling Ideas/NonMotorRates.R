# Author: Ryan Peterson
# Date created : 8/01/16 
# Date modified: 8/03/16 
# Description: Rate of Change calculation and Modeling

source('Programs\\project.R')
source('Programs\\Data\\NonMotor.R')
source('Programs\\Analysis\\BaselineData.R')
source("Programs/Analysis/Control.R")


## FIRST, MODEL ABSOLUTE RATES

# Absolute Rate Models ----------------------------------------------------


outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc', 'upsit_total',
              'scopa_total', 'quip_total', 'ess_total', 'gds_total')
outcomes_in <- NonMotor

rates <- outcome.rate(outcomes[1], NonMotor, patno_only = T)

for(outcome in outcomes) {
  rate.i <- outcome.rate(outcome, outcomes_in, scale = 'absolute')[,c(1:2)]
  names(rate.i) <- c('patno', paste0(outcome, 'rate'))
  rates <- join(rates, rate.i, by = 'patno')
}

head(rates)

Dataset <- join(BaselinePD, rates, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- paste0(outcomes, 'rate')

apply(Dataset[outVars], 2, hist)

trMethods <- c("gbm", "glmnet", "glmStepAIC", "plsRglm", "rf", "svmLinear")
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()
for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)
}

Fit

maxR <- 0
bestMod <- ''

for(outVar in outVars) {
  resamps <- resamples(Fit[[outVar]]$Train)
  s <- summary(resamps)
  best <- max(s$statistics$Rsquared[,3])
  
  if(best > maxR) {
    maxR <- best
    bestMod <- names(which.max(s$statistics$Rsquared[,3]))
    bestVar <- outVar
  }
  print(dotplot(resamps, main = outVar))
}

maxR
bestMod
bestVar
NonMotor_abs_rateFits <- Fit
save(NonMotor_abs_rateFits, file = 'Programs/Analysis/Models/NonMotor_abs_rateFits.RData')


# Relative Rate Models ----------------------------------------------------

# NOW MODEL RELATIVE RATES

outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc', 'upsit_total',
              'scopa_total', 'quip_total', 'ess_total', 'gds_total')
outcomes_in <- NonMotor

rates <- outcome.rate(outcomes[1], NonMotor, patno_only = T)

for(outcome in outcomes) {
  rate.i <- outcome.rate(outcome, outcomes_in, scale = 'relative')[,c(1:2)]
  names(rate.i) <- c('patno', paste0(outcome, 'rate'))
  rates <- join(rates, rate.i, by = 'patno')
}

head(rates)

Dataset <- join(BaselinePD, rates, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- paste0(outcomes, 'rate')

apply(Dataset[outVars], 2, hist)

trMethods <- c("gbm", "glmnet", "glmStepAIC", "plsRglm", "rf", "svmLinear")
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()
for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)
}

Fit

maxR <- 0
bestMod <- ''

for(outVar in outVars) {
  resamps <- resamples(Fit[[outVar]]$Train)
  s <- summary(resamps)
  best <- max(s$statistics$Rsquared[,3])
  
  if(best > maxR) {
    maxR <- best
    bestMod <- names(which.max(s$statistics$Rsquared[,3]))
    bestVar <- outVar
  }
  print(dotplot(resamps, main = outVar))
}

maxR
bestMod
bestVar
NonMotor_rel_rateFits <- Fit
save(NonMotor_rel_rateFits, file = 'Programs/Analysis/Models/NonMotor_rel_rateFits.RData')


