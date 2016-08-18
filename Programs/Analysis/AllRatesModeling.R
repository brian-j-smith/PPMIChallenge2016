# Author: Ryan Peterson
# Date created : 8/11/16 
# Description: Run all rates models

source('Programs/project.R')
source('Programs/Data/Motor.R')
source('Programs/Data/NonMotor.R')
source('Programs/Analysis/BaselineData.R')
source("Programs/Analysis/Control.R")

# Initialize outcomes of interest
motor_outcomes <- c('np1total', 'np2total', 'np3total', 'nptotal')
non_motor_outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc', 'upsit_total',
                        'scopa_total', 'quip_total', 'ess_total', 'gds_total')
other_outcomes <- c()
outcomes <- c(motor_outcomes, non_motor_outcomes, other_outcomes)


# Initialize all scales
scales <- c('relative', 'absolute')

outVars <- paste(rep(outcomes, each = 2), scale, sep = '.')

# save(outVars, file = 'Programs/Analysis/Models/Rate_outVars.RData')

## Initialize rates (extract patno only)
rates <- outcome.rate(outcomes[1], Motor, patno_only = T)

# Calculate outcomes from data
## Prints outcome for patients whose relative models don't converge,
## For whose slope is assumed to be 0... make sure it makes sense
for(outcome in outcomes) {
  if(outcome %in% motor_outcomes) data <- Motor
  if(outcome %in% non_motor_outcomes) data <- NonMotor
  if(outcome %in% other_outcomes) stop('Specify Data set for other outcomes')
  
  for(scale in scales) {
    rate.ij <- outcome.rate(outcome, data, scale = scale)[,c(1:2)]
    names(rate.ij) <- c('patno', paste(outcome, scale, sep = '.'))
    rates <- join(rates, rate.ij, by = 'patno')
  }
}

(h <- head(rates))
summary(rates)

## Initialize Dataset for Modeling
Dataset <- join(BaselinePD, rates, by = "patno")
str(Dataset)

## Run models
trMethods <- c("gbm", "glmnet", "glmStepAIC", "pls", "rf", "svmLinear")
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()

nIter <- length(outVars)
i <- 1
for(outVar in outVars) {
  
  cat('Modeling ', i, ' out of ', nIter, ' variables (', outVar,')\n', sep = '')
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)
}

Fit
AllRatesFits <- Fit
# save(AllRatesFits, file = 'Programs/Analysis/Models/AllRatesFits.RData')