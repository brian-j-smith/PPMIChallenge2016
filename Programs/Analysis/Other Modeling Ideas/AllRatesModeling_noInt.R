# Author: Ryan Peterson
# Date created : 8/11/16 
# Description: Run all rates models (forcing intercept through 0)

source('Programs/project.R')
source('Programs/Data/Motor.R')
source('Programs/Data/NonMotor.R')
source('Programs/Data/Imaging.R')
source('Programs/Analysis/BaselineData.R')
source("Programs/Analysis/Control.R")

# Initialize outcomes of interest
motor_outcomes <- c('np1total', 'np2total', 'np3total', 'nptotal')
non_motor_outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc',
                        'scopa_total', 'quip_total', 'ess_total', 'gds_total')
imaging_outcomes <- c('aiputamen', 'countdensityratio', 'meanstriatum')
outcomes <- c(motor_outcomes, non_motor_outcomes, imaging_outcomes)


# Initialize all scales
scales <- c('relative', 'absolute')

outVars <- paste(rep(outcomes, each = 2), scales, sep = '.')

# save(outVars, file = 'Programs/Analysis/Models/Rate_outVars.RData')

## Initialize rates (extract patno only)
rates <- outcome.rate(outcomes[1], Motor, patno_only = T)

# Calculate outcomes from data
## Prints outcome for patients whose relative models don't converge,
## For whose slope is assumed to be 0... make sure it makes sense
for(outcome in outcomes) {
  
  if(outcome %in% motor_outcomes) data <- Motor
  if(outcome %in% non_motor_outcomes) data <- NonMotor
  if(outcome %in% imaging_outcomes) data <- Imaging
  
  for(scale in scales) {
    rate.ij <- outcome.rate(outcome, data, scale = scale, 
                            allow_int = F)[,c(1:2)]
    names(rate.ij) <- c('patno', paste(outcome, scale, sep = '.'))
    rates <- join(rates, rate.ij, by = 'patno')
  }
}

(h <- head(rates))
summary(rates)

# Save rates dataset
# save(rates, file = 'Programs/Analysis/Models/rates.RData')

## Initialize Dataset for Modeling
Dataset <- join(BaselinePD, rates, by = "patno")
str(Dataset)

## Run models
trMethods <- c("gbm", "glmnet", "glmStepAIC", "pls", "rf", "svmLinear", 'svmRadial', 'earth')
sbfMethods <- 'glm'
rfeMethods <- NULL

# save(trMethods, sbfMethods, file = 'Programs/Analysis/Models/controlMethods.RData')

Fit <- list()

nIter <- length(outVars)
i <- 1
for(outVar in rev(outVars)) {
  
  cat('Modeling ', i, ' out of ', nIter, ' variables (', outVar,')\n', sep = '')
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25, seed = 1232)
  i <- i + 1
  tempRatesFits_noInt <- Fit
  save(tempRatesFits_noInt, file = 'Programs/Analysis/Models/tempRatesFitsnoInt.RData')
  
}

Fit
AllRatesFits_noInt <- Fit
save(AllRatesFits_noInt, file = 'Programs/Analysis/Models/AllRatesFits_noInt.RData')
