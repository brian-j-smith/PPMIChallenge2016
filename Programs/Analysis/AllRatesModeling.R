# Author: Ryan Peterson
# Date created : 8/11/16 
# Description: Run all rates models

source('Programs/project.R')
source('Programs/Analysis/BaselineData.R')
source("Programs/Analysis/Control.R")
load("Data/Imaging.RData")
load("Data/Motor.RData")
load("Data/NonMotor.RData")

# Initialize outcomes of interest
motor_outcomes <- c('np1total', 'np2total', 'np3total', 'nptotal')
non_motor_outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc',
                        'scopa_total', 'quip_total', 'ess_total', 'gds_total',
                        'dvt_total_recall')
imaging_outcomes <- c('aiputamen', 'countdensityratio', 'meanstriatum', 'meanputamen')
outcomes <- c(motor_outcomes, non_motor_outcomes, imaging_outcomes)


# Initialize all scales
getRelative <- c('meanstriatum', 'meanputamen')

scales <- ifelse(outcomes %in% getRelative, 'relative', 'absolute')

outVars <- paste(outcomes, scales, sep = '.')
save(outVars, file = 'Programs/Analysis/Models/Rate_outVars.RData')


time_cuttoff <- 24 # Don't include observations past this length of time
visit_musts <- 'V06' # Patients must have had this visit

## Initialize rates (extract patno only)
rates <- outcome.rate(outcomes[1], Motor, patno_only = T, 
                      exclude_MedUse = F, visit_musts = visit_musts, 
                      time_cuttoff = time_cuttoff)

# Calculate outcomes from data
## Prints outcome for patients whose relative models don't converge,
## For whose slope is assumed to be 0... make sure it makes sense
i <- 0
for(outcome in outcomes) {
  i <- i + 1
  if(outcome %in% motor_outcomes) data <- Motor
  if(outcome %in% non_motor_outcomes) data <- NonMotor
  if(outcome %in% imaging_outcomes) data <- Imaging
    
  rate.i  <- outcome.rate(outcome, data, scale = scales[i], 
                          exclude_MedUse = T, visit_musts = visit_musts, 
                          time_cuttoff = time_cuttoff)[,c(1:2)]
  names(rate.i) <- c('patno', paste(outcome, scales[i], sep = '.'))
  rates <- join(rates, rate.i, by = 'patno')
}

(h <- head(rates))
summary(rates)

# Save rates dataset
save(rates, file = 'Programs/Analysis/Models/rates.RData')

## Initialize Dataset for Modeling
Dataset <- join(BaselinePD, rates, by = "patno")
str(Dataset)

## Run models
trMethods <- c("gbm", "glmnet", 'glmStepAIC',"pls", "rf", "svmLinear", 'svmRadial')
sbfMethods <- 'glm'
rfeMethods <- NULL

save(trMethods, sbfMethods, rfeMethods, file = 'Programs/Analysis/Models/controlMethods.RData')

Fit <- list()

nIter <- length(outVars)
i <- 1
for(outVar in (outVars)) {
  
  cat('Modeling ', i, ' out of ', nIter, ' variables (', outVar,')\n', sep = '')
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            seed = 1232)
  i <- i + 1
  tempRatesFits <- Fit
  save(tempRatesFits, file = 'Programs/Analysis/Models/tempRatesFits.RData')
  
}

Fit
AllRatesFits <- Fit
save(AllRatesFits, file = 'Programs/Analysis/Models/AllRatesFits.RData')
