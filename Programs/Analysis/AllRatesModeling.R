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
imaging_outcomes <- c('aiputamen', 'countdensityratio', 'aicaudate','meanstriatum', 'meanputamen')
outcomes <- c(motor_outcomes, non_motor_outcomes, imaging_outcomes)


# Initialize all scales
getRelative <- c('meanstriatum', 'meanputamen')

scales <- ifelse(outcomes %in% getRelative, 'relative', 'absolute')

outVars <- paste(outcomes, scales, sep = '.')
# save(outVars, file = 'Programs/Analysis/Models/Rate_outVars.RData')

time_cuttoff <- 24 # Don't include observations past this length of time
visit_musts <- 'V06' # Patients must have had this visit

# Calculate outcomes from data
## Prints outcome for patients whose relative models don't converge,
## For whose slope is assumed to be 0... make sure it makes sense
Dataset <- BaselinePD
i <- 0
for(outcome in outcomes) {
  i <- i + 1
  baseline.id <- c('SC', 'BL')
  if(outcome %in% motor_outcomes) {
    data <- Motor
    baseline.id <- 'BL'
  }
  if(outcome %in% non_motor_outcomes) data <- NonMotor
  if(outcome %in% imaging_outcomes) data <- Imaging
    
  rate.i  <- outcome.rate(outcome, data, scale = scales[i], 
                          exclude_MedUse = F, visit_musts = visit_musts, 
                          baseline.id = baseline.id,
                          time_cuttoff = time_cuttoff)[,c(1:2)]
  names(rate.i) <- c('patno', paste(outcome, scales[i], sep = '.'))
  Dataset <- join(Dataset, rate.i, by = 'patno')
}

str(Dataset)
apply(Dataset, 2, function(x) sum(!is.na(x)))

outVarsList <- list(
  "MDS-UPDRS" = c("2-Year Slope" = "nptotal.absolute"),
  "MDS-UPDRS I" = c("2-Year Slope" = "np1total.absolute"),
  "MDS-UPDRS II" = c("2-Year Slope" = "np2total.absolute"),
  "MDS-UPDRS III" = c("2-Year Slope" = "np3total.absolute"),
  
  "MCA Total" = c("2-Year Slope" = "mcatot.absolute"),
  "GDS Total" = c("2-Year Slope" = "gds_total.absolute"),
  "REM Total" = c("2-Year Slope" = "rem_total.absolute"),
  "STAI Total" = c("2-Year Slope" = "stai_total.absolute"),
  "JLO Total" = c("2-Year Slope" = "jlo_totcalc.absolute"),
  "SCOPA Total" = c("2-Year Slope" = "scopa_total.absolute"),
  "QUIP Total" = c("2-Year Slope" = "quip_total.absolute"),
  "ESS Total" = c("2-Year Slope" = "ess_total.absolute"),
  "Total Recall" = c("2-Year Slope" = "dvt_total_recall.absolute"),
  
  "AI Putamen" = c("2-Year Slope" = "aiputamen.absolute"),
  "AI Caudate" = c("2-Year Slope" = "aicaudate.absolute"),
  "CDR" = c("2-Year Slope" = "countdensityratio.absolute"),
  "Mean Striatum" = c("2-Year Relative Slope" = "meanstriatum.relative"),
  "Mean Putamen" = c("2-Year Relative Slope" = "meanputamen.relative")
)


## Run models
trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "pls", "rf", "svmLinear",
               "svmRadial")
sbfMethods <- c("glm")

tuneGrids <- list(
  "nnet" = expand.grid(size=c(1, 3, 5), decay=0.1^(1:4))
)

Fit <- list()

nIter <- length(outVars)
i <- 1
for(outVar in unlist(outVars)) {
  
  cat('Modeling ', i, ' out of ', nIter, ' variables (', outVar,')\n', sep = '')
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods,  tuneGrids=tuneGrids,
                            seed = 1232)
  i <- i + 1
  tempRatesFits <- Fit
  save(tempRatesFits, file = 'Programs/Analysis/Models/tempRatesFits.RData')
  
}

Fit
AllRatesFits <- Fit
save(AllRatesFits, file = 'Programs/Analysis/Models/AllRatesFits.RData')

## Summary results

RatesFitsSummary <- SummaryTable(AllRatesFits, digits=3)
RatesFitsBest <- bestmodel(AllRatesFits, metric = 'RMSE')

## Shiny trial design tool data

RatesFitsVars <- outVarsList


idx <- unlist(RatesFitsVars) %in% unlist(RatesFitsVars[c("Mean Putamen", 'Mean Striatum')])
RatesFitsVals <- c(
  outValsList(RatesFitsBest[!idx], digits=1, 
              transform = function(x) 24 * x),
  
  outValsList(RatesFitsBest[idx], digits=3, 
              transform = function(x) exp(24 * x) - 1)
)

## Save results and data

save(RatesFitsSummary, RatesFitsBest, RatesFitsVars, RatesFitsVals,
     file="Programs/Analysis/RatesFits.RData")

