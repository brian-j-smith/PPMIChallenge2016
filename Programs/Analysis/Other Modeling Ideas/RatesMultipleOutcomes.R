# Author: Ryan Peterson
# Date created : 8/10/16 
# Date modified: 8/10/16 
# Description: Rate of Change calculation and Modeling for MULTIPLE OUTCOMES

source('Programs\\project.R')
source('Programs\\Data\\Motor.R')
source('Programs\\Data\\NonMotor.R')
source('Programs\\Analysis\\BaselineData.R')
source("Programs/Analysis/Control.R")

motor_outcomes <- c('np1total', 'np2total', 'np3total', 'nptotal')
non_motor_outcomes <- c('mcatot', 'rem_total', 'stai_total', 'jlo_totcalc', 'upsit_total',
                        'scopa_total', 'quip_total', 'ess_total', 'gds_total')
outcomes <- c(motor_outcomes, non_motor_outcomes)

rates <- outcome.rate(outcomes[1], Motor, patno_only = T)

for(outcome in outcomes) {
  if(outcome %in% motor_outcomes) data <- Motor
  if(outcome %in% non_motor_outcomes) data <- NonMotor
  rate.i <- outcome.rate(outcome, data, scale = 'absolute')[,c(1:2)]
  names(rate.i) <- c('patno', paste0(outcome, 'rate'))
  rates <- join(rates, rate.i, by = 'patno')
}

(h <- head(rates))

## Calculate outcome for each patno
find_euclid <- function(x) sqrt(sum(x^2))
find_manhattan <- function(x) dist(rbind(x, rep(0, length(x))), 'manhattan')
find_minkowski <- function(x) dist(rbind(x, rep(0, length(x))), 'minkowski')

## Weight your own?
find_wtsum <- function(x, w = rep(1,length(x))) sum(x*w)
find_std_wtsum <- function(x) {
  g_slopes <- scale(x, center = F)
  w <- rep(1, dim(x)[2])
  apply(g_slopes, 1, find_wtsum)
}


euclid_norm <- apply(rates[,-1], 1, find_euclid)
max <- apply(rates[,-1], 1, max)
manhattan <- apply(rates[,-1], 1, find_manhattan)
minkowski <- apply(rates[,-1], 1, find_minkowski)
wtsum <- apply(rates[,-1], 1, find_wtsum)
std_wtsum <- find_std_wtsum(rates[,-1])

par(mfrow = c(2,2))
hist(euclid_norm)
hist(max)
hist(manhattan)
hist(minkowski)
hist(wtsum)
hist(std_wtsum)


comb_outcome <- data.frame(patno = rates$patno, euclid_norm = euclid_norm, 
                           max = max, manhattan = manhattan, 
                           minkowski = minkowski, wtsum = wtsum,
                           std_wtsum = std_wtsum)
head(comb_outcome)

Dataset <- join(BaselinePD, comb_outcome, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- names(comb_outcome)[6:7]

trMethods <- c("glmnet", "rf")
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

combined_abs_rateFits <- Fit
save(combined_abs_rateFits, file = 'Programs/Analysis/Models/combined_abs_rateFits.RData')



