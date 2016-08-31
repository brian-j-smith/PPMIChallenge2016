# Author: Ryan Peterson
# Date created : 8/10/16 
# Date modified: 8/10/16 
# Description: Optimal weights for multiple outcomes

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

## Weight your own fn
find_wtsum <- function(x, w = rep(1,dim(x)[1])) t((as.matrix(w)) %*% t(x))

# First find optimal motor weights
w_tries <- list()
for(i in 1:length(motor_outcomes)) w_tries[[i]] <- c(0,1)
w_tries
motor_w_tries <- expand.grid(w_tries)[-1,]

# Create outcomes under motor weighting schemes
motor_rates <- rates[,2:5]
wtd_rates <- c()
outVars <- c()
for(i in 1:dim(motor_w_tries)[1]){
  outVar <- paste0('w', paste0(motor_w_tries[i,], collapse = '_'))
  wtd_rate.i <- find_wtsum(motor_rates, motor_w_tries[i,])
  outVars <- c(outVars, outVar)
  wtd_rates <- (cbind(wtd_rates, wtd_rate.i))
  colnames(wtd_rates) <- outVars
  cat('generating outcomes...\n')
}

dim(wtd_rates)

wts <- data.frame(outcome.rate(outcomes[1], Motor, patno_only = T), wtd_rates)
head(wts)

Dataset <- join(BaselinePD, wts, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- colnames(wtd_rates)

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
bestVar <- ''

allFits <- list()
for(outVar in outVars) {
  allFits <- c(allFits, Fit[[outVar]]$Train)
}
length(allFits)
names(allFits) <- paste0(trMethods, rep(outVars, each = length(trMethods)))

(resamps <- resamples(allFits))
(s <- summary(resamps))

print(dotplot(resamps, metric = 'Rsquared'))


combined_abs_rate_optimal_weight_motorFits <- Fit
save(combined_abs_rate_optimal_weight_motorFits, 
     file = 'Programs/Analysis/Models/combined_abs_rate_optimal_weight_motorFits.RData')

## Now for NonMotors

w_tries <- list()
for(i in 1:length(non_motor_outcomes)) w_tries[[i]] <- c(0,1)
w_tries
non_motor_w_tries <- expand.grid(w_tries)[-1,]

# Create outcomes under motor weighting schemes
wtd_rates <- c()
outVars <- c()
non_motor_rates <- rates[,6:14]

for(i in 1:dim(non_motor_w_tries)[1]){
  outVar <- paste0('w', paste0(non_motor_w_tries[i,], collapse = '_'))
  wtd_rate.i <- find_wtsum(non_motor_rates, w = non_motor_w_tries[i,])
  outVars <- c(outVars, outVar)
  wtd_rates <- (cbind(wtd_rates, wtd_rate.i))
  colnames(wtd_rates) <- outVars
  cat('generating outcome', i, 'out of', dim(non_motor_w_tries)[1], '\n')
}

dim(wtd_rates)

wts <- data.frame(outcome.rate(outcomes[1], Motor, patno_only = T), wtd_rates)
head(wts)

Dataset <- join(BaselinePD, wts, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- colnames(wtd_rates)
trMethods <- c("glmnet")
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()
i <- 1

for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)
  cat('Fitting', i, 'out of', dim(non_motor_w_tries)[1], '\n')
  i <- i + 1
}

Fit

maxR <- 0
bestMod <- ''
bestVar <- ''

allFits <- list()
for(outVar in outVars) {
  allFits <- c(allFits, Fit[[outVar]]$Train)
}
length(allFits)
names(allFits) <- paste0(trMethods, rep(outVars, each = length(trMethods)))

(resamps <- resamples(allFits))
(s <- summary(resamps))

print(dotplot(resamps, metric = 'Rsquared', ylab = ''))

combined_abs_rate_optimal_weight_nonmotorFits <- Fit
save(combined_abs_rate_optimal_weight_nonmotorFits, 
     file = 'Programs/Analysis/Models/combined_abs_rate_optimal_weight_nonmotorFits.RData')

sort(s$statistics$Rsquared[,3])

## Assign weights based on Rsquared and number of times chosen
names(allFits)

NMresults <- cbind(non_motor_w_tries,s$statistics$Rsquared[,3])
head(NMresults)

f <- function(x) (x %*% s$statistics$Rsquared[,3]) / sum(x)

cv_wts <- apply(non_motor_w_tries, 2, f)
cv_wts
sum(cv_wts)

# final NM Fit

cv_wtd_rate <- find_wtsum(non_motor_rates, w = t(as.matrix(cv_wts)))

cv_wts <- data.frame(outcome.rate(outcomes[1], Motor, patno_only = T), cv_wtd_rate)
head(cv_wts)

Dataset <- join(BaselinePD, cv_wts, by = "patno")
str(Dataset)

outVars <- 'cv_wtd_rate'
trMethods <- c("glmnet", 'rf')
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()
i <- 1

for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)

}

Fit

# final Motor Fit

## Assign weights based on Rsquared and number of times chosen
names(combined_abs_rate_optimal_weight_motorFits)

Mresults <- cbind(rbind(motor_w_tries,motor_w_tries),
                  s$statistics$Rsquared[substr(names(allFits), 1, 6) == 'glmnet',3])
head(NMresults)

f <- function(x) (x %*% s$statistics$Rsquared[substr(names(allFits), 1, 6) == 'glmnet',3]) / sum(x)

m_cv_wts <- apply(motor_w_tries, 2, f)
m_cv_wts
sum(m_cv_wts)


cv_wtd_rate <- find_wtsum(motor_rates, w = t(as.matrix(m_cv_wts)))

m_wtd_rates <- data.frame(outcome.rate(outcomes[1], Motor, patno_only = T), cv_wtd_rate)
head(m_wtd_rates)

Dataset <- join(BaselinePD, m_wtd_rates, by = "patno")
str(Dataset)

hist(Dataset$cv_wtd_rate)

outVars <- 'cv_wtd_rate'
trMethods <- c("glmnet", 'rf')
sbfMethods <- NULL
rfeMethods <- NULL

Fit <- list()
i <- 1

for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25)
  
}

Fit




