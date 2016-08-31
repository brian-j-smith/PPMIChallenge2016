# Author: Ryan Peterson
# Date created : 8/01/16 
# Date modified: 8/08/16 
# Description: Rate of Change calculation and Modeling

source('Programs\\project.R')
source('Programs\\Data\\Motor.R')
source('Programs\\Analysis\\BaselineData.R')
source("Programs/Analysis/Control.R")

## On the absolute scale
scale <- 'absolute'

np1 <- outcome.rate('np1total', Motor, scale = scale)
np2 <- outcome.rate('np2total', Motor, scale = scale)
np3 <- outcome.rate('np3total', Motor, scale = scale)
np <-  outcome.rate('nptotal', Motor,  scale = scale)

names(np1)[-1] <- paste0(scale, '_np1_', names(np1)[-1])
names(np2)[-1] <- paste0(scale, '_np2_', names(np2)[-1])
names(np3)[-1] <- paste0(scale, '_np3_', names(np3)[-1])
names(np)[-1]  <- paste0(scale, '_np_',  names(np)[-1])

outcomes <- join(np1, np2, by = 'patno')
outcomes <- join(outcomes, np3, by = 'patno')
outcomes <- join(outcomes, np, by = 'patno')
outcomes <- outcomes[c('patno', 'absolute_np1_rates', 'absolute_np2_rates', 
                       'absolute_np3_rates', 'absolute_np_rates')]

head(outcomes)

## Now for relative scale
scale <- 'relative'

np1 <- outcome.rate('np1total', Motor, scale = scale)
np2 <- outcome.rate('np2total', Motor, scale = scale)
np3 <- outcome.rate('np3total', Motor, scale = scale)
np <-  outcome.rate('nptotal', Motor,  scale = scale)

names(np1)[-1] <- paste0(scale, '_np1_', names(np1)[-1])
names(np2)[-1] <- paste0(scale, '_np2_', names(np2)[-1])
names(np3)[-1] <- paste0(scale, '_np3_', names(np3)[-1])
names(np)[-1]  <- paste0(scale, '_np_',  names(np)[-1])

outcomes <- join(outcomes, np1, by = 'patno')
outcomes <- join(outcomes, np2, by = 'patno')
outcomes <- join(outcomes, np3, by = 'patno')
outcomes <- join(outcomes, np,  by = 'patno')
outcomes <- outcomes[c('patno', 
                       'absolute_np1_rates', 'absolute_np2_rates', 
                       'absolute_np3_rates', 'absolute_np_rates', 
                       'relative_np1_rates', 'relative_np2_rates', 
                       'relative_np3_rates', 'relative_np_rates')]

head(outcomes)

Dataset <- join(BaselinePD, outcomes, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- c('absolute_np1_rates', 'absolute_np2_rates', 
             'absolute_np3_rates', 'absolute_np_rates', 
             'relative_np1_rates', 'relative_np2_rates', 
             'relative_np3_rates', 'relative_np_rates')

trMethods <- c("gbm", "glmnet", "glmStepAIC", "pls", "rf", "svmLinear")
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
  #readline('Press Enter to continue\n')
}
maxR
bestMod
bestVar
UPDRS_rateFits <- Fit
save(UPDRS_rateFits, file = 'Programs/Analysis/Models/UPDRS_rateFits.RData')

## Dichotomizing Rates
# Cut at average rate among all subjs
outVars <- c('absolute_np1_rates', 'absolute_np2_rates', 
             'absolute_np3_rates', 'absolute_np_rates', 
             'relative_np1_rates', 'relative_np2_rates', 
             'relative_np3_rates', 'relative_np_rates')

names(outcomes)
cut <- apply(outcomes[,2:9], 2, mean, na.rm = T)
newVars <- c()

for(i in 1:length(outVars)){
  newVar <- paste0(substr(outVars[i], 1, 12), '_cut_at_', round(cut[i],3))
  newVar <- sub('-','neg', newVar)
  newVars <- c(newVars, newVar)
  outcomes[newVar] <- factor(ifelse(outcomes[outVars[i]] > cut, 'Progressing', 'NotProgressing'))
}

head(outcomes)

Dataset <- join(BaselinePD, outcomes, by = "patno")
str(Dataset)

## Try caret 

## Model fitting
outVars <- newVars
outVars
trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear")
sbfMethods <- NULL
rfeMethods <- NULL

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

Fit <- list()
for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                            prop.na=0.25, trControl = fitControl, metric = 'ROC')
}

Fit


maxROC <- 0
bestMod <- ''

for(outVar in outVars) {
  resamps <- resamples(Fit[[outVar]]$Train)
  s <- summary(resamps)
  best <- max(s$statistics$ROC[,3])
  
  if(best > maxROC) {
    maxROC <- best
    bestMod <- names(which.max(s$statistics$ROC[,3]))
    bestVar <- outVar
  }
  print(dotplot(resamps, main = outVar))
  #readline('Press Enter to continue\n')
}
maxROC
bestMod
bestVar

UPDRS_rateDichotomized_at_mean_Fits <- Fit
save(UPDRS_rateDichotomized_at_mean_Fits, file = 'Programs/Analysis/Models/UPDRS_rateDichotomized_at_mean_Fits.RData')

