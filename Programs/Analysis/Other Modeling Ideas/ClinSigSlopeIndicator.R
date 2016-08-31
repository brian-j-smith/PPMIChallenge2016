# Author: Ryan Peterson
# Date created : 8/09/16 
# Date modified: 8/09/16 
# Description: Significant positive slope after 2 years

require(survival)

source('Programs\\project.R')
source('Programs\\Data\\Motor.R')
source('Programs\\Analysis\\BaselineAllData.R')
source("Programs/Analysis/Control.R")
source("Programs/Analysis/BaselineData.R")

## Choose Tuning parameters
time_on_study_cuttoff <- 0
scale <- 'relative'
clin.sig.level <- 1.01 # points per month if absolute, or pct increase per month if relative
stat.sig.level <- 1

## run models
model_rate <- function(clin.sig.level, stat.sig.level = 1, 
                       scale = 'relative', time_on_study_cuttoff = 0,...) {
  ## Extract Rates
  np1total.rate <- outcome.rate('np1total', Motor, scale = scale)
  np2total.rate <- outcome.rate('np2total', Motor, scale = scale)
  np3total.rate <- outcome.rate('np3total', Motor, scale = scale)
  nptotal.rate <- outcome.rate('nptotal', Motor, scale = scale)
  
  # Extract those who've been on the study more than time cuttoff months
  np1 <- subset(np1total.rate, time_on_study >= time_on_study_cuttoff) 
  np2 <- subset(np2total.rate, time_on_study >= time_on_study_cuttoff) 
  np3 <- subset(np3total.rate, time_on_study >= time_on_study_cuttoff) 
  np <- subset(nptotal.rate,   time_on_study >= time_on_study_cuttoff) 
  
  # Label events as significant based on clin + sig level
  np1$csp_ind <- with(np1, factor(((exp(rates) > clin.sig.level) & p_rates <= stat.sig.level), labels = c('no','yes')))
  np2$csp_ind <- with(np2, factor(((exp(rates) > clin.sig.level) & p_rates <= stat.sig.level), labels = c('no','yes')))
  np3$csp_ind <- with(np3, factor(((exp(rates) > clin.sig.level) & p_rates <= stat.sig.level), labels = c('no','yes')))
  np$csp_ind <-  with(np,  factor(((exp(rates) > clin.sig.level) & p_rates <= stat.sig.level), labels = c('no','yes')))
  
  # Rename and put into dataframe to be joined
  names(np1)[-1] <- paste0('np1', names(np1)[-1])
  names(np2)[-1] <- paste0('np2', names(np2)[-1])
  names(np3)[-1] <- paste0('np3', names(np3)[-1])
  names(np)[-1]  <- paste0('np',  names(np)[-1])
  
  outcomes <- join(np1, np2, by = 'patno')
  outcomes <- join(outcomes, np3, by = 'patno')
  outcomes <- join(outcomes, np, by = 'patno')
  outcomes <- outcomes[c('patno', 'np1csp_ind', 'np2csp_ind','np3csp_ind','npcsp_ind')]
  head(outcomes)
  cat('Full data set: increasing by a factor of', 1 + (clin.sig.level-1)*12, 'per year?','\n')
  print(apply(outcomes[,-1], 2, table))
  
  ## Incorporate into dataset
  Dataset <- join(BaselinePD, outcomes, by = "patno")

  cat('Modeling data set: increasing by a factor of', 1 + (clin.sig.level-1)*12, 'per year?','\n')
  print(apply(Dataset[c('np1csp_ind', 'np2csp_ind','np3csp_ind','npcsp_ind')], 2, table))
  
  outVars <- names(outcomes)[-1]
  
  trMethods <- c('rf', "glmnet", 'gbm', 'svmLinear', 'lda', 'pls')
  sbfMethods <- NULL
  rfeMethods <- NULL
  
  Fit <- list()
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  for(outVar in outVars) {
    
    ## Model inputs and outputs
    fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
    Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                              sbfMethods=sbfMethods, rfeMethods=rfeMethods,
                              trControl = fitControl,
                              metric = 'ROC')
    
  }
  
  bestfit <- character(4)
  bestROC <- numeric(4)
  
  np1_csp_ind_models <- Fit[['np1csp_ind']]$Train
  (resamps <- resamples(np1_csp_ind_models))
  (s <- summary(resamps))
  print(dotplot(resamps, main = paste0('np1_csp_ind, csp = ', clin.sig.level, '/month')))
  bestfit[1] <- names(which.max(s$statistics$ROC[,3]))
  bestROC[1] <- max(s$statistics$ROC[,3])
  
  np2_csp_ind_models <- Fit[['np2csp_ind']]$Train
  (resamps <- resamples(np2_csp_ind_models))
  s <- summary(resamps)
  print(dotplot(resamps, main = paste0('np2_csp_ind, csp = ', clin.sig.level, '/month')))
  bestfit[2] <- names(which.max(s$statistics$ROC[,3]))
  bestROC[2] <- max(s$statistics$ROC[,3])
  
  np3_csp_ind_models <- Fit[['np3csp_ind']]$Train
  (resamps <- resamples(np3_csp_ind_models))
  s <- summary(resamps)
  print(dotplot(resamps, main = paste0('np3_csp_ind, csp = ', clin.sig.level, '/month')))
  bestfit[3] <- names(which.max(s$statistics$ROC[,3]))
  bestROC[3] <- max(s$statistics$ROC[,3])
  
  np_csp_ind_models <- Fit[['npcsp_ind']]$Train
  (resamps <- resamples(np_csp_ind_models))
  s <- summary(resamps)
  print(dotplot(resamps, main = paste0('np_csp_ind, csp = ', clin.sig.level, '/month')))
  bestfit[4] <- names(which.max(s$statistics$ROC[,3]))
  bestROC[4] <- max(s$statistics$ROC[,3])
  
  return(list(Fit = Fit, bestfit = bestfit, bestROC = bestROC))
}

csp_tries <- 1 + c(.025, .05, .1, .15, .25, .5)/12
Fits <- list()
bestROCs <- matrix(nrow = length(csp_tries), ncol = 4)
bestfits <- matrix(nrow = length(csp_tries), ncol = 4)

colnames(bestROCs) <- colnames(bestfits) <- c('np1total', 'np2total', 'np3total', 'nptotal')

for(i in 1:length(csp_tries)) {
  model_rates <- model_rate(csp_tries[i])
  bestROCs[i,] <- model_rates$bestROC
  bestfits[i,] <- model_rates$bestfit
  #Fits <- list(model_rates$Fit, Fits)
}
par(mfrow = c(2,2))
for(i in 1:4) print(plot(csp_tries, bestROCs[,i], main = colnames(bestROCs)[i]))