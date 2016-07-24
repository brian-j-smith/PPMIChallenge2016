## Include files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

MotorV$np1total_auc.V06 <- with(MotorV,
   auc.change(cbind(0, np1total_diff.V04, np1total_diff.V06),
              c(0, 12, 24) / 24)
)
  
MotorV$np1total_auc.V08 <- with(MotorV,
  auc.change(cbind(0, np1total_diff.V04, np1total_diff.V06, np1total_diff.V08),
             c(0, 12, 24, 36) / 36)
)

Dataset <- join(BaselinePD, MotorV, by = "patno")
str(Dataset)
summary(Dataset)


## Model fitting

outVars <- c(
  "np1total_diff.V04", "np2total_diff.V04", "np3total_diff.V04", "nptotal_diff.V04",
  "np1total_diff.V06", "np2total_diff.V06", "np3total_diff.V06", "nptotal_diff.V06",
  "np1total_auc.V06", "np1total_auc.V08"
)
#outVars <- "np1total_diff.V04"

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear")
sbfMethods <- c("glm")
rfeMethods <- c("glm")

Fit <- list()
for(outVar in outVars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods)

}
