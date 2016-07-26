## Include Files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

NonMotorV$mcatot_auc.V06 <- with(
    NonMotorV,
    auc.change(cbind(0, mcatot_diff.V04, mcatot_diff.V06),
               c(0, 12, 24) / 24)
)
                           
NonMotorV$mcatot_auc.V08 <- with(
    NonMotorV,
    auc.change(cbind(0, mcatot_diff.V04, mcatot_diff.V06, mcatot_diff.V08),
               c(0, 12, 24, 36) / 36)
)

Dataset <- join(BaselinePD, NonMotorV, by = "patno")

str(Dataset)
summary(Dataset)


## Model fitting
outVars <- c(
    "mcatot_diff.V04", "mcatot_diff.V06", "mcatot_diff.V08", 
    "mcatot_auc.V06", "mcatot_auc.V08"
)

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




