## Include Files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

NonMotorV$mcatot_auc.V06 <- with(
    NonMotorV,
    auc.change(cbind(0, mcatot_diff.V04, mcatot_diff.V06),
               c(0, 12, 24) / 24)
)

Dataset <- join(BaselinePD, NonMotorV, by = "patno")

str(Dataset)
summary(Dataset)


## Explore distribution of MCA outcomes
qplot(Dataset$mcatot, geom = "bar")
qplot(Dataset$mcatot_diff.V04, geom = "bar")
qplot(Dataset$mcatot_diff.V06, geom = "bar")
qplot(Dataset$mcatot_auc.V06, geom = "bar")


## Model fitting

outVars <- c(
    "Montreal Cognitive Assessment 1-Year Change" = "mcatot_diff.V04", 
    "Montreal Cognitive Assessment 2-Year Change" = "mcatot_diff.V06", 
    "Montreal Cognitive Assessment 2-Year AUC" = "mcatot_auc.V06"
)

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear", "earth")
sbfMethods <- c("glm")
rfeMethods <- c("glm")


FitMCA <- list()
for(outVar in outVars) {
    
    ## Model inputs and outputs
    fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
    FitMCA[[outVar]] <- modelfit(fo, Dataset, 
                              ImpMethod = c("knnImpute"),
                              trMethods=trMethods,
                              sbfMethods=sbfMethods, 
                              rfeMethods=rfeMethods)
    
}

FitMCA$mcatot_diff.V04
FitMCA$mcatot_diff.V06
FitMCA$mcatot_auc.V06

(FitMCAR2 <- SummaryTable(FitMCA))

save(FitMCA, FitMCAR2, file = "Programs/Analysis/FitMCA.RData")
