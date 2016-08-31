## Include files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

Dataset <- join(BaselinePD, MotorV, by = "patno")
str(Dataset)
summary(Dataset)


## Model fitting

outVarsList <- list(
  "MDS-UPDRS" = c("1-Year Change" = "nptotal_diff.V04",
                  "2-Year Change" = "nptotal_diff.V06"),
  "MDS-UPDRS I" = c("1-Year Change" = "np1total_diff.V04",
                    "2-Year Change" = "np1total_diff.V06"),
  "MDS-UPDRS II" = c("1-Year Change" = "np2total_diff.V04",
                     "2-Year Change" = "np2total_diff.V06"),
  "MDS-UPDRS III" = c("1-Year Change" = "np3total_diff.V04",
                      "2-Year Change" = "np3total_diff.V06")
)

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "pls", "rf", "svmLinear",
               "svmRadial")

tuneGrids <- list(
  "glmnet" = expand.grid(alpha=1, lambda=0.1^seq(0, 3, by=0.25)),
  "nnet" = expand.grid(size=c(1, 3, 5), decay=0.1^(1:4))
)

FitList <- list()
for(outVar in unlist(outVarsList)) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  FitList[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                                tuneGrids=tuneGrids, seed=123)

}


## Summary results

MotorUPDRSSummary <- SummaryTable(FitList, digits=3)
MotorUPDRSBest <- bestmodel(FitList)


## Shiny trial design tool data

MotorUPDRSVars <- outVarsList
MotorUPDRSVals <- outValsList(MotorUPDRSBest, digits=1)


## Save results and data

save(MotorUPDRSSummary, MotorUPDRSBest, MotorUPDRSVars, MotorUPDRSVals,
     file="Programs/Analysis/MotorUPDRS.RData")
