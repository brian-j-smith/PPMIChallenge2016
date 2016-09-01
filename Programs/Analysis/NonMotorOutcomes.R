## Include Files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

Dataset <- join(BaselinePD, NonMotorV, by = "patno")

str(Dataset)
summary(Dataset)


## Model fitting

outVarsList <- list(
    "Total Recall" = c("1-Year Change" = "dvt_total_recall_diff.V04",
                       "2-Year Change" = "dvt_total_recall_diff.V06"),
    "ESS Total" = c("1-Year Change" = "ess_total_diff.V04",
                    "2-Year Change" = "ess_total_diff.V06"),
    "GDS Total" = c("1-Year Change" = "gds_total_diff.V04",
                    "2-Year Change" = "gds_total_diff.V06"),
    "JLO Total" = c("1-Year Change" = "jlo_totcalc_diff.V04",
                    "2-Year Change" = "jlo_totcalc_diff.V06"),
    "MCA Total" = c("1-Year Change" = "mcatot_diff.V04",
                    "2-Year Change" = "mcatot_diff.V06"), 
    "QUIP Total" = c("1-Year Change" = "quip_total_diff.V04",
                     "2-Year Change" = "quip_total_diff.V06"),
    "REM Total" = c("1-Year Change" = "rem_total_diff.V04",
                    "2-Year Change" = "rem_total_diff.V06"),
    "SCOPA Total" = c("1-Year Change" = "scopa_total_diff.V04",
                      "2-Year Change" = "scopa_total_diff.V06"),
    "STAI Total" = c("1-Year Change" = "stai_total_diff.V04",
                     "2-Year Change" = "stai_total_diff.V06")
)


trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear", "svmRadial")

tuneGrids <- list(
    "nnet" = expand.grid(size=c(1, 3, 5), decay=0.1^(1:4))
)

FitList <- list()
for(outVar in unlist(outVarsList)) {
    
    ## Model inputs and outputs
    fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
    FitList[[outVar]] <- modelfit(
        fo, 
        Dataset, 
        ImpMethod = c("knnImpute"),
        trMethods=trMethods,
        tuneGrids = tuneGrids,
        seed = 123
    )
    
}


## Modelling results

NonMotorSummary <- SummaryTable(FitList, digits = 3)
NonMotorBest <- bestmodel(FitList, metric = "RMSE")


## Analysis results to pass to Shiny trial calc

NonMotorVars <- outVarsList
NonMotorVals <- outValsList(NonMotorBest, digits = 1)


## Save results and data

save(NonMotorSummary, NonMotorBest, NonMotorVars, NonMotorVals, 
     file = "Programs/Analysis/NonMotorOutcomes.RData")
