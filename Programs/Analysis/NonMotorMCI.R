## Include Files and load survival package

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

Dataset <- join(BaselinePD, NonMotorV, by = "patno")
Dataset <- within(
    Dataset, {
        mci.V04 <- factor(mci.V04, 0:1, labels = c("No", "Yes"))
        mci.V06 <- factor(mci.V06, 0:1, labels = c("No", "Yes"))
        dli.V04 <- factor(dli.V04)
        dli.V06 <- factor(dli.V06)
    }
)

str(Dataset)
summary(Dataset)


## Explore distributions of MCI and DLI

table(Dataset$mci.V04, useNA = "always")
table(Dataset$mci.V06, useNA = "always")
table(Dataset$dli.V04, useNA = "always")
table(Dataset$dli.V06, useNA = "always")


## Model fitting

outVars <- c(
    "Mild Cognitive Impairment 1 Year" = "mci.V04",
    "Mild Cognitive Impairment 2 Year" = "mci.V06"
)

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear", "earth", "lda", "nb")
FitMCI <- list()
for (outVar in outVars){
    
    Train <- list()
    
    for (trMethod in trMethods){
        
        fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
        ModelData <- model.data(fo, Dataset, prop.na = 0.2, method = c("zv", "nzv"))
        Train[[trMethod]] <- try(train(
            ModelData$x, ModelData$y,
            method = trMethod,
            preProcess = "knnImpute",
            metric = "ROC",
            trControl = trControlCV(
                summaryFunction = twoClassSummary, 
                classProbs = TRUE                ),
            tuneLength = 3
        ), TRUE)
        
    }
    
    FitMCI[[outVar]] <- list(Train = Train)
}

FitMCI$mci.V04
FitMCI$mci.V06

(FitMCIAUC <- SummaryTable(FitMCI, metric = "ROC"))

save(FitMCI, FitMCIAUC, file = "Programs/Analysis/FitMCI.RData")
