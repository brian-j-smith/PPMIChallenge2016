## Include files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")

## Analytic dataset

ImagingV$meanstriatum_auc.V06 <- with(ImagingV,
                                auc.change(cbind(0, meanstriatum_diff.V04, meanstriatum_diff.V06),
                                           c(0, 12, 24) / 24)
)
ImagingV$meancaudate_auc.V06 <- with(ImagingV,
                                      auc.change(cbind(0, meancaudate_diff.V04, meancaudate_diff.V06),
                                                 c(0, 12, 24) / 24)
)
ImagingV$meanputamen_auc.V06 <- with(ImagingV,
                                      auc.change(cbind(0, meanputamen_diff.V04, meanputamen_diff.V06),
                                                 c(0, 12, 24) / 24)
)
ImagingV$countdensityratio_auc.V06 <- with(ImagingV,
                                      auc.change(cbind(0, countdensityratio_diff.V04, countdensityratio_diff.V06),
                                                 c(0, 12, 24) / 24)
)
ImagingV$aicaudate_auc.V06 <- with(ImagingV,
                                      auc.change(cbind(0, aicaudate_diff.V04, aicaudate_diff.V06),
                                                 c(0, 12, 24) / 24)
)
ImagingV$aiputamen_auc.V06 <- with(ImagingV,
                                   auc.change(cbind(0, aiputamen_diff.V04, aiputamen_diff.V06),
                                              c(0, 12, 24) / 24)
)

##### adding in PCA #####
Dataset <- join(BaselinePD, ImagingV, by = "patno")

pca <- princomp(na.omit(Dataset[c("aiputamen", "countdensityratio", "meanstriatum")]))
pc1 <- pca$loadings[,1]
Dataset <- within(Dataset, {
  imaging_pca.V04 <- as.vector(
    cbind(aiputamen_diff.V04, countdensityratio_diff.V04, meanstriatum_diff.V04) %*% pc1
  )
  imaging_pca.V06 <- as.vector(
    cbind(aiputamen_diff.V06, countdensityratio_diff.V06, meanstriatum_diff.V06) %*% pc1
  )
})

## Model fitting

outVarsList <- list(
  "Mean Striatum" = c("1-Year Change" = "meanstriatum_diff.V04",
                      "2-Year Change" = "meanstriatum_diff.V06",
                      "1-Year % Change" = "meanstriatum_perchange.V04",
                      "2-Year % Change" = "meanstriatum_perchange.V06"),
  "Mean Putamen" = c("1-Year Change" = "meanputamen_diff.V04",
                     "2-Year Change" = "meanputamen_diff.V06",
                     "1-Year % Change" = "meanputamen_perchange.V04",
                     "2-Year % Change" = "meanputamen_perchange.V06"),
  "CDR" =  c("1-Year Change" = "countdensityratio_diff.V04",
             "2-Year Change" = "countdensityratio_diff.V06",
             "2-Year AUC" = "countdensityratio_auc.V06"),
  "AI Putamen" = c("1-Year Change" = "aiputamen_diff.V04",
                   "2-Year Change" = "aiputamen_diff.V06"),
  "AI Putamen AUC" = c("2-Year AUC" = "aiputamen_auc.V06"),
  "AI Caudate" = c("1-Year Change" = "aicaudate_diff.V04",
                   "2-Year Change" = "aicaudate_diff.V06"),
  "AI Caudate AUC" = c("2-Year AUC" = "aicaudate_auc.V06"),
  "Imaging PCA" = c("1-Year Change" = "imaging_pca.V04",
                    "2-Year Change" = "imaging_pca.V06")
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
                                 tuneGrids=tuneGrids,
                                seed=123)
  
}


## Summary results

ImagingSummary <- SummaryTable(FitList, digits=3)
ImagingBest <- bestmodel(FitList)


## Shiny trial design tool data

ImagingVars <- outVarsList
ImagingVals <- outValsList(ImagingBest, digits=1)


## Save results and data

save(ImagingSummary, ImagingBest, ImagingVars, ImagingVals,
     file="Programs/Analysis/Imaging.RData")
