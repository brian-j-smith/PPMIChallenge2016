## Include files

source("Programs/Analysis/BaselineData.R")
source("Programs/Analysis/Control.R")


## Analytic dataset

Dataset <- join(BaselinePD, MotorV, by = "patno")

pca <- princomp(na.omit(Dataset[c("np1total", "np2total", "np3total")]))
pc1 <- pca$loadings[,1]
Dataset <- within(Dataset, {
  np1total_auc.V06 <- auc.change(
    cbind(0, np1total_diff.V04, np1total_diff.V06),
    c(0, 12, 24) / 24
  )
  np1total_auc.V06 <- auc.change(
    cbind(0, np1total_diff.V04, np1total_diff.V06, np1total_diff.V08),
    c(0, 12, 24, 36) / 24
  )
  nptotal_pca.V04 <- as.vector(
    cbind(np1total_diff.V04, np2total_diff.V04, np3total_diff.V04) %*% pc1
  )
  nptotal_pca.V06 <- as.vector(
    cbind(np1total_diff.V06, np2total_diff.V06, np3total_diff.V06) %*% pc1
  )
  nptotal_pca.V08 <- as.vector(
    cbind(np1total_diff.V08, np2total_diff.V08, np3total_diff.V08) %*% pc1
  )
})

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

# outVarsList <- list(
#   "MDS-UPDRS I" = c("2-Year AUC" = "np1total_auc.V06",
#                     "3-Year AUC" = "np1total_auc.V08")
# )
# 
# outVarsList <- list(
#   "MDS-UPDRS PCA" = c("1-Year Change" = "nptotal_pca.V04",
#                       "2-Year Change" = "nptotal_pca.V06",
#                       "3-Year Change" = "nptotal_pca.V08")
# )

trMethods <- c("earth", "gbm", "glmnet", "glmStepAIC", "nnet", "pls", "rf",
               "svmLinear", "svmRadial")
sbfMethods <- c("glm")

tuneGrids <- list(
  "glmnet" = expand.grid(alpha=1, lambda=0.1^seq(0, 3, by=0.25)),
  "nnet" = expand.grid(size=c(1, 3, 5), decay=0.1^(1:4))
)

Fit <- list()
for(outVar in unlist(outVarsList)) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, tuneGrids=tuneGrids,
                            seed=123)

}

MotorUPDRSSummary <- SummaryTable(Fit, digits=3)


## Shiny trial design tool data

MotorUPDRSVars <- outVarsList

MotorUPDRSVals <- lapply(bestmodel(Fit), function(fit) {
  data.frame(obs = fit$trainingData$.outcome,
             pred = round(predict(fit), 1))
})

save(MotorUPDRSSummary, MotorUPDRSVars, MotorUPDRSVals,
     file="Programs/Analysis/MotorUPDRS.RData")
