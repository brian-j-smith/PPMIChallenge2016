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
  "MDS-UPDRS-I" = c("1-Year Change" = "np1total_diff.V04",
                    "2-Year Change" = "np1total_diff.V06"),
  "MDS-UPDRS-II" = c("1-Year Change" = "np2total_diff.V04",
                     "2-Year Change" = "np2total_diff.V06"),
  "MDS-UPDRS-III" = c("1-Year Change" = "np3total_diff.V04",
                      "2-Year Change" = "np3total_diff.V06")
)

# outVars <- c("np1total_auc.V06", "np1total_auc.V08")
# outVars <- c("nptotal_pca.V04", "nptotal_pca.V06", "nptotal_pca.V08")
# outVars <- "cut(np1total_diff.V04, c(-Inf, -1.5, 1.5, Inf))"
# outVars <- "np1total_diff.V04"

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear")
sbfMethods <- c("glm")
rfeMethods <- c("glm")

Fit <- list()
for(outVar in unlist(outVarsList)) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outVar, "~", paste(BaselinePDVars, collapse=" + ")))
  Fit[[outVar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods)

}


## Shiny trial design tool data

OutcomeVars <- outVarsList

OutcomeVals <- lapply(Fit, function(outVar) {
  fit <- outVar$Train$glmnet
  data.frame(obs = fit$trainingData$.outcome,
             pred = round(predict(fit), 1))
})

save(OutcomeVars, OutcomeVals, file="shiny/Outcomes.RData")
