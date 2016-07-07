## Control structures
source("Programs/Analysis/Control.R")


## Analytic dataset

Dataset <- dropfactors(join.ppmi(
  SubjectsBL, MotorBL, MotorV,
  by = "patno",
  subset = (recruitment_cat == "PD" & enroll_status == "Enrolled"),
  select = c(np1total_diff.V04:nptotal_diff.V04,
             gender:bioparpd, np1total:np3total, nhy, mseadlg)
))
str(Dataset)
summary(Dataset)

outvars <- names(Dataset)[1:4]
invars <- setdiff(names(Dataset), outvars)


## Model fitting

Fit <- list()
FitSBF <- list()
FitRFE <- list()

for(outvar in outvars) {
  
  ## Model inputs x and output y
  
  fo <- formula(paste(outvar, "~", paste(invars, collapse=" + ")))
  ModelData <- model.data(fo, Dataset, method = c("zv", "nzv"))
  
  
  ## Fit predictive model
  
  Fit[[outvar]] <- train(
    ModelData$x, ModelData$y,
    method = "lm",
    preProcess = preProcMethod,
    trControl = trControlCV()
  )
  
  
  ## Selection by filtering
  
  FitSBF[[outvar]] <- sbf(
    ModelData$x, ModelData$y,
    method = "lm",
    preProcess = preProcMethod,
    sbfControl = sbfControlCV(0.10)
  )
  
  
  ## Recursive feature extraction (backward selection)
  
  FitRFE[[outvar]] <- rfe(
    ModelData$x, ModelData$y,
    preProcess = preProcMethod,
    rfeControl = rfeControlCV(lmFuncs)
  )

}
