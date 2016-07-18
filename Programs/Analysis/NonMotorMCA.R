## Control structures
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

Dataset <- dropfactors(join.ppmi(
    NonMotorV, SubjectsBL, MotorBL, NonMotorBL, MedHxBL, Enroll, ImagingBL, BiospecimenBL,
    by = "patno",
    subset = (recruitment_cat == "PD" & enroll_status == "Enrolled"),
    select = c(mcatot_diff.V04, mcatot_diff.V06, mcatot_diff.V08, 
               mcatot_auc.V06, mcatot_auc.V08,
               gender:bioparpd, np1total:np3total, nhy, mseadlg,
               age_at_enroll, time_dx_enroll,
               aiputamen:meancaudate,
               jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
               dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
               stai_total, dvt_sdm, upsit_total,
               rs55785911:rs114138760)
))
str(Dataset)
summary(Dataset)

outvars <- names(Dataset)[1:5]
invars <- c(seq.names(Dataset, "gender", "upsit_total"),
            paste0("as.numeric(", seq.names(Dataset, "rs55785911", "rs114138760"), ")"))


## Model fitting

FitLM <- list()
FitLM_SBF <- list()
FitLM_RFE <- list()

FitGBM <- list()
FitGLMNET <- list()
FitGLMSTEP <- list()
FitNN <- list()
FitPLS <- list()
FitRF <- list()

for(outvar in outvars) {
    
    ## For testing
    ## outvar <- outvars[1]
    
    ## Model inputs x and output y
    
    fo <- formula(paste(outvar, "~", paste(invars, collapse=" + ")))
    ModelData <- model.data(fo, Dataset, method = c("zv", "nzv"))
    x <- ModelData$x
    y <- ModelData$y
    
    
    ## Fit predictive model
    
    FitLM[[outvar]] <- try(train(
        x, y,
        method = "lm",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## Selection by filtering
    
    FitLM_SBF[[outvar]] <- try(sbf(
        x, y,
        method = "lm",
        preProcess = preProcMethod,
        sbfControl = sbfControlCV(0.20)
    ), TRUE)
    
    
    ## Recursive feature extraction (backward selection)
    
    FitLM_RFE[[outvar]] <- try(rfe(
        x, y,
        method = "lm",
        preProcess = preProcMethod,
        rfeControl = rfeControlCV()
    ), TRUE)
    
    
    ## Generalized boosted modeling
    
    FitGBM[[outvar]] <- try(train(
        x, y,
        method = "gbm",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## GLM with lasso or elsticnet
    
    FitGLMNET[[outvar]] <- try(train(
        x, y,
        method = "glmnet",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## GLM stepwise feature selection
    
    FitGLMSTEP[[outvar]] <- try(train(
        x, y,
        method = "glmStepAIC",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## Neural networks
    
    FitNN[[outvar]] <- try(train(
        x, y,
        method = "nnet",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## Partial least squares
    
    FitPLS[[outvar]] <- try(train(
        x, y,
        method = "pls",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
    
    ## Random forests
    
    FitRF[[outvar]] <- try(train(
        x, y,
        method = "rf",
        preProcess = preProcMethod,
        trControl = trControlCV()
    ), TRUE)
    
}
