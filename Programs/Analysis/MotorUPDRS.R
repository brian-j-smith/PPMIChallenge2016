## Control structures
source("Programs/Analysis/Control.R")


## Analytic dataset

MotorV$nptotal_auc.V06 <- with(MotorV,
   auc.change(cbind(0, nptotal_diff.V04, nptotal_diff.V06),
              c(0, 12, 24) / 24)
)
  
MotorV$nptotal_auc.V08 <- with(MotorV,
  auc.change(cbind(0, nptotal_diff.V04, nptotal_diff.V06, nptotal_diff.V08),
             c(0, 12, 24, 36) / 36)
)

Dataset <- dropfactors(join.ppmi(
  MotorV, SubjectsBL, MotorBL, Enroll, NonMotorBL, MedHxBL, ImagingBL, BiospecimenBL,
  by = "patno",
  subset = (recruitment_cat == "PD" & enroll_status == "Enrolled"),
  select = c(np1total_diff.V06:nptotal_diff.V06, nptotal_auc.V06, nptotal_auc.V08,
             gender:bioparpd, np1total:np3total, nhy, mseadlg,
             age_at_enroll, time_dx_enroll,
             jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
             dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
             stai_total, dvt_sdm, upsit_total,
             aiputamen:meancaudate,
             rs55785911:rs114138760)
))
str(Dataset)
summary(Dataset)

outvars <- names(Dataset)[1:6]
invars <- c(seq.names(Dataset, "gender", "meancaudate"),
            paste0("as.numeric(", seq.names(Dataset, "rs55785911", "rs114138760"), ")"))


## Model fitting

trMethods <- c("gbm", "glmnet", "glmStepAIC", "nnet", "plsRglm", "rf", "svmLinear")
sbfMethods <- c("glm")
rfeMethods <- c("glm")

Fit <- list()
for(outvar in outvars) {
  
  ## Model inputs and outputs
  fo <- formula(paste(outvar, "~", paste(invars, collapse=" + ")))
  Fit[[outvar]] <- modelfit(fo, Dataset, trMethods=trMethods,
                            sbfMethods=sbfMethods, rfeMethods=rfeMethods)

}
