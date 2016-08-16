BaselinePD <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, MotorBL, NonMotorBL, MedHxBL, ImagingBL, BiospecimenBL,
  PDMedUseV,
  by = "patno",
  subset = (recruitment_cat == "PD" & enroll_status == "Enrolled"),
  select = c(patno, gender:bioparpd,
             age_at_bl, time_dx_bl,
             np1total:np3total, nhy, mseadlg,
             jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
             dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
             stai_total, dvt_sdm, upsit_total,
             aiputamen:meancaudate,
             `lsires.RBC Morphology`:`testvalue.Serum IGF-1`, 
             `testvalue.Total Cholesterol`:ttau_abeta,
             pd_med_any.V04, pd_med_any.V06, pd_med_any.V08)
))
str(BaselinePD)
summary(BaselinePD)

BaselinePDVars <- c(
  paste0("`", seq.names(BaselinePD, "gender", "SNCA"), "`"),
  paste0("as.numeric(`", seq.names(BaselinePD, "rs55785911", "rs114138760"), "`)"), 
  paste0("`", seq.names(BaselinePD, "znf746", "pd_med_any.V08"), "`")
)