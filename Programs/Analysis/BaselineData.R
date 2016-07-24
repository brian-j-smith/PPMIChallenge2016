BaselinePD <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, MotorBL, NonMotorBL, MedHxBL, ImagingBL, BiospecimenBL,
  by = "patno",
  subset = (recruitment_cat == "PD" & enroll_status == "Enrolled"),
  select = c(patno, gender:bioparpd,
             age_at_bl, time_dx_bl,
             np1total:np3total, nhy, mseadlg,
             jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
             dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
             stai_total, dvt_sdm, upsit_total,
             aiputamen:meancaudate,
             `lsires.RBC Morphology`:`lsires.Urea Nitrogen`, ApoE,
             rs55785911:rs114138760)
))
str(BaselinePD)
summary(BaselinePD)

BaselinePDVars <- c(
  paste0("`", seq.names(BaselinePD, "gender", "ApoE"), "`"),
  paste0("as.numeric(`", seq.names(BaselinePD, "rs55785911", "rs114138760"), "`)")
)
