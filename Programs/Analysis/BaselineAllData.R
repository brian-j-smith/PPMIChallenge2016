BaselineAll <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, MotorBL, NonMotorBL, MedHxBL, ImagingBL, BiospecimenBL,
  by = "patno",
  subset = (recruitment_cat %in% c('PD', 'GENPD', 'REGPD', 'PRODROMA')),
  select = c(patno, recruitment_cat, enroll_status, gender:bioparpd,
             age_at_bl, time_dx_bl, 
             np1cog:np1total, nhy, mseadlg,
             jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
             dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
             stai_total, dvt_sdm, upsit_total,
             aiputamen:meancaudate,
             `lsires.RBC Morphology`:`lsires.Urea Nitrogen`, 
             rs55785911:rs114138760)
))

BaselineAll$recruitment_cat <- with(BaselineAll, 
  factor(recruitment_cat, 
     levels = c('PD', 'PRODROMA', 'GENPD','REGPD')))

BaselineAll$enroll_status <- as.numeric(factor(BaselineAll$enroll_status))

BaselineAllVars <- c(
  'as.numeric(recruitment_cat)',
  paste0("`", seq.names(BaselineAll, "enroll_status", "lsires.Urea Nitrogen"), "`"),
  paste0("as.numeric(`", seq.names(BaselineAll, "rs55785911", "rs114138760"), "`)")
)