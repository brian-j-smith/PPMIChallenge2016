BaselineAll <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, MotorBL, NonMotorBL, MedHxBL, ImagingBL, BiospecimenBL,
  by = "patno",
  select = c(patno, recruitment_cat, enroll_status, gender:bioparpd,
             age_at_bl, time_dx_bl,
             np1total:np3total, nhy, mseadlg,
             jlo_totcalc, ess_total, gds_total, dvt_total_recall:dvt_recog_disc_index,
             dvs_lns, quip_total, rem_total, scopa_total, dvs_sftanim, mcatot,
             stai_total, dvt_sdm, upsit_total,
             aiputamen:meancaudate,
             `lsires.RBC Morphology`:`lsires.Urea Nitrogen`, 
             rs55785911:rs114138760)
))


BaselineAll$PD_status <- with(BaselineAll, ifelse(recruitment_cat %in% c('PD', 'GENPD', 'REGPD'), 1, 0))
BaselineAll$PD_status2 <- with(BaselineAll, ifelse(recruitment_cat == 'PRODROMA', recruitment_cat, PD_status))
BaselineAll$PD_status <- factor(BaselineAll$PD_status, levels = c('0','1'), labels = c('CONTROL', 'PD')) 
BaselineAll$PD_status2 <-  factor(BaselineAll$PD_status2, levels = c('0','1', 'PRODROMA'), labels = c('HC', 'PD', 'PRODROMA')) 
BaselineAll$PD_pheno <- with(BaselineAll, 0 * (PD_status2 == 'HC') + 
                               1 * (PD_status2 == 'PRODROMA') + 
                               2 * (PD_status2 == 'PD'))
BaselineAll$recruitment_cat <- with(BaselineAll, 
                                    as.numeric(factor(recruitment_cat, 
                                                      levels = c('HC', 'PD', 'PRODROMA', 'GENPD',
                                                                 'GENUN', 'REGPD', 'REGUN'))
                                    )
)

BaselineAll$enroll_status <- as.numeric(factor(BaselineAll$enroll_status))
summary(BaselineAll)


BaselineAllVars <- c(
  paste0("`", seq.names(BaselineAll, "recruitment_cat", "lsires.Urea Nitrogen"), "`"),
  paste0("as.numeric(`", seq.names(BaselineAll, "rs55785911", "rs114138760"), "`)"), 
  'PD_pheno', 'as.numeric(PD_status)'
)

myMotorV <- join(MotorV, myOutcome, by = 'patno')
Dataset <- join(BaselineAll, myMotorV, by = "patno")
str(Dataset)


fit0 <- lm(rates ~ PD_pheno, data = Dataset)
summary(fit0)

fit1 <- lm(rates ~ PD_status2, data = Dataset)
summary(fit1)

fit2 <- lm(rates ~ as.numeric(PD_status), data = Dataset)
summary(fit2)

fit3 <- lm(rates ~ recruitment_cat, data = Dataset)
summary(fit3)

fit4 <- lm(rates ~ enroll_status, data = Dataset)
summary(fit4)
