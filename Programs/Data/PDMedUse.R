## PD medication use
PDMedUse <- join.ppmi(
  mutate(NUPDRS3, pd_med_any = factor(pmin(pd_med_use, 1), 0:1)),
  subset = pag_name == "NUPDRS3",
  select = c(patno, event_id, pd_med_use, pd_med_any),
  ST2V = TRUE
)

PDMedUseV <- reshape(
  PDMedUse,
  idvar = "patno",
  timevar = "event_id",
  direction = "wide"
)


## Summary statistics
str(PDMedUseV)
summary(PDMedUseV)


## Save datasets
save(PDMedUse, PDMedUseV, file="Data/PDMedUse.RData")
