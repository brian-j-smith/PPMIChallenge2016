## Merge MDS-UPDRS/Hoehn & Yahr motor assessments
str(NUPDRS1)
str(NUPDRS1P)
str(NUPDRS2P)
str(NUPDRS3)
str(MODSEADL)

Temp <- join.ppmi(
  subset(NUPDRS1, select = c(patno, event_id, infodt, np1cog:np1dds)),
  subset(NUPDRS1P, select = c(patno, event_id, np1slpn:np1fatg)),
  subset(NUPDRS2P, select = c(patno, event_id, np2spch:np2frez)),
  subset(NUPDRS3, pag_name == "NUPDRS3",
                  select = c(patno, event_id, np3spch:np3rtcon, nhy, pd_med_use)),
  subset(MODSEADL, select = c(patno, event_id, mseadlg)),
  by = c("patno", "event_id"),
  select = -infodt,
  ST2V = TRUE
)

Motor <- within(Temp, {
  np1total <- rowSums(subset(Temp, select = c(np1cog:np1dds, np1slpn:np1fatg)))
  np2total <- rowSums(subset(Temp, select = np2spch:np2frez))
  np3total <- rowSums(subset(Temp, select = np3spch:np3rtcon))
  nptotal <- np1total + np2total + np3total
  pd_med_use <- factor(pd_med_use, 0:6)
})


## Baseline assessments
MotorBL <- subset(Motor, event_id == "BL", -event_id)


## Change in assessments from baseline to follow-up visits
baseline <- function(x, id) {
  i <- which(id == "BL")
  if(length(i)) x[i] else NA
}

MotorDiff <- ddply(Motor, .(patno), mutate,
                   np1total_diff = np1total - baseline(np1total, event_id),
                   np2total_diff = np2total - baseline(np2total, event_id),
                   np3total_diff = np3total - baseline(np3total, event_id),
                   nptotal_diff = nptotal - baseline(nptotal, event_id))

MotorV <- reshape(
  subset(MotorDiff, substr(event_id, 1, 1) == "V",
         c(patno, event_id, np1total_diff, np2total_diff, np3total_diff, nptotal_diff)),
  idvar = "patno",
  timevar = "event_id",
  direction = "wide"
)


## Summary statistics
str(Motor)
str(MotorBL)
str(MotorV)

summary(Motor)
summary(MotorBL)
summary(MotorV)


## Save datasets
save(MotorBL, MotorV, file="Data/Motor.RData")
