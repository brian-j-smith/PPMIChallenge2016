## Merge MDS-UPDRS/Hoehn & Yahr motor assessments
str(NUPDRS1)
str(NUPDRS1P)
str(NUPDRS2P)
str(NUPDRS3)
str(MODSEADL)

byvars <- c("patno", "event_id")
Temp <- join_all(
  list(
    NUPDRS1[c(byvars, seq.names(NUPDRS1, "np1cog", "np1dds"))],
    NUPDRS1P[c(byvars, seq.names(NUPDRS1P, "np1slpn", "np1fatg"))],
    NUPDRS2P[c(byvars, seq.names(NUPDRS2P, "np2spch", "np2frez"))],
    NUPDRS3[c(byvars, seq.names(NUPDRS3, "np3spch", "np3rtcon"), "nhy", "pd_med_use")],
    MODSEADL[c(byvars, "mseadlg")]
  ),
  by = byvars
)

Motor <- within(Temp, {
  np1total <- rowSums(Temp[c(seq.names(Temp, "np1cog", "np1dds"),
                             seq.names(Temp, "np1slpn", "np1fatg"))])
  np2total <- rowSums(Temp[seq.names(Temp, "np2spch", "np2frez")])
  np3total <- rowSums(Temp[seq.names(Temp, "np3spch", "np3rtcon")])
  nptotal <- np1total + np2total + np3total
  pd_med_use <- factor(pd_med_use, 0:6)
})


## Baseline assessments
MotorBL <- subset(Motor, event_id == "BL")


## Change in assessments from baseline to follow-up visits
Baseline <- MotorBL[c("patno", "np1total", "np2total", "np3total", "nptotal")]
Baseline <- rename(Baseline, c("np1total" = "np1totalbl",
                               "np2total" = "np2totalbl",
                               "np3total" = "np3totalbl",
                               "nptotal" = "nptotalbl"))

Temp <- join(subset(Motor, substr(event_id, 1, 1) == "V"),
             Baseline,
             by = "patno")

MotorV <- with(Temp, {
  data.frame(
    patno = patno,
    event_id = event_id,
    np1total_diff = np1total - np1totalbl,
    np2total_diff = np2total - np2totalbl,
    np3total_diff = np3total - np3totalbl,
    nptotal_diff = nptotal - nptotalbl
  )
})


## Summary statistics
str(Motor)
str(MotorBL)
str(MotorV)

summary(Motor)
summary(MotorBL)
summary(MotorV)
by(MotorV, MotorV$event_id, summary)
