## Merge subject characteristics
str(SCREEN)
str(SOCIOECO)
str(FAMHXPD)

Temp <- join_all(
  list(
    subset(PATIENT_STATUS, select = c(patno, recruitment_cat, enroll_status)),
    subset(SCREEN, select = c(patno, gender, hisplat:ranos)),
    subset(SOCIOECO, select = c(patno, event_id, educyrs, handed)),
    subset(FAMHXPD, select = c(patno, biomompd, biodadpd, magparpd, pagparpd,
                               matau, mataupd, patau, pataupd, fulsib,
                               fulsibpd))
  ),
  by = "patno"
)

SubjectsBL <- within(Temp, {
  gender <- factor(gender, 0:2)
  hisplat <- factor(hisplat, 0:1)
  raindals <- factor(raindals, 0:1)
  raasian <- factor(raasian, 0:1)
  rablack <- factor(rablack, 0:1)
  rahawopi <- factor(rahawopi, 0:1)
  rawhite <- factor(rawhite, 0:1)
  ranos <- factor(ranos, 0:1)

  handed <- factor(handed, 1:3)

  bioparpd <- biomompd + biodadpd
  gparpd <- magparpd + pagparpd
  n <- matau + patau
  aupdpct <- ifelse(n != 0, (mataupd + pataupd) / n, NA)
  fulsibpdpct <- ifelse(fulsib != 0, fulsibpd / fulsib, NA)

  rm(biomompd, biodadpd, magparpd, pagparpd, matau, mataupd, patau, pataupd,
     fulsibpd, fulsib, n, event_id)
})


## Summary statistics
str(SubjectsBL)

summary(SubjectsBL)


## Save dataset
save(SubjectsBL, file="Data/Subjects.RData")
