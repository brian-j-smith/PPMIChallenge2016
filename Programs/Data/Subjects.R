## Merge subject characteristics
str(SCREEN)
str(SOCIOECO)
str(FAMHXPD)

Temp <- join_all(
  list(
    SCREEN[c("patno", "gender", seq.names(SCREEN, "hisplat", "ranos"))],
    SOCIOECO[c("patno", "event_id", "educyrs", "handed")],
    FAMHXPD[c("patno", "biomompd", "biodadpd", "magparpd", "pagparpd", "matau",
              "mataupd", "patau", "pataupd", "fulsib", "fulsibpd")]
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
  n <- mataupd + pataupd
  aupdpct <- ifelse(is.na(n), 0, n) / (matau + patau)
  fulsibpdpct <- fulsibpd / fulsib

  rm(biomompd, biodadpd, magparpd, pagparpd, matau, mataupd, patau, pataupd,
     fulsibpd, fulsib, n)
})


## Summary statistics
str(SubjectsBL)

summary(SubjectsBL)
