## Merge Medical History data
str(PENEURO)
str(PECN)
str(PDFEAT)
str(PRODDIAG)
str(SURGPD)
str(PDMEDUSE)
str(VITAL)

potent_peneuro_vars <- seq.names(PENEURO, "msrarsp", "plrlcm")
nchar_peneuro <- nchar(potent_peneuro_vars)
peneuro_vars_idx <- substr(potent_peneuro_vars, nchar_peneuro-1, nchar_peneuro) != "cm"
peneuro_vars <- potent_peneuro_vars[peneuro_vars_idx]

potent_pecn_vars <- seq.names(PECN, "cn1rsp", "cn12rsp")
nchar_pecn <- nchar(potent_pecn_vars)
pecn_vars_idx <- substr(potent_pecn_vars, nchar_pecn-1, nchar_pecn) != "cm"
pecn_vars <- potent_pecn_vars[pecn_vars_idx]

byvars <- c("patno", "event_id")
Temp <- join_all(
    list(
        PENEURO[c(byvars, peneuro_vars)],
        PECN[c(byvars, pecn_vars)],
        PDFEAT[c(byvars, seq.names(PDFEAT, "dxtremor", "dxothsx"), "domside")],
        PRODDIAG[c(byvars, "primdiag", "pslvl2")],
        SURGPD[c(byvars, "pdsurg", "pdsurgtp", seq.names(SURGPD, "pdsurgsd", "pdslunk"))],
        PDMEDUSE[c(byvars, seq.names(PDMEDUSE, "pdmedyn", "fulnupdr"))],
        VITAL[c(byvars, seq.names(VITAL, "wgtkg", "tempc"),
                seq.names(VITAL, "syssup", "hrstnd"))]
    ),
    by = byvars,
    type = "full"
)

Temp[c(pecn_vars, "fulnupdr")] <- apply(Temp[c(pecn_vars, "fulnupdr")], 
                                        2, function(x) ifelse(x %in% c("", " ", "U", "u"), NA, x))
Temp[seq.names(Temp, "dxtremor", "domside")] <- apply(Temp[seq.names(Temp, "dxtremor", "domside")], 
                                                      2, function(x) ifelse(x %in% c("", " ", "U", "u"), NA, x))
Temp[peneuro_vars] <- lapply(Temp[peneuro_vars], factor)
Temp[pecn_vars] <- lapply(Temp[pecn_vars], factor)
Temp[seq.names(Temp, "dxtremor", "domside")] <- lapply(Temp[seq.names(Temp, "dxtremor", "domside")],
                                                           factor)
Temp[seq.names(Temp, "pdsurg", "pdslunk")] <- lapply(Temp[seq.names(Temp, "pdsurg", "pdslunk")],
                                                     factor)
Temp[seq.names(Temp, "pdmedyn", "fulnupdr")] <- lapply(Temp[seq.names(Temp, "pdmedyn", "fulnupdr")],
                                                      factor)
MedHx <- within(Temp, {
    primdiag <- factor(primdiag)
    pslvl2 <- factor(pslvl2)
})

## Baseline assessments
MedHxBL <- subset(MedHx, event_id == "BL")
Baseline <- MedHxBL[,-2]
names(Baseline)[-1] <- paste0(names(Baseline[-1]), "bl")

Temp <- join(subset(MedHx, substr(event_id, 1, 1) == "V"),
             Baseline,
             by = "patno")

MedHxV <- with(Temp, {
    data.frame(
        patno = patno,
        event_id = event_id,
        wgtkg_diff = wgtkg - wgtkgbl,
        tempc_diff = tempc - tempcbl,
        syssup_diff = syssup - syssupbl,
        diasup_diff = diasup - diasupbl,
        hrsup_diff = hrsup - hrsupbl,
        sysstnd_diff = sysstnd - sysstndbl,
        diastnd_diff = diastnd - diastndbl,
        hrstnd_diff = hrstnd - hrstndbl
    )
})

str(MedHx)
str(MedHxBL)
str(MedHxV)

summary(MedHx)
summary(MedHxBL)
summary(MedHxV)
