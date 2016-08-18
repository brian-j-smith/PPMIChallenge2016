## Merge Medical History data
str(CLINDX)
str(PENEURO)
str(PECN)
str(PDFEAT)
str(SURGPD)
str(VITAL)

table(CLINDX$event_id, useNA = "always")
table(PENEURO$event_id, useNA = "always")
table(PECN$event_id, useNA = "always")
table(PDFEAT$event_id, useNA = "always")
table(SURGPD$event_id, useNA = "always")
table(VITAL$event_id, useNA = "always")

sc_to_bl <- function(data){
    screen <- subset(data, event_id == "SC")$patno
    row_names <- as.numeric(row.names(subset(data, event_id == "SC")))
    baseline <- subset(data, event_id == "BL")$patno
    idx <- row_names[!(screen %in% baseline)]
    VITAL$event_id[idx] <- "BL"
    return(data)
}

CLINDX <- sc_to_bl(CLINDX)
PENEURO <- sc_to_bl(PENEURO)
PECN <- sc_to_bl(PECN)
PDFEAT <- sc_to_bl(PDFEAT)
SURGPD <- sc_to_bl(SURGPD)

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
        CLINDX[c(byvars, seq.names(CLINDX, "dcnomtr", "dxfnathx"))],
        PENEURO[c(byvars, peneuro_vars)],
        PECN[c(byvars, pecn_vars)],
        PDFEAT[c(byvars, "domside", "dxbrady", "dxothsx", "dxposins",
                                  "dxrigid", "dxtremor")],
        SURGPD[c(byvars, "pdsurg", "pdsurgtp", seq.names(SURGPD, "pdsurgsd", "pdslunk"))],
        VITAL[c(byvars, "infodt", seq.names(VITAL, "wgtkg", "tempc"),
                seq.names(VITAL, "syssup", "hrstnd"))] ## using infodt from here
    ),
    by = byvars,
    type = "full"
)

Temp <- ddply(Temp, .(patno), mutate, event_id = ST2V(event_id, infodt))
Temp$infodt <- NULL

pdfeat_vars <- c("domside", "dxbrady", "dxothsx", 
                 "dxposins", "dxrigid", "dxtremor")
Temp[c(pecn_vars, pdfeat_vars)] <- apply(Temp[c(pecn_vars, pdfeat_vars)], 
                            2, 
                            function(x) ifelse(x %in% c("", " ", "U", "u"), NA, x))
Temp[peneuro_vars] <- lapply(Temp[peneuro_vars], factor)
Temp[pecn_vars] <- lapply(Temp[pecn_vars], factor)
Temp[pdfeat_vars] <- lapply(Temp[pdfeat_vars], factor)
Temp[seq.names(Temp, "pdsurg", "pdslunk")] <- lapply(Temp[seq.names(Temp, "pdsurg", "pdslunk")],
                                                     factor)

MedHx <- Temp
MedHx$bmi <- MedHx$wgtkg / (MedHx$htcm / 100)^2


## Baseline assessments
MedHxBL <- subset(MedHx, event_id == "BL", -event_id)


## Change in assessments from baseline to follow-up visits
baseline <- function(x, id) {
    i <- which(id == "BL")
    if(length(i)) x[i] else NA
}

MedHxDiff <- ddply(
    MedHx, .(patno), mutate,
    wgtkg_diff = wgtkg - baseline(wgtkg, event_id),
    tempc_diff = tempc - baseline(tempc, event_id),
    syssup_diff = syssup - baseline(syssup, event_id),
    diasup_diff = diasup - baseline(diasup, event_id),
    hrsup_diff = hrsup - baseline(hrsup, event_id),
    sysstnd_diff = sysstnd - baseline(sysstnd, event_id),
    diastnd_diff = diastnd - baseline(diastnd, event_id),
    hrstnd_diff = hrstnd - baseline(hrstnd, event_id)
)

MedHxV <- reshape(
    subset(MedHxDiff, substr(event_id, 1, 1) == "V",
           select = c(patno, event_id,
                      wgtkg_diff,
                      tempc_diff,
                      syssup_diff,
                      diasup_diff,
                      hrsup_diff,
                      sysstnd_diff,
                      diastnd_diff,
                      hrstnd_diff)
    ),
    idvar = "patno",
    timevar = "event_id",
    direction = "wide"
)

str(MedHx)
str(MedHxBL)
str(MedHxV)

summary(MedHx)
summary(MedHxBL)
summary(MedHxV)

## Save datasets
save(MedHxBL, MedHxV, file = "Data/MedHx.RData")
