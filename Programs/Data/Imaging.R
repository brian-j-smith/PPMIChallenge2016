## Imaging
str(ind_av133_metadata) #38 variables, information about image being taken
str(AVIMAG) #32 variables, 
str(ind_av133_sbr) 
str(DATSCAN) 
str(ind_spect_sbr) # 6 variables, caudate and putamen volume
str(ind_datscan_source_data) 
str(ind_mri_source_data) 
str(MRI) 

byvars <- c("patno", "event_id")

### change column names in ind_av133_sbr ###

colnames(ind_av133_sbr)[1] = "patno"
colnames(ind_av133_sbr)[2] = "event_id"

#############################################

ind_av133_sbr = rename(ind_av133_sbr, c("scan_date" = "infodt"))

Temp <- join.ppmi(
  subset(ind_spect_sbr, select = c(patno, event_id, caudate_r, caudate_l, putamen_r, putamen_l)),
  subset(ind_av133_sbr, select = c(patno, event_id, infodt, rcaud.s, rputant.s, 
                                   rputpost.s, lcaud.s, lputant.s, lputpost.s)), 
 # subset(ind_fbb_results, select = c(patno, event_id, cerebellar_cortex_r:mean_whole_cerebellum)),
  by = c("patno", "event_id"), 
  select = -infodt, 
  ST2V = TRUE
)

Imaging <- within(Temp, {
  meancaudate = (Temp$caudate_r + Temp$caudate_l)/2
  meanputamen = (Temp$putamen_r + Temp$putamen_l)/2
  meanstriatum = (Temp$caudate_r + Temp$caudate_l + Temp$putamen_r + Temp$putamen_l)/4
  countdensityratio = meancaudate/meanputamen
  aicaudate = abs(((Temp$caudate_l - Temp$caudate_r) / meancaudate)*100)
  aiputamen = abs(((Temp$putamen_l - Temp$putamen_r) / meanputamen)*100)
})


## Baseline assessments
ImagingBL <- subset(Imaging, event_id == "SC", -event_id)

## Change in assessments from baseline to follow-up visits
Baseline <- ImagingBL[c("patno", seq.names(ImagingBL, "caudate_r", "meancaudate"))]
Baseline <- rename(Baseline, c("rcaud.s" = "rcaud.s_bl",
                               "rputant.s" = "rputant.s_bl",
                               "rputpost.s" = "rputpost.s_bl",
                               "lcaud.s" = "lcaud.s_bl",
                               "lputant.s" = "lputant.s_bl", 
                               "lputpost.s" = "lputpost.s_bl", 
                               "caudate_r" = "caudate_r_bl", 
                               "caudate_l" = "caudate_l_bl", 
                               "putamen_r" = "putamen_r_bl",
                               "putamen_l" = "putamen_l_bl",
                               "aiputamen" = "aiputamen_bl",
                               "aicaudate" = "aicaudate_bl", 
                               "countdensityratio" = "countdensityratio_bl", 
                               "meanstriatum" = "meanstriatum_bl",
                               "meanputamen" = "meanputamen_bl",
                               "meancaudate" = "meancaudate_bl"
                    
                               ))


Temp <- join(subset(Imaging, substr(event_id, 1, 1) == "V"),
             Baseline,
             by = "patno")

ImagingDiff <- with(Temp, {
  data.frame(
    patno = patno,
    event_id = event_id,
    rcaud.s_diff = rcaud.s - rcaud.s_bl,
    rputant.s_diff = rputant.s - rputant.s_bl,
    rputpost.s_diff = rputpost.s - rputpost.s_bl,
    lcaud.s_diff = lcaud.s - lcaud.s_bl,
    lputant.s_diff = lputant.s - lputant.s_bl,
    lputpost.s_diff = lputpost.s - lputpost.s_bl,
    caudate_r_diff = caudate_r - caudate_r_bl,
    caudate_l_diff = caudate_l - caudate_l_bl,
    putamen_r_diff = putamen_r - putamen_r_bl, 
    putamen_l_diff = putamen_l - putamen_l_bl,
    aiputamen_diff = aiputamen - aiputamen_bl, 
    aicaudate_diff = aicaudate - aicaudate_bl, 
    countdensityratio_diff = countdensityratio - countdensityratio_bl, 
    meanstriatum_diff = meanstriatum - meanstriatum_bl, 
    meanputamen_diff = meanputamen - meanputamen_bl, 
    meancaudate_diff = meancaudate - meancaudate_bl
  )
})

ImagingV <- reshape(
  ImagingDiff,
  v.names = c("rcaud.s_diff", "rputant.s_diff", "rputpost.s_diff", "lcaud.s_diff", 
              "lputant.s_diff", "lputpost.s_diff", "caudate_r_diff", "caudate_l_diff", 
              "putamen_r_diff", "putamen_l_diff", "aiputamen_diff", "aicaudate_diff", 
              "countdensityratio_diff", "meanstriatum_diff", "meanputamen_diff", "meancaudate_diff"
        ),
  idvar = "patno",
  timevar = "event_id",
  direction = "wide"
)

## Summary statistics
str(Imaging)
str(ImagingBL)
str(ImagingV)

summary(Imaging)
summary(ImagingBL)
summary(ImagingV)

save(ImagingBL, ImagingV, file="Data/Imaging.RData")



