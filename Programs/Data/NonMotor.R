## Merge non-motor assessments
str(LINEORNT)
str(COGCATG)
str(EPWORTH)
str(GDSSHORT)
str(HVLT)
str(LNSPD)
str(MOCA)
str(QUIPCS)
str(REMSLEEP)
str(SCOPAAUT)
str(SFT)
str(STAI)
str(SDM)
str(UPSIT)


## use MOCA SC assessments as BL
table(MOCA$event_id, useNA = "always")
patno_bl <- subset(MOCA, event_id == "BL")$patno
patno_sc <- subset(MOCA, event_id == "SC")$patno
idx <- which(patno_sc %in% patno_bl)
patno_sc <- patno_sc[idx]
MOCA$event_id <- with(MOCA,
    ifelse(event_id == "SC" & patno != patno_sc, "BL", event_id)   
)
table(MOCA$event_id, useNA = "always")

byvars <- c("patno", "event_id")
Temp <- join_all(
    list(
        subset(LINEORNT, select = c(patno, event_id, jlo_totraw:dvs_jlo_mssae)),
        subset(COGCATG, select = c(patno, event_id, cogdecln:cogdxcl)),
        subset(EPWORTH, select = c(patno, event_id, ess1:ess8)),
        subset(GDSSHORT, select = c(patno, event_id, gdssatis:gdsbeter)),
        subset(HVLT, select = c(patno, event_id, dvt_total_recall:dvt_recog_disc_index)),
        subset(LNSPD, select = c(patno, event_id, lns_totraw, dvs_lns)),
        subset(MOCA, select = c(patno, event_id, mcatot)),
        subset(QUIPCS, select = c(patno, event_id, tmgamble:cntrldsm)),
        subset(REMSLEEP, select = c(patno, event_id, drmvivid:brninfm)),
        subset(SCOPAAUT, select = c(patno, event_id, scau1:scau23, scau24, scau25)),
        subset(SFT, select = c(patno, event_id, vltanim, vltveg, vltfruit, dvs_sftanim, dvt_sftanim)),
        subset(STAI, select = c(patno, event_id, staiad1:staiad40)),
        subset(SDM, select = c(patno, event_id, dvsd_sdm, dvt_sdm)),
        subset(UPSIT, select = c(patno, event_id, upsitbk1:upsitbk4))
    ),
    by = byvars
)


## compute derived variables
rev_score <- function(x){
    4*(x == 1) + 3*(x == 2) +  2*(x == 3) + 1*(x == 4)
}
stai_rev_vars <- c(paste0("staiad", c(1,2,5,8,10,11,15,16,19,20,21,23,26,27,30,33,34,36,39)))
Temp[,stai_rev_vars] <- apply(Temp[,stai_rev_vars], 2, rev_score)
Temp[,c(paste0("scau", 1:25))] <- apply(Temp[,c(paste0("scau", 1:25))], 
                                        2, 
                                        function(x) 3*(x == 9) + x*(x != 9))
NonMotor <- within(Temp, {
    cogdecln <- factor(cogdecln)
    fncdtcog <- factor(fncdtcog)
    cogstate <- factor(cogstate)
    cogdxcl <- factor(cogdxcl)
    tmdismed <- factor(tmdismed)
    cntrldsm <- factor(cntrldsm)
    ess_total <- rowSums(Temp[c(paste0("ess", 1:8))], na.rm = TRUE)
    ess_sleepy <- factor(1*(ess_total >= 10))
    gds_sub_1 <- 1*(gdssatis == 0) + 1*(gdsgspir == 0) + 1*(gdshappy == 0) +
        1*(gdsalive == 0) + 1*(gdsenrgy == 0)
    gds_sub_2 <- rowSums(Temp[seq.names(Temp, "gdsdropd", "gdsbeter")], na.rm = TRUE)
    gds_total <- gds_sub_1 + gds_sub_2
    gds_deprs <- factor(1*(gds_total >= 5))
    quip_sec_a <- 1*(cntrlgmb == 1 | tmgamble == 1)
    quip_sec_b <- 1*(cntrlsex == 1 | tmsex == 1)
    quip_sec_c <- 1*(cntrlbuy == 1 | tmbuy == 1)
    quip_sec_d <- 1*(cntrleat == 1 | tmeat == 1)
    quip_sec_e <- tmtoract + tmtmtact + tmtrwd
    quip_total <- quip_sec_a + quip_sec_b + quip_sec_c + quip_sec_d + quip_sec_e
    rem_sub_1 <- rowSums(Temp[seq.names(Temp, "drmvivid", "slpdstrb")], na.rm = TRUE)
    rem_sub_2 <- 1*(rowSums(Temp[seq.names(Temp, "stroke", "brninfm")], na.rm = TRUE) > 0)
    rem_total <- rem_sub_1 + rem_sub_2
    rem_slp_dis <- factor(1*(rem_total >= 5))
    scopa_total <- rowSums(Temp[c(paste0("scau", 1:25))], na.rm = TRUE)
    stai_sub_1 <- rowSums(Temp[c(paste0("staiad", c(3,4,6,7,9,12,13,14,17,18,22,24,25,
                                                28,29,31,32,35,37,38,40)))], na.rm = TRUE)
    stai_sub_2 <- rowSums(Temp[stai_rev_vars], na.rm = TRUE)
    stai_total <- stai_sub_1 + stai_sub_2
    upsit_total <- rowSums(Temp[seq.names(Temp, "upsitbk1", "upsitbk4")], na.rm = TRUE)
    mci_cond_1 <- 1*(cogdecln == 1)
    mci_cond_2 <- 1*(1*(dvt_total_recall <= 35) + 
        1*(dvt_recog_disc_index <= 35) + 
        1*(dvs_jlo_mssae <= 6) +
        1*(dvs_lns <= 6) +
        1*(dvt_sftanim <= 35) +
        1*(dvt_sdm <= 35) >= 2)
    mci_cond_3 <- 1*(fncdtcog == 0)
    mild_cog_imp <- factor(1*((mci_cond_1 + mci_cond_2 + mci_cond_3) == 3))
})


## Baseline assessments
NonMotorBL <- subset(
    NonMotor,
    event_id == "BL",
    select = c(patno,
               jlo_totraw, jlo_totcalc, dvs_jlo_mssa, dvs_jlo_mssae,
               cogdecln:cogdxcl,
               ess_total, ess_sleepy,
               gds_total, gds_deprs,
               dvt_total_recall:dvt_recog_disc_index,
               lns_totraw, dvs_lns,
               mcatot,
               quip_total,
               rem_total, rem_slp_dis,
               scopa_total,
               dvt_sftanim, dvs_sftanim,
               stai_total,
               dvsd_sdm, dvt_sdm,
               upsit_total)
)


## Change in assessments from baseline to follow-up visits
baseline <- function(x, id) {
    i <- which(id == "BL")
    if(length(i)) x[i] else NA
}

NonMotorDiff <- ddply(
    NonMotor, .(patno), mutate,
    jlo_totraw_diff = jlo_totraw - baseline(jlo_totraw, event_id),      
    jlo_totcalc_diff = jlo_totcalc - baseline(jlo_totcalc, event_id),          
    dvs_jlo_mssa_diff = dvs_jlo_mssa - baseline(dvs_jlo_mssa, event_id),         
    dvs_jlo_mssae_diff = dvs_jlo_mssae - baseline(dvs_jlo_mssae, event_id),        
    ess_total_diff = ess_total - baseline(ess_total, event_id),            
    gds_total_diff = gds_total - baseline(gds_total, event_id),            
    dvt_total_recall_diff = dvt_total_recall - baseline(dvt_total_recall, event_id),
    dvt_delayed_recall_diff = dvt_delayed_recall - baseline(dvt_delayed_recall, event_id), 
    dvt_retention_diff = dvt_retention - baseline(dvt_retention, event_id),        
    dvt_recog_disc_index_diff = dvt_recog_disc_index - baseline(dvt_recog_disc_index, event_id), 
    lns_totraw_diff = lns_totraw - baseline(lns_totraw, event_id),           
    dvs_lns_diff = dvs_lns - baseline(dvs_lns, event_id),    
    mcatot_diff = mcatot - baseline(mcatot, event_id), 
    quip_total_diff = quip_total - baseline(quip_total, event_id),           
    rem_total_diff = rem_total - baseline(rem_total, event_id),             
    scopa_total_diff = scopa_total - baseline(scopa_total, event_id),          
    dvt_sftanim_diff = dvt_sftanim - baseline(dvt_sftanim, event_id),          
    dvs_sftanim_diff = dvs_sftanim - baseline(dvs_sftanim, event_id),          
    stai_total_diff = stai_total - baseline(stai_total, event_id),           
    dvsd_sdm_diff = dvsd_sdm - baseline(dvsd_sdm, event_id),             
    dvt_sdm_diff = dvt_sdm - baseline(dvt_sdm, event_id),              
    upsit_total_diff = upsit_total - baseline(upsit_total, event_id)
)

## remove two duplicate records
if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
} else {
    library(dplyr)
}

NonMotorDiff <- NonMotorDiff %>%
    arrange(patno, event_id, desc(jlo_totraw)) %>%
    group_by(patno, event_id) %>%
    slice(1) %>%
    as.data.frame()


## Convert differences from wide to long
NonMotorV <- reshape(
    subset(NonMotorDiff, substr(event_id, 1, 1) == "V",
           select = c(patno, event_id,
                      jlo_totraw_diff,      
                      jlo_totcalc_diff,          
                      dvs_jlo_mssa_diff,         
                      dvs_jlo_mssae_diff,        
                      ess_total_diff,            
                      gds_total_diff,            
                      dvt_total_recall_diff,
                      dvt_delayed_recall_diff, 
                      dvt_retention_diff,        
                      dvt_recog_disc_index_diff, 
                      lns_totraw_diff,           
                      dvs_lns_diff,    
                      mcatot_diff, 
                      quip_total_diff,           
                      rem_total_diff,             
                      scopa_total_diff,          
                      dvt_sftanim_diff,          
                      dvs_sftanim_diff,          
                      stai_total_diff,           
                      dvsd_sdm_diff,             
                      dvt_sdm_diff,              
                      upsit_total_diff)
    ),
    idvar = "patno",
    timevar = "event_id",
    direction = "wide"
)

str(NonMotor)
str(NonMotorBL)
str(NonMotorV)

summary(NonMotor)
summary(NonMotorBL)
summary(NonMotorV)


## Save datasets
save(NonMotorBL, NonMotorV, file = "Data/NonMotor.RData")
