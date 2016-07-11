## Merge non-motor assessments
str(LINEORNT)
str(COGCATG)
str(EPWORTH)
str(GDSSHORT)
str(HVLT)
str(LNSPD)
str(QUIPCS)
str(REMSLEEP)
str(SCOPAAUT)
str(SFT)
str(STAI)
str(SDM)
str(UPSIT)

byvars <- c("patno", "event_id")
Temp <- join_all(
    list(
        LINEORNT[c(byvars, seq.names(LINEORNT, "jlo_totraw", "dvs_jlo_mssae"))],
        COGCATG[c(byvars, seq.names(COGCATG, "cogdecln", "cogdxcl"))],
        EPWORTH[c(byvars, seq.names(EPWORTH, "ess1", "ess8"))],
        GDSSHORT[c(byvars, seq.names(GDSSHORT, "gdssatis", "gdsbeter"))],
        HVLT[c(byvars, seq.names(HVLT, "dvt_total_recall", "dvt_recog_disc_index"))],
        LNSPD[c(byvars, "lns_totraw", "dvs_lns")],
        QUIPCS[c(byvars, seq.names(QUIPCS, "tmgamble", "cntrldsm"))],
        REMSLEEP[c(byvars, seq.names(REMSLEEP, "drmvivid",  "brninfm"))],
        SCOPAAUT[c(byvars, paste0("scau", 1:25))],
        SFT[c(byvars, "vltanim", "vltveg", "vltfruit", "dvs_sftanim", "dvt_sftanim")],
        STAI[c(byvars, seq.names(STAI, "staiad1", "staiad40"))],
        SDM[c(byvars, "dvsd_sdm", "dvt_sdm")],
        UPSIT[c(byvars, seq.names(UPSIT, "upsitbk1", "upsitbk4"))]
    ),
    by = byvars
)


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
NonMotorBL <- subset(NonMotor, event_id == "BL")
NonMotorBL <- NonMotorBL[c("patno",
             "jlo_totraw", "jlo_totcalc", "dvs_jlo_mssa", "dvs_jlo_mssae",
             seq.names(NonMotorBL, "cogdecln", "cogdxcl"),
             "ess_total", "ess_sleepy",
             "gds_total", "gds_deprs",
             seq.names(NonMotorBL, "dvt_total_recall", "dvt_recog_disc_index"),
             "lns_totraw", "dvs_lns",
             "quip_total",
             "rem_total", "rem_slp_dis",
             "scopa_total",
             "dvt_sftanim", "dvs_sftanim",
             "stai_total",
             "dvsd_sdm", "dvt_sdm",
             "upsit_total")]

## Change in assessments from baseline to follow-up visits
Baseline <- NonMotorBL
names(Baseline)[-1] <- paste0(names(Baseline[-1]), "bl")

Temp <- join(subset(NonMotor, substr(event_id, 1, 1) == "V"),
             Baseline,
             by = "patno")

NonMotorDiff <- with(Temp, {
    data.frame(
        patno = patno,
        event_id = event_id,
        jlo_totraw_diff = jlo_totraw - jlo_totrawbl,      
        jlo_totcalc_diff = jlo_totcalc - jlo_totcalcbl,         
        dvs_jlo_mssa_diff = dvs_jlo_mssa - dvs_jlo_mssabl,        
        dvs_jlo_mssae_diff = dvs_jlo_mssae - dvs_jlo_mssaebl,       
        ess_total_diff = ess_total - ess_totalbl,           
        gds_total_diff = gds_total - gds_totalbl,           
        dvt_total_recall_diff = dvt_total_recall - dvt_total_recallbl,    
        dvt_delayed_recall_diff = dvt_delayed_recall - dvt_delayed_recallbl,  
        dvt_retention_diff = dvt_retention - dvt_retentionbl,       
        dvt_recog_disc_index_diff = dvt_recog_disc_index - dvt_recog_disc_indexbl,
        lns_totraw_diff = lns_totraw - lns_totrawbl,          
        dvs_lns_diff = dvs_lns - dvs_lnsbl,             
        quip_total_diff = quip_total - quip_totalbl,          
        rem_total_diff = rem_total - rem_totalbl,            
        scopa_total_diff = scopa_total - scopa_totalbl,         
        dvt_sftanim_diff = dvt_sftanim - dvt_sftanimbl,         
        dvs_sftanim_diff = dvs_sftanim - dvs_sftanimbl,         
        stai_total_diff = stai_total - stai_totalbl,          
        dvsd_sdm_diff = dvsd_sdm - dvsd_sdmbl,            
        dvt_sdm_diff = dvt_sdm - dvt_sdmbl,             
        upsit_total_diff = upsit_total - upsit_totalbl
    )
})

NonMotorDiff <- NonMotorDiff[-c(451,1063),] # remove 2 dup records

NonMotorV <- reshape(
    NonMotorDiff,
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
