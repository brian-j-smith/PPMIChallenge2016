### merge Biospecimen

str(Biospecimen_Analysis) #13 variables
str(COVANCE) #24 variables
str(CLINLAB) #13 variables
str(DNA) #14 variables
str(MUTRSLT) #13 variables
str(LAB) #60 variables
str(LUMBAR) #51 variables
str(SKBIO) #22 variables
str(WHOLEBLD) #13 variables

### fixing Biospecimen_Analysis clinical_event to event_id

for (i in 1:length(Biospecimen_Analysis$clinical_event)){
  if (Biospecimen_Analysis$clinical_event[i] == "Baseline Collection"){
    Biospecimen_Analysis$clinical_event[i] = "BL"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Screening Visit") {
    Biospecimen_Analysis$clinical_event[i] = "SC"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Visit 01"){
    Biospecimen_Analysis$clinical_event[i] = "V01"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Visit 02"){
    Biospecimen_Analysis$clinical_event[i] = "V02"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Visit 03"){
    Biospecimen_Analysis$clinical_event[i] = "V03"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Visit 04"){
    Biospecimen_Analysis$clinical_event[i] = "V04"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Visit 06"){
    Biospecimen_Analysis$clinical_event[i] = "V06"
  } else if (Biospecimen_Analysis$clinical_event[i] == "Unscheduled 1"){
    Biospecimen_Analysis$clinical_event[i] = "U01"
  }
}

colnames(Biospecimen_Analysis)[4] = "event_id"

##############################################

### check units in Biospecimen_Analysis

# uniq_units = rep(NA, 78)
# for (i in 1:length(unique(Biospecimen_Analysis$testname))) {
#   uniq_units[i] = length(unique(Biospecimen_Analysis$units[which(Biospecimen_Analysis$testname == Biospecimen_Analysis$testname[i] )]))
#   }

##############################################


### reduce data to only BL and SC
BioBLSC = Biospecimen_Analysis[which(Biospecimen_Analysis$event_id == "BL" | Biospecimen_Analysis$event_id == "SC"),]

# change NA units to none
BioBLSC["units"][is.na(BioBLSC["units"])] <- "none"

#remove RNA standard deviations
BioBLSC = BioBLSC[which(BioBLSC$units != "SD"),]

#change character responces to NA
BioBLSC[BioBLSC$testname == "CSF Hemoglobin","testvalue"] = as.numeric(BioBLSC[which(BioBLSC$testname == "CSF Hemoglobin") , "testvalue"])

#remove unwanted columns
col_to_remove = c(seq.names(BioBLSC, "gender", "event_id"), 
                  seq.names(BioBLSC,"units", "update_stamp")
                 )

BioBLSC <- BioBLSC[, !(colnames(BioBLSC) %in% col_to_remove)]

### divide into character and numeric sets

test_with_dups = c("CSF Hemoglobin", "GLT25D1", "GUSB", "MON1B", "RPL13", 
                   "SRCAP", "UBC", "DHPR", "DJ-1", "FBXO7-001", 
                   "FBXO7-005", "FBXO7-007", "FBXO7-007", "FBXO7-008", "FBXO7-010",
                   "SNCA-007", "SNCA-3UTR-1", "SNCA-3UTR-2", "SNCA-E3E4", "SNCA-E4E6",
                   "ZNF746", "CSF Alpha-synuclein", "Total tau", "Abeta 42", "p-Tau181P")

BioOK = BioBLSC[!(BioBLSC$testname %in% test_with_dups), ]

BioDups = BioBLSC[BioBLSC$testname %in% test_with_dups, ]

#average duplicates

BioDups[,"testvalue"] = as.numeric(BioDups[,"testvalue"])


Temp = aggregate(BioDups[BioDups$testname %in% test_with_dups, "testvalue"],
          by = list(BioDups[BioDups$testname %in% test_with_dups,"patno"],
                    BioDups[BioDups$testname %in% test_with_dups,"type"],
                    BioDups[BioDups$testname %in% test_with_dups,"testname"]
                   ), FUN = mean )

colnames(Temp) = c("patno", "type", "testname", "testvalue")
  
  
# long to wide
Bio_wide1 <- reshape(
  Temp,
  v.names = c("testvalue"),
  idvar = c("patno"),
  timevar = c("testname"),
  direction = "wide"
)

Bio_wide2 <- reshape(
  BioOK,
  v.names = c("testvalue"),
  idvar = c("patno"),
  timevar = c("testname"),
  direction = "wide"
)

### merge

Bio_wide = merge(Bio_wide1, Bio_wide2, by = "patno")

#remove unnecessary vars and convert to numeric

Bio_wide = Bio_wide[, -c(2,27)]

Bio_wide[, 27:33] <- sapply(Bio_wide[, 27:33], as.numeric)
Bio_wide[, 67] <- sapply(Bio_wide[, 67], as.numeric)
Bio_wide[, 69:78] <- sapply(Bio_wide[, 69:78], as.numeric)


Bio_wide[, "ApoE"] = pmin(Bio_wide[,"testvalue.APOE GENOTYPE"], 
                        Bio_wide[,"testvalue.ApoE Genotype"], na.rm=TRUE)

# which( colnames(Bio_wide)=="testvalue.SCORE" )
# which( colnames(Bio_wide)=="testvalue.SOD2" )
# which( colnames(Bio_wide)=="testvalue.PTBP1" )

### Nanostring Data

controls = c("testvalue.UBC", "testvalue.GUSB", "testvalue.MON1B", "testvalue.SRCAP", "testvalue.GLT25D1", "testvalue.RPL13")
Bio_wide$s = rowSums(Bio_wide[, controls])
Bio_wide$m = sum(Bio_wide$s, na.rm=T)

Temp_bio = within(Bio_wide, {
  ttau_abeta = Bio_wide$`testvalue.Total tau`/Bio_wide$`testvalue.Abeta 42`
  ptau_abeta = Bio_wide$`testvalue.p-Tau181P`/Bio_wide$`testvalue.Abeta 42`
  ptau_ttau = Bio_wide$`testvalue.p-Tau181P`/Bio_wide$`testvalue.Total tau`
  sod2 = 2**(-1*(Bio_wide$testvalue.SOD2 - Bio_wide$testvalue.GAPDH))
  app = 2**(-1*(Bio_wide$testvalue.APP - Bio_wide$testvalue.GAPDH))
  copz1 = 2**(-1*(Bio_wide$testvalue.COPZ1 - Bio_wide$testvalue.GAPDH))
  hnf4a = 2**(-1*(Bio_wide$testvalue.HNF4A - Bio_wide$testvalue.GAPDH))
  c5orf4 = 2**(-1*(Bio_wide$testvalue.C5ORF4 - Bio_wide$testvalue.GAPDH))
  wls = 2**(-1*(Bio_wide$testvalue.WLS - Bio_wide$testvalue.GAPDH))
  eftud2 = 2**(-1*(Bio_wide$testvalue.EFTUD2 - Bio_wide$testvalue.GAPDH))
  ptbp1 = 2**(-1*(Bio_wide$testvalue.PTBP1 - Bio_wide$testvalue.GAPDH))
  znf160 = 2**(-1*(Bio_wide$testvalue.ZNF160 - Bio_wide$testvalue.GAPDH))
  dhpr = Bio_wide$testvalue.DHPR * Bio_wide$m / Bio_wide$s 
  dj1 = Bio_wide$`testvalue.DJ-1` * Bio_wide$m / Bio_wide$s 
  fbx07001 = Bio_wide$`testvalue.FBXO7-001` * Bio_wide$m / Bio_wide$s 
  fbx07005 = Bio_wide$`testvalue.FBXO7-005` * Bio_wide$m / Bio_wide$s 
  fbx07007 = Bio_wide$`testvalue.FBXO7-007` * Bio_wide$m / Bio_wide$s 
  fbx07008 = Bio_wide$`testvalue.FBXO7-008` * Bio_wide$m / Bio_wide$s 
  fbx07010 = Bio_wide$`testvalue.FBXO7-010` * Bio_wide$m / Bio_wide$s 
  snca007 = Bio_wide$`testvalue.SNCA-007` * Bio_wide$m / Bio_wide$s 
  snca3utr1 = Bio_wide$`testvalue.SNCA-3UTR-1` * Bio_wide$m / Bio_wide$s 
  snca3utr2 = Bio_wide$`testvalue.SNCA-3UTR-2` * Bio_wide$m / Bio_wide$s 
  sncae3e4 = Bio_wide$`testvalue.SNCA-E3E4` * Bio_wide$m / Bio_wide$s 
  sncae4e6 = Bio_wide$`testvalue.SNCA-E4E6` * Bio_wide$m / Bio_wide$s 
  znf746 = Bio_wide$testvalue.ZNF746 * Bio_wide$m / Bio_wide$s 
  
  rs114138760 = as.factor(testvalue.rs114138760)
  rs76763715_GBA_p.N370S = as.factor(testvalue.rs76763715_GBA_p.N370S)
  
  rs71628662 = as.factor(testvalue.rs71628662)
  rs823118  = as.factor(testvalue.rs823118)
  rs10797576 = as.factor(testvalue.rs10797576)
  rs6430538 = as.factor(testvalue.rs6430538)
  rs1955337 = as.factor(testvalue.rs1955337)
  rs12637471 = as.factor(testvalue.rs12637471)
  
  rs34884217 = as.factor(testvalue.rs34884217)
  rs34311866 = as.factor(testvalue.rs34311866)
  rs11724635 = as.factor(testvalue.rs11724635)
  rs6812193 = as.factor(testvalue.rs6812193)
  rs356181 = as.factor(testvalue.rs356181)
  
  rs3910105 = as.factor(testvalue.rs3910105)
  rs8192591 = as.factor(testvalue.rs8192591)
  rs115462410 = as.factor(testvalue.rs115462410)
  rs199347 = as.factor(testvalue.rs199347)
  rs591323 = as.factor(testvalue.rs591323)
  
  rs118117788 = as.factor(testvalue.rs118117788)
  rs329648 = as.factor(testvalue.rs329648)
  rs76904798 = as.factor(testvalue.rs76904798)
  rs34995376_LRRK2_p.R1441H = as.factor(testvalue.rs34995376_LRRK2_p.R1441H)
  rs35801418_LRRK2_p.Y1699C = as.factor(testvalue.rs35801418_LRRK2_p.Y1699C)
  
  rs34637584_LRRK2_p.G2019S = as.factor(testvalue.rs34637584_LRRK2_p.G2019S)
  rs35870237_LRRK2_p.I2020T = as.factor(testvalue.rs35870237_LRRK2_p.I2020T)
  rs11060180 = as.factor(testvalue.rs11060180)
  rs11158026 = as.factor(testvalue.rs11158026)
  rs2414739 = as.factor(testvalue.rs2414739)
  
  rs14235 = as.factor(testvalue.rs14235)
  rs11868035 = as.factor(testvalue.rs11868035)
  rs17649553 = as.factor(testvalue.rs17649553)
  rs12456492 = as.factor(testvalue.rs12456492)
  rs55785911 = as.factor(testvalue.rs55785911)
  ApoE = as.factor(ApoE)
  SNCA = as.factor(testvalue.SNCA_multiplication)
  })


### remove columns where raw data is not needed 
col_to_remove = c(seq.names(Temp_bio, "testvalue.DHPR", "testvalue.Total tau"), 
                  seq.names(Temp_bio,"testvalue.ZNF746", "testvalue.ApoE Genotype"), 
                  seq.names(Temp_bio, "testvalue.rs114138760", "testvalue.rs55785911"), 
                   "testvalue.SNCA_multiplication",
                  seq.names(Temp_bio, "testvalue.SOD2", "testvalue.APOE GENOTYPE"),
                  "s", "m")

Temp_bio <- Temp_bio[, !(colnames(Temp_bio) %in% col_to_remove)]

##############################################
View(COVANCE)
##############################################

### check units in COVANCE

# uniq_units = rep(NA, 41)
# for (i in 1:length(unique(COVANCE$ltstname))) {
#   uniq_units[i] = length(unique(COVANCE$lsiunit[which(COVANCE$ltstname == COVANCE$ltstname[i] )]))
#   }

### reduce data to only BL and SC
COVANCESC = COVANCE[which(COVANCE$event_id == "RETEST" | COVANCE$event_id == "SC"),]

### remove unnecessary columns 
col_to_remove = c(seq.names(COVANCESC, "lcolldt", "lgroup"), "lvistype",
                  seq.names(COVANCESC,"lsilorng", "ltstcomm"), "ltstcode", "lsiunit", "pag_name")

COVANCESC <- COVANCESC[, !(colnames(COVANCESC) %in% col_to_remove)]
COVANCESC= COVANCESC[!(COVANCESC$lsires == ""),]



### divide into character and numeric sets

test_with_dups = c("Prothrombin Time", "APTT-QT", "Monocytes", "Eosinophils", "Basophils", 
                   "Platelets", "Neutrophils (%)", "Lymphocytes (%)", "Monocytes (%)", "Eosinophils (%)", 
                   "Basophils (%)", "Hematocrit", "RBC", "Hemoglobin", "WBC",
                   "Neutrophils", "Lymphocytes", "Total Bilirubin", "Serum Glucose", "Total Protein",
                   "Albumin-QT", "Alkaline Phosphatase", "Serum Sodium", "Serum Bicarbonate", "Serum Chloride",
                   "Calcium (EDTA)", "Creatinine (Rate Blanked)", "ALT (SGPT)", "AST (SGOT)", "Urea Nitrogen",
                   "Serum Uric Acid", "Atypical Lymphocytes", "Serum Potassium")

CovOK = COVANCESC[!(COVANCESC$ltstname %in% test_with_dups), ]

CovDups = COVANCESC[COVANCESC$ltstname %in% test_with_dups, ]

#average duplicates

CovDups[,"lsires"] = as.numeric(CovDups[,"lsires"])


Temp = aggregate(CovDups[CovDups$ltstname %in% test_with_dups, "lsires"],
                 by = list(CovDups[CovDups$ltstname %in% test_with_dups,"patno"],
                           CovDups[CovDups$ltstname %in% test_with_dups,"event_id"],
                           CovDups[CovDups$ltstname %in% test_with_dups,"ltstname"]
                 ), FUN = mean )

colnames(Temp) = c("patno", "event_id", "ltstname", "lsires")

# long to wide
Cov_wide1 <- reshape(
  CovOK,
  v.names = c("lsires"),
  idvar = c("patno"),
  timevar = c("ltstname"),
  direction = "wide"
)

Cov_wide2 <- reshape(
  Temp,
  v.names = c("lsires"),
  idvar = c("patno"),
  timevar = c("ltstname"),
  direction = "wide"
)
### merge

Cov_wide = merge(Cov_wide1, Cov_wide2, by = "patno")


### convert to numeric

### remove unnecessary columns 
col_to_remove = c("event_id.y","event_id.x", "lsires.Which visit being performed?", "lsires.Myelocyte", "lsires.Serum beta hCG, Qualitative-QT")
Temp_cov <- Cov_wide[, !(colnames(Cov_wide) %in% col_to_remove)]

Temp_cov[,"lsires.RBC Morphology"] = as.factor(Temp_cov[,"lsires.RBC Morphology"])


##########################################################

byvars = c("patno")
Temp <- join_all(
  list(
    Temp_cov,
    Temp_bio
    #,MUTRSLT[c(byvars, "genecat", "lrrkcd", "mutrslt")] #HAS ITS OWN EVENT ID???, NOT RIGHT PATNO
    ),
  by = byvars
  )


## Baseline assessments
BiospecimenBL <- Temp

## Summary statistics
str(BiospecimenBL)

summary(BiospecimenBL)

## Save datasets
save(BiospecimenBL, file="Data/Biospecimen.RData")






