## Import Function
read.ppmi <- function(file) {
  X <- read.csv(file, as.is=TRUE)
  names(X) <- tolower(names(X))
  print(file)
  str(X)
  X
}


## Subject Characteristics
SITE_NAMES <- read.ppmi("Data\\_Subject_Characteristics\\Center-Subject_List.csv")
FAMHXPD <- read.ppmi("Data\\_Subject_Characteristics\\Family_History__PD_.csv")
PATIENT_STATUS <- read.ppmi("Data\\_Subject_Characteristics\\Patient_Status.csv")
SCREEN <- read.ppmi("Data\\_Subject_Characteristics\\Screening___Demographics.csv")
SOCIOECO <- read.ppmi("Data\\_Subject_Characteristics\\Socio-Economics.csv")

## Biospecimen
Biospecimen_Analysis <- read.ppmi("Data\\Biospecimen\\Biospecimen_Analysis_Results.csv")
COVANCE <- read.ppmi("Data\\Biospecimen\\Blood_Chemistry___Hematology.csv")
CLINLAB <- read.ppmi("Data\\Biospecimen\\Clinical_Labs.csv")
DNA <- read.ppmi("Data\\Biospecimen\\DNA_Sample_Collection.csv")
MUTRSLT <- read.ppmi("Data\\Biospecimen\\Genetic_Testing_Results.csv")
LAB <- read.ppmi("Data\\Biospecimen\\Laboratory_Procedures_with_Elapsed_Times.csv")
LUMBAR <- read.ppmi("Data\\Biospecimen\\Lumbar_Puncture_Sample_Collection.csv")
SKBIO <- read.ppmi("Data\\Biospecimen\\Skin_Biopsy.csv")
WHOLEBLD <- read.ppmi("Data\\Biospecimen\\Whole_Blood_Sample_Collection.csv")

## Imaging
ind_av133_metadata <- read.ppmi("Data\\Imaging\\AV-133_Image_Metadata.csv")
AVIMAG <- read.ppmi("Data\\Imaging\\AV-133_Imaging.csv")
ind_av133_sbr <- read.ppmi("Data\\Imaging\\AV-133_SBR_Results.csv")
DATSCAN <- read.ppmi("Data\\Imaging\\DaTscan_Imaging.csv")
ind_spect_sbr <- read.ppmi("Data\\Imaging\\DaTscan_Striatal_Binding_Ratio_Results.csv")
ind_datscan_source_data <- read.ppmi("Data\\Imaging\\ind_dat_source_data.csv")
ind_mri_source_data <- read.ppmi("Data\\Imaging\\ind_mri_source_data.csv")
MRI <- read.ppmi("Data\\Imaging\\Magnetic_Resonance_Imaging.csv")

## Medical History
AE <- read.ppmi("Data\\Medical_History\\Adverse_Event_Log.csv")
CLINDX <- read.ppmi("Data\\Medical_History\\Clinical_Diagnosis_and_Management.csv")
CONMED <- read.ppmi("Data\\Medical_History\\Concomitant_Medications.csv")
CURRCOND <- read.ppmi("Data\\Medical_History\\Current_Medical_Conditions_Log.csv")
DIAGFEAT <- read.ppmi("Data\\Medical_History\\Diagnostic_Features.csv")
MHXGEN <- read.ppmi("Data\\Medical_History\\General_Medical_History.csv")
PENEURO <- read.ppmi("Data\\Medical_History\\General_Neurological_Exam.csv")
GENPHYEX <- read.ppmi("Data\\Medical_History\\General_Physical_Exam.csv")
INCIDENTS <- read.ppmi("Data\\Medical_History\\Initiation_of_PD_Medication-_incidents.csv")
PECN <- read.ppmi("Data\\Medical_History\\Neurological_Exam_-_Cranial_Nerves.csv")
PDFEAT <- read.ppmi("Data\\Medical_History\\PD_Features.csv")
PREGNANC <- read.ppmi("Data\\Medical_History\\Pregnancy_Form.csv")
PRODDIAG <- read.ppmi("Data\\Medical_History\\Prodromal_Diagnostic_Questionnaire.csv")
SURGPD <- read.ppmi("Data\\Medical_History\\Surgery_for_Parkinson_Disease.csv")
PDMEDUSE <- read.ppmi("Data\\Medical_History\\Use_of_PD_Medication.csv")
VITAL <- read.ppmi("Data\\Medical_History\\Vital_Signs.csv")

## Motor Assessments
NUPDRS1 <- read.ppmi("Data\\Motor___MDS-UPDRS\\MDS_UPDRS_Part_I.csv")
NUPDRS1P <- read.ppmi("Data\\Motor___MDS-UPDRS\\MDS_UPDRS_Part_I__Patient_Questionnaire.csv")
NUPDRS2P <- read.ppmi("Data\\Motor___MDS-UPDRS\\MDS_UPDRS_Part_II__Patient_Questionnaire.csv")
NUPDRS3 <- read.ppmi("Data\\Motor___MDS-UPDRS\\MDS_UPDRS_Part_III__Post_Dose_.csv")
NUPDRS4 <- read.ppmi("Data\\Motor___MDS-UPDRS\\MDS_UPDRS_Part_IV.csv")
MODSEADL <- read.ppmi("Data\\Motor___MDS-UPDRS\\Modified_Schwab_+_England_ADL.csv")
PASEHSWK <- read.ppmi("Data\\Motor___MDS-UPDRS\\PASE_-_Household_Activity.csv")
PASELS <- read.ppmi("Data\\Motor___MDS-UPDRS\\PASE_-_Leisure_Time_Activity.csv")
KINETICS <- read.ppmi("Data\\Motor___MDS-UPDRS\\TAP-PD_Kinetics_Device_Testing.csv")
TAPOPDM <- read.ppmi("Data\\Motor___MDS-UPDRS\\TAP-PD_OPDM_Assessment.csv")
TAPUSE <- read.ppmi("Data\\Motor___MDS-UPDRS\\TAP-PD_OPDM_Use_Questionnaire.csv")

## Non-Motor Assessments
LINEORNT <- read.ppmi("Data\\Non-motor_Assessments\\Benton_Judgment_of_Line_Orientation.csv")
COGTIME <- read.ppmi("Data\\Non-motor_Assessments\\Cognitive_Assessments.csv")
COGCATG <- read.ppmi("Data\\Non-motor_Assessments\\Cognitive_Categorization.csv")
EPWORTH <- read.ppmi("Data\\Non-motor_Assessments\\Epworth_Sleepiness_Scale.csv")
GDSSHORT <- read.ppmi("Data\\Non-motor_Assessments\\Geriatric_Depression_Scale__Short_.csv")
HVLT <- read.ppmi("Data\\Non-motor_Assessments\\Hopkins_Verbal_Learning_Test.csv")
LNSPD <- read.ppmi("Data\\Non-motor_Assessments\\Letter_-_Number_Sequencing__PD_.csv")
MOCA <- read.ppmi("Data\\Non-motor_Assessments\\Montreal_Cognitive_Assessment__MoCA_.csv")
ind_upsit <- read.ppmi("Data\\Non-motor_Assessments\\Olfactory_UPSIT.csv")
QUIPCS <- read.ppmi("Data\\Non-motor_Assessments\\QUIP_Current_Short.csv")
REMSLEEP <- read.ppmi("Data\\Non-motor_Assessments\\REM_Sleep_Disorder_Questionnaire.csv")
SCOPAAUT <- read.ppmi("Data\\Non-motor_Assessments\\SCOPA-AUT.csv")
SFT <- read.ppmi("Data\\Non-motor_Assessments\\Semantic_Fluency.csv")
STAI <- read.ppmi("Data\\Non-motor_Assessments\\State-Trait_Anxiety_Inventory.csv")
SDM <- read.ppmi("Data\\Non-motor_Assessments\\Symbol_Digit_Modalities.csv")
UPSIT <- read.ppmi("Data\\Non-motor_Assessments\\University_of_Pennsylvania_Smell_ID_Test.csv")

## Enrollment
AVELIG <- read.ppmi("Data\\Study_Enrollment\\AV-133_Subject_Eligibility.csv")
CONCL <- read.ppmi("Data\\Study_Enrollment\\Conclusion_of_Study_Participation.csv")
FOUNDINF <- read.ppmi("Data\\Study_Enrollment\\Contact_Information_FOUND.csv")
INEX <- read.ppmi("Data\\Study_Enrollment\\Inclusion_Exclusion.csv")
PRIMDXPD <- read.ppmi("Data\\Study_Enrollment\\Primary_Diagnosis.csv")
RANDOM <- read.ppmi("Data\\Study_Enrollment\\Randomization_table.csv")
RAD <- read.ppmi("Data\\Study_Enrollment\\Research_Advance_Directive.csv")
SIG <- read.ppmi("Data\\Study_Enrollment\\Signature_Form.csv")
SKBIOELG <- read.ppmi("Data\\Study_Enrollment\\Skin_Biopsy_Eligibility.csv")
ST_CATALOG <- read.ppmi("Data\\Study_Enrollment\\ST_CATALOG.csv")
TAPCONCL <- read.ppmi("Data\\Study_Enrollment\\TAP-PD_Conclusion.csv")
TAPELIG <- read.ppmi("Data\\Study_Enrollment\\TAP-PD_Subject_Eligibility.csv")


save("AE", "AVELIG", "AVIMAG", "Biospecimen_Analysis", "CLINDX", 
     "CLINLAB", "COGCATG", "COGTIME", "CONCL", "CONMED", "COVANCE", 
     "CURRCOND", "DATSCAN", "DIAGFEAT", "DNA", "EPWORTH", "FAMHXPD", 
     "FOUNDINF", "GDSSHORT", "GENPHYEX", "HVLT", "INCIDENTS", "ind_av133_metadata", 
     "ind_av133_sbr", "ind_datscan_source_data", "ind_mri_source_data", 
     "ind_spect_sbr", "ind_upsit", "INEX", "KINETICS", "LAB", "LINEORNT", 
     "LNSPD", "LUMBAR", "MHXGEN", "MOCA", "MODSEADL", "MRI", "MUTRSLT", 
     "NUPDRS1", "NUPDRS1P", "NUPDRS2P", "NUPDRS3", "NUPDRS4", "PASEHSWK", 
     "PASELS", "PATIENT_STATUS", "PDFEAT", "PDMEDUSE", "PECN", "PENEURO", 
     "PREGNANC", "PRIMDXPD", "PRODDIAG", "QUIPCS", "RAD", "RANDOM", 
     "REMSLEEP", "SCOPAAUT", "SCREEN", "SDM", "SFT", "SIG", "SITE_NAMES", 
     "SKBIO", "SKBIOELG", "SOCIOECO", "ST_CATALOG", "STAI", "SURGPD", 
     "TAPCONCL", "TAPELIG", "TAPOPDM", "TAPUSE", "UPSIT", "VITAL", 
     "WHOLEBLD", "read.ppmi",
     file="Data\\PPMI.RData")
