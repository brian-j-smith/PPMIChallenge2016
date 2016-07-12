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

byvars = c("patno", "event_id")
Temp <- join_all(
  list(
    Biospecimen_Analysis[c(byvars, "type", "testname", "testvalue", "units" )], 
    COVANCE[c(byvars, "lgroup", "ltstcode", "lsires", "lsiunit", "lsilorng", "lsihirng", "lresflg")],
    #CLINLAB[c(byvars, "", "", )], not sure if any of this is imp-- bldlab is blood for safety labs 0-3
    DNA[c(byvars,"blddna", "blddnadt","bldvolml")],
    #MUTRSLT[],
    #LAB[],
    #LUMBAR[],
    #WHOLEBLD[],
    SKBIO[c(byvars, "skbiosid", "woundcls" )] #not sure if side/closure type are important
    ),
  by = byvars
  )

Biospecimen <- 


