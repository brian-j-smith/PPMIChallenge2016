load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/Imaging.RData")

OutcomeVars <- c(
  MotorUPDRSVars,
  ImagingVars
)

OutcomeVals <- c(
  MotorUPDRSVals, 
  ImagingVals
)

save(VERSION, OutcomeVars, OutcomeVals, file="shiny/Outcomes.RData")
