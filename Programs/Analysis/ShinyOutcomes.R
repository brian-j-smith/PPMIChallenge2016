load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/Imaging.RData")

OutcomeVars <- c(
  MotorUPDRSVars,
  ImagingVars,
  NonMotorVars
)

OutcomeVals <- c(
  MotorUPDRSVals, 
  ImagingVals,
  NonMotorVals
)

save(VERSION, OutcomeVars, OutcomeVals, file="shiny/Outcomes.RData")
