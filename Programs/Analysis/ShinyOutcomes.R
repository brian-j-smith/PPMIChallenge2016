load("Programs/Analysis/MotorUPDRS.RData")

OutcomeVars <- c(
  MotorUPDRSVars
)

OutcomeVals <- c(
  MotorUPDRSVals
)

save(VERSION, OutcomeVars, OutcomeVals, file="shiny/Outcomes.RData")
