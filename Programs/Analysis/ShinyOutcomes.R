load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/RatesFits.RData")

# Append Rates to other lists
MotorUPDRSVars <- appendList(MotorUPDRSVars, 
                             RatesFitsVars[intersect(names(RatesFitsVars), names(MotorUPDRSVars))])
# NonMotorVars <- appendList(NonMotorVars, 
#                            RatesFitsVars[intersect(names(RatesFitsVars), names(NonMotorVars))])
# ImagingVars <- appendList(ImagingVars, 
#                           RatesFitsVars[intersect(names(RatesFitsVars), names(ImagingVars))])

OutcomeVars <- c(
  MotorUPDRSVars #, NonMotorVars, ImagingVars
)

OutcomeVals <- c(
  MotorUPDRSVals, RatesFitsVals #, NonMotorVals, ImagingVals
)

save(VERSION, OutcomeVars, OutcomeVals, file="shiny/Outcomes.RData")
