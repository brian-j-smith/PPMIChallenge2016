load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/RatesFits.RData")
load("Programs/Analysis/Imaging.RData")
load("Programs/Analysis/NonMotorOutcomes.RData")

# Append Rates to other lists
MotorUPDRSVars <- appendList(MotorUPDRSVars, 
                             RatesFitsVars[intersect(names(RatesFitsVars), names(MotorUPDRSVars))])
NonMotorVars <- appendList(NonMotorVars,
                           RatesFitsVars[intersect(names(RatesFitsVars), names(NonMotorVars))])
ImagingVars <- appendList(ImagingVars,
                          RatesFitsVars[intersect(names(RatesFitsVars), names(ImagingVars))])

OutcomeVars <- c(
  MotorUPDRSVars, NonMotorVars, ImagingVars
)
OutcomeVars <- OutcomeVars[order(names(OutcomeVars))]

OutcomeVals <- c(
  MotorUPDRSVals, RatesFitsVals, NonMotorVals, ImagingVals
)

OutcomeBest <- c(
  MotorUPDRSBest, RatesFitsBest, NonMotorBest, ImagingBest
)

AppInfo <- list(
  version = VERSION,
  selectedOutcome = names(MotorUPDRSVars)[1]
)

save(AppInfo, OutcomeVars, OutcomeVals, OutcomeBest,
     file="shiny/Outcomes.RData")
