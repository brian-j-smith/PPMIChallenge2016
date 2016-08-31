# Author: Ryan Peterson
# Date created : 8/30/16 
# Description: Calculate Within Sample estimates of error for best models

source('Programs/utils.R')
load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/NonMotorOutcomes.RData")
load("Programs/Analysis/ImagingOutcomes.RData")
load("Programs/Analysis/RatesFits.RData")

# within sample prediction estimates
getWithinSampleError(MotorUPDRSVals, FitObject = F)
getWithinSampleError(RatesFitsVals, FitObject = F)
getWithinSampleError(ImagingVals, FitObject = F)
getWithinSampleError(NonMotorVals, FitObject = F)
