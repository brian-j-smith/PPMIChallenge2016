# Author: Ryan Peterson
# Date created : 8/01/16 
# Date modified: 8/03/16 
# Description: Time to Significant (CLINCIAL) Progression calculation 

require(survival)

source('Programs\\project.R')
source('Programs\\Data\\Motor.R')
source('Programs\\Analysis\\BaselineData.R')
source("Programs/Analysis/Control.R")

par(mfrow = c(1,1))
np1total.cp <- outcome.changepoint(outcome = 'np1total', data = Motor, sig.level = .05, 
                                   clin.sig.level = 1.50, scale = 'relative', plot = T)
np2total.cp <- outcome.changepoint('np2total', Motor, sig.level = .05, 
                                    clin.sig.level = 1.50, scale = 'relative')
np3total.cp <- outcome.changepoint('np3total', Motor, sig.level = .05, 
                                    clin.sig.level = 1.50, scale = 'relative')
nptotal.cp <- outcome.changepoint('nptotal', Motor, sig.level = .05, 
                                    clin.sig.level = 1.50, scale = 'relative')

head(np1total.cp)

## Plot Kaplan-Meyer curves
par(mfrow = c(2,2))
S <- survfit(Surv(ttcsp, csp_ind) ~ 1, data = np1total.cp)
summary(S)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np1total, sig.level = 0.05, clin.sig = 1.5',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

S <- survfit(Surv(ttcsp, csp_ind) ~ 1, data = np2total.cp)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np2total, sig.level = 0.05, clin.sig = 1.5',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

S <- survfit(Surv(ttcsp, csp_ind) ~ 1, data = np3total.cp)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np3total, sig.level = 0.05, clin.sig = 1.5',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

S <- survfit(Surv(ttcsp, csp_ind) ~ 1, data = nptotal.cp)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, nptotal, sig.level = 0.05, clin.sig = 1.5',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

par(mfrow = c(1,1))

## Next step; do this plot but for each cohort

## Get cohort data
cohorts <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, 
  by = "patno",
  subset = (recruitment_cat %in% c('PD', 'GENPD', 'REGPD', 'PRODROMA')),
  select = c(patno, recruitment_cat, enroll_status)
))

cohorts$recruitment_cat <- 
  with(cohorts,
       factor(recruitment_cat, 
              levels = c('PD', 'PRODROMA', 'GENPD','REGPD')
       )
  )

cohorts$enroll_status <- (factor(cohorts$enroll_status))

par(mfrow = c(2,2))
# np1total stratified by PD cohort
Dataset <- join(cohorts, np1total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np1total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# np2total
Dataset <- join(cohorts, np2total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np2total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')
## Odd that the prodroma patients are doing worse...
summary(lm(rates ~ recruitment_cat, data= Dataset))
## Their rates are less extreme. Perhaps also include a clinical significance cutoff?

# np3total
Dataset <- join(cohorts, np3total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np3total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# nptotal
Dataset <- join(cohorts, nptotal.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'nptotal, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

## A look at PD cohort VS HC cohort

## Get cohort data
cohorts <- dropfactors(join.ppmi(
  SubjectsBL, Enroll, 
  by = "patno",
  subset = (recruitment_cat %in% c('PD', 'HC')),
  select = c(patno, recruitment_cat, enroll_status)
))

cohorts$recruitment_cat <- 
  with(cohorts,
       factor(recruitment_cat, 
              levels = c('PD', 'HC')
       )
  )

par(mfrow = c(2,2))
# np1total stratified by PD cohort
Dataset <- join(cohorts, np1total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np1total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# np2total
Dataset <- join(cohorts, np2total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np2total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')
## Odd that the prodroma patients are doing worse...
summary(lm(rates ~ recruitment_cat, data= Dataset))
## Their rates are less extreme. Perhaps also include a clinical significance cutoff?

# np3total
Dataset <- join(cohorts, np3total.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np3total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# nptotal
Dataset <- join(cohorts, nptotal.cp, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttcsp, csp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'nptotal, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

## An initial model
Dataset <- join(BaselinePD, nptotal.cp, by = "patno")

S <- survfit(Surv(ttcsp, csp_ind) ~ 1, data = Dataset)
plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival")

fit0 <- coxph(Surv(ttcsp, csp_ind) ~ age_at_bl, Dataset)
summary(fit0)

## A look at (potential) functional forms
R <- resid(fit0, type = "deviance")
par(mfrow = c(2,2))
for(i in 3:(dim(Dataset)[2])){
  tempvar <- as.numeric(Dataset[,i])
  tempvar[is.na(tempvar)] <- mean(((tempvar)), na.rm = T)
  varName <- names(Dataset)[i]
  plot(tempvar, R, pch = 20, xlab = varName)
  lines(lowess(tempvar, R), col = "slateblue", lwd = 2)
}

## A "full" model
scope <- list(lower = ~1, upper = formula(paste("~", paste(BaselinePDVars, collapse=" + "))))
(fit.full <- stepAIC(fit0, Dataset, k = log(433), direction = 'forward',
                     scope = scope))
summary(fit.full)


