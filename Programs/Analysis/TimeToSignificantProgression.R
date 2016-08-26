# Author: Ryan Peterson
# Date created : 8/01/16 
# Date modified: 8/10/16 
# Description: Time to (statistically) Significant Progression calculation and Modeling

require(survival)

source('Programs\\project.R')
source('Programs\\Data\\Motor.R')
source("Programs/Analysis/Control.R")

np1total.rate <- outcome.rate('np1total', Motor, sig.level = .1)
np2total.rate <- outcome.rate('np2total', Motor, sig.level = .1)
np3total.rate <- outcome.rate('np3total', Motor, sig.level = .1)
nptotal.rate <- outcome.rate('nptotal', Motor, sig.level = .1)

head(np1total.rate)

## Plot Kaplan-Meyer curves
par(mfrow = c(2,2))
S <- survfit(Surv(ttsp, sp_ind) ~ 1, data = np1total.rate)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np1total, sig.level = 0.1',
     col=pal(1), lwd=2, mark.time = F)
ciband(fit, col=pal(1, alpha = .3))

S <- survfit(Surv(ttsp, sp_ind) ~ 1, data = np2total.rate)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np2total, sig.level = 0.1',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

S <- survfit(Surv(ttsp, sp_ind) ~ 1, data = np3total.rate)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, np3total, sig.level = 0.1',
     col=pal(1), lwd=2, mark.time = F)
ciband(S, col=pal(1, alpha = .3))

S <- survfit(Surv(ttsp, sp_ind) ~ 1, data = nptotal.rate)
plot(S, bty='n', xlab='Time (months)', 
     ylab="Significant-progression-free Survival", 
     main = 'All Cohorts, nptotal, sig.level = 0.1',
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

# np1total stratified by PD cohort
Dataset <- join(cohorts, np1total.rate, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttsp, sp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np1total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# np2total
Dataset <- join(cohorts, np2total.rate, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttsp, sp_ind) ~ recruitment_cat, data = Dataset)

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
Dataset <- join(cohorts, np3total.rate, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttsp, sp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'np3total, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

# nptotal
Dataset <- join(cohorts, nptotal.rate, by = "patno")
str(Dataset)

# Plot KM Curves of outcome by cohort
S <- survfit(Surv(ttsp, sp_ind) ~ recruitment_cat, data = Dataset)

plot(S, bty='n', xlab='Time (Months)', 
     ylab="Progression-free survival", 
     las=1, col=pal(4), lwd=2, mark.time=FALSE, 
     main = 'nptotal, Stratified by PD Cohort')
ciband(S, col=pal(4, alpha = .3))
legend('bottomleft', legend=levels(Dataset$recruitment_cat), col=pal(4), 
       lwd=2, bty = 'n')

