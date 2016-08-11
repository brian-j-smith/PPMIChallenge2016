## PPMI Enrollment by cohort group and enrollment status
xtabs(~ recruitment_cat + enroll_status, data=PATIENT_STATUS)

# Explore the baseline data
source("Programs\\Analysis\\BaselineData.R")

freq <- function(x) {
  n <- table(x, useNA="always")
  cbind(n, round(100 * prop.table(n), 2))
}

means <- function(x) {
  y <- na.omit(x)
  structure(c(length(y), mean(y), sd(y), median(y), min(y), max(y), skewness(y), sum(is.na(x))),
            names=c("N", "Mean", "SD", "Median", "Min", "Max", "Skew", 'NAs'))
}

catvars <- sapply(BaselinePD, is.factor)
(cat_sumstats <- apply(BaselinePD[,catvars], 2, freq))
(cont_sumstats <- as.data.frame(t(apply(BaselinePD[,!catvars], 2, means))))

# What do the continuous covariates look like?

par(mfrow = c(2,2))
hist(cont_sumstats$Mean, main = 'Hist of imaging covariate means', xlab = 'mean')
hist(cont_sumstats$SD, main = 'Hist of imaging covariate SDs', xlab = 'sd')
hist(cont_sumstats$Median, main = 'Hist of imaging covariate Median', xlab = 'Median')
hist(cont_sumstats$Skew, main = 'Hist of imaging covariate Skew', xlab = 'Skew')

hist(cont_sumstats$N, main = 'Hist of covariate N', xlab = 'N')
hist(cont_sumstats$Min, main = 'Hist of covariate mins', xlab = 'min')
hist(cont_sumstats$Max, main = 'Hist of imaging covariate Max', xlab = 'Max')
hist(cont_sumstats$NAs, main = 'Hist of imaging covariate NAs', xlab = 'NAs')

# Atypical lymphocytes has a lot of missing data
