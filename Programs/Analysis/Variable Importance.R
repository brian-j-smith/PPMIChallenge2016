# Author: Ryan Peterson
# Date created : 8/31/16 
# Description: Variable importance table for paper

load("Programs/Analysis/MotorUPDRS.RData")
load("Programs/Analysis/RatesFits.RData")
load("Programs/Analysis/Imaging.RData")
load("Programs/Analysis/NonMotorOutcomes.RData")

names(MotorUPDRSBest)
names(NonMotorBest)
names(ImagingBest)
names(RatesFitsBest)


motorVar <- 'np1total_diff.V06'
nonMotorVar <- 'mcatot_diff.V06'
ImagingVar <- "meanputamen_perchange.V06"
WildCardVar <- 'gds_total.absolute'

m1 <- MotorUPDRSBest[[motorVar]]
m2 <- NonMotorBest[[nonMotorVar]]
m3 <- ImagingBest[[ImagingVar]]
m4 <- RatesFitsBest[[WildCardVar]]

v1 <- varImp(m1)
v2 <- varImp(m2)
v3 <- varImp(m3)
v4 <- varImp(m4)
o1 <- order(v1$importance, decreasing = T)
o2 <- order(v2$importance, decreasing = T)
o3 <- order(v3$importance, decreasing = T)
o4 <- order(v4$importance, decreasing = T)
top <- 10

t1 <- as.matrix(v1$importance[o1,][1:top])
rownames(t1) <- rownames(v1$importance)[o1][1:top]
colnames(t1) <- c(paste0(v1$model, ' Variable importance'))

t2 <- as.matrix(v2$importance[o2,][1:top])
rownames(t2) <- rownames(v2$importance)[o2][1:top]
colnames(t2) <- c(paste0(v2$model, ' Variable importance'))

t3 <- as.matrix(v3$importance[o3,][1:top])
rownames(t3) <- rownames(v3$importance)[o3][1:top]
colnames(t3) <- c(paste0(v3$model, ' Variable importance'))

t4 <- as.matrix(v4$importance[o4,][1:top])
rownames(t4) <- rownames(v4$importance)[o4][1:top]
colnames(t4) <- c(paste0(v4$model, ' Variable importance'))

t1
t2
t3
t4

# Or, to extract the coefficients of the glmnet models,
c1 <- coef(m1$finalModel, m1$bestTune$lambda)
sort(abs(c1), decreasing = T)
