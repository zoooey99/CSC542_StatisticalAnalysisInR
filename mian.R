rm(list = ls())

library(ISLR2)
library(MASS)
library(dplyr)
library(corrplot)

parkinsons_updrs <- read.csv("~/Desktop/CSC 542/CSC542_Assignment1/data/parkinsons_updrs.data", header=TRUE)
head(parkinsons_updrs)
names(parkinsons_updrs)
summary(parkinsons_updrs)

M <- cor(parkinsons_updrs)
corrplot(M)


smp_size <- floor(0.80 * nrow(parkinsons_updrs))

set.seed(123)
train_ind <- sample(seq_len(nrow(parkinsons_updrs)), size = smp_size)

train <- parkinsons_updrs[train_ind, ]
test <- parkinsons_updrs[-train_ind, ]

# lm(): fits a simple linear regression model
# Basic syntax: lm(y~x, data)

mlrModel = lm(formula = motor_UPDRS ~ Jitter... + 
                        Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 +    
                        Jitter.DDP + Shimmer + Shimmer.dB. +
                        Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 +
                        Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE,
                        data = train)
#model_residuals <- mlrModel$residuals
#hist(model_residuals)

summary(mlrModel)

p <- predict(mlrModel, test)
summary_p <- summary(p)
print(summary_p)

#calculate rsquared
testVals <- test$motor_UPDRS
sumSquares <- sum((testVals - mean(testVals))^2)
sumSquaresResid <- sum((testVals - p)^2)
rsquared <- 1 - (sumSquaresResid / sumSquares)
print(rsquared)

#calculate RSE
residuals <- testVals - p
mse <- mean(residuals^2)
rse <- sqrt(mse)
print(rse)






