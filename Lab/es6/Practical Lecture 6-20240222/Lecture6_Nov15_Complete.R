################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 6                                                    #
# Wednesday, November 15, 2023                                 #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Shrinkage methods (Lasso and Ridge)
##  2. Introduction to dimension reduction methods: PCR and PLS

cat("\014")
rm(list = ls())

################################################################ 
# 1. Shrinkage Methods                                         #
################################################################
## Differently from testing each regression coefficient, the shrinkage 
## methods are useful to automatically select relevant predictors by
## constraining/regularizing the coefficient estimates, i.e.,  
## shrinking the coefficient estimates towards zero.

## Which is the difference between Ridge and Lasso?
## The regularization/shrinkage penalty.

## RIDGE 
## Penalty: lambda*sum(beta^2) (shrinkage to exactly zero only if lambda -> +inf).
##          --> L2 penalty.
## The shrinkage penalty is small when the coefficients are close to zero,
## and so it has the effect of shrinking their estimates of towards zero. 
## ATTENTION: Increasing the value of lambda will tend to reduce the magnitudes 
# of the coefficients, but will not result in exclusion of any of the variables!

## LASSO
## Penalty: lambda*sum(abs(beta)) (some coefficients can be EXACTLY zero when 
##          lambda is sufficiently large).
##          --> L1 penalty --> VARIABLE SELECTION!

## ATTENTION::
## In general, the LASSO performs better in a setting where a relatively small 
## number of predictors have substantial coefficients and the remaining predictors 
## have coefficients that are very small or that equal zero. 
## Instead, the RIDGE performs better when the response is a function of many predictors,
## all with coefficients of roughly equal size. 
## However, the number of predictors that is related to the response is USUALLY 
## NOT KNOWN A PRIORI for real data sets!
## Cross-validation can be used in order to determine which approach is better
## on the data set under study.

## Exercise 9 page 266
## TEXT
## In this exercise, we will predict the number of applications received
## using the other variables in the College data set (library(ISLR)).
## QUESTIONS:
## (a) Split the data set into a training set and a test set.
## (b) Fit a linear model using least squares on the training set, and
##     report the test error obtained.
## (c) Fit a ridge regression model on the training set, with lambda chosen by cross-validation. 
##     Report the test error obtained.
## (d) Fit a lasso model on the training set, with lambda chosen by cross-validation.
##     Report the test error obtained, along with the number of non-zero 
##     coefficient estimates.

# install.packages("ISLR")
library(ISLR)

data <- College

? ISLR:: College

data <- data[, -1]

names(data)

dim(data)

attach(data)

## (a)
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

## (b) LS Regression
lm.fit <- lm(Apps ~ ., data = data, subset = train)
lm.pred <- predict(lm.fit, data[!train,])

lm.mse <- mean((lm.pred-Apps[!train])^2)

############################################################

## (c) Ridge Regression
? model.matrix
x <- model.matrix (Apps ~ ., data)[, -1]
y <- Apps

# install.packages("glmnet")
library(glmnet)
? glmnet

## Before fitting the Ridge regression, analyse the variability of the data.
str(data)
# install.packages("pracma")
library(pracma)
heatmap(cov(data[,-c(1:2)]), Colv = NA, Rowv = NA)

apply(data[, -1], 2, sd)

## Do we need to standardize the data?
# glmnet option: standardize = TRUE (default)!

grid <- 10^seq(10, -2, length = 100) 
length(grid)
# It approximately covers the scenarios from the null model up to the LS fit.

ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
ridge.mod

dim(coef(ridge.mod)) # Including the intercept!

## Which is the behaviour of the coefficient estimates when lambda increases?
ridge.mod$lambda [60] 
coef(ridge.mod)[, 60]

ridge.mod$lambda [50]
coef(ridge.mod)[, 50]

## The coefficient estimates are smaller when a large value of lambda is used...in terms of l2 norm!
sqrt(sum(coef(ridge.mod )[-1, 60]^2))
sqrt(sum(coef(ridge.mod )[-1, 50]^2))

## How to choose the tuning parameter?
## CROSS VALIDATION!
? cv.glmnet
# 10-fold cross-validation by default.
# nfolds input: nfolds = 10.

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
plot(cv.out)

names(cv.out)
# Lambda with the smallest cross-validation error.
best.lam.ridge <- cv.out$lambda.min 
best.lam.ridge

## This value corresponds to...
log(cv.out$lambda.min)

## Make predictions.

ridge.pred <- predict(ridge.mod, s = best.lam.ridge, newx = x[!train, ])
ridge.mse <- mean((ridge.pred-y[!train])^2)

## Compare this results with a model with just an intercept.
ridge.pred.null <- predict(ridge.mod, s = grid[1], newx = x[!train, ])

ridge.mse
mean((ridge.pred.null - y[!train])^2)

## Analyse the results with the chosen lambda
ridge.mod.f <- glmnet(x, y, alpha = 0, lambda = best.lam.ridge)
coef(ridge.mod.f)

##################################################################

## (d) Lasso Regression

lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.mod

dim(coef(lasso.mod))

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(cv.out)

best.lam.lasso <- cv.out$lambda.min
best.lam.lasso

log(best.lam.lasso)

lasso.pred <- predict(lasso.mod, s = best.lam.lasso, newx = x[!train, ])
lasso.mse <- mean((lasso.pred-y[!train])^2)

lasso.pred.null <- predict(lasso.mod, s = grid[1], newx = x[!train, ])
lasso.mse
mean((lasso.pred.null - y[!train])^2)

## Compare the results
lm.mse
ridge.mse
lasso.mse

################################################################
## Analyse the results with the best model.

lasso.best <- glmnet(x, y, alpha = 1, lambda = best.lam.lasso)
lasso.best$beta

ridge.best <- glmnet(x, y, alpha = 0, lambda = best.lam.ridge)
ridge.best$beta

