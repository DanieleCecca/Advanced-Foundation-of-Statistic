################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 5                                                    #
# Wednesday, November 8, 2023                                  #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  0. Data set uploading
##  1. Robust Regression (comparison with simple linear regression)
##  2. Subset Selection Methods and Cross Validation

# 0. Re-load the Automobile data set we use during the last lectures.
#    Run the following lines, that we have already introduced before.

NAval <- which(Automobiles == "?", arr.ind=T)
for (i in 1:dim(NAval)[1]){
  Automobiles[NAval[i,1],NAval[i,2]] <- NA
}

data <- as.data.frame(cbind(as.numeric(Automobiles$V22), as.numeric(Automobiles$V26)))

data <- as.data.frame(data[-which(is.na(data), arr.ind = T)[,1],])
colnames(data) <- c("Horsepower", "Price")

################################################################ 
# 1. Robust Regression                                         #
################################################################

## LEAST TRIMMED SQUARES ESTIMATOR
# Install the library "robustbase".
# install.packages("robustbase")
library(robustbase)

# The following command can be used for fitting robust regression.
? ltsReg # Read the details.

## Compare the lm and the lts model considering the variables Price (V26) and Horsepower (V22)
## of the Automobiles data set.
reg.simple <- lm(Price ~ Horsepower, data = data)
reg.lts <- ltsReg(Price ~ Horsepower, data = data) 

# Which is the difference in the coefficient estimation?
coef(reg.simple)
coef(reg.lts)

# Plot the estimated regression lines.
par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", main = "Regression model", pch = 20)
abline(reg.simple, col = "red", lwd = 2)
abline(reg.lts, col = "green", lwd = 2)

# What changes in the Q-Q plot?
par(mfrow = c(1,1))
? plot.lm
? plot.lts
plot(reg.simple, which = 2) # which = 2 --> Normal Q-Q plot
plot(reg.lts, which = "rqq") # which = "rqq" --> Normal Q-Q plot of the standardized residuals

## How to classify the outliers? Plot the outlier map.
plot(reg.lts, which = "rdiag")

## What changes if we change alpha?

reg.lts.alpha2 <- ltsReg(Price ~ Horsepower, data = data, alpha = 0.75) 
reg.lts.alpha3 <- ltsReg(Price ~ Horsepower, data = data, alpha = 0.90)

coef(reg.simple)
coef(reg.lts)
coef(reg.lts.alpha2)
coef(reg.lts.alpha3)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", main = "Regression models with different alphas", pch = 20)
abline(reg.simple, col = "red", lwd = 2)
abline(reg.lts, col = "green", lwd = 2)
abline(reg.lts.alpha2, col = "blue", lwd = 2)
abline(reg.lts.alpha3, col = "purple", lwd = 2)
legend(x = "bottomrigh", y = NULL, legend = c("alpha = 0.50", "alpha = 0.75", "alpha = 0.90", "alpha = 1"), 
       fill = c("green", "blue", "purple", "red"))

## Which is the number of outliers detected by different alphas? 
# For the simple linear regression compute the standardized residuals with
? stdres
length(which(abs(stdres(reg.simple)) > 2.5)) # alpha = 1 (LS)
length(which(abs(reg.lts$resid) > 2.5)) # alpha = 0.5
length(which(abs(reg.lts.alpha2$resid) > 2.5)) # alpha = 0.75
length(which(abs(reg.lts.alpha3$resid) > 2.5)) # alpha = 0.9

################################################################ 
# 2. Subset Selection Methods and Cross Validation             #
################################################################
## For computational reasons, "best subset selection" cannot be 
## applied with very large p. A big search space can lead to
## overfitting and high variance of the coefficient estimates.
## For this reason, we practically skip the "best subset selection"
## approach and we will focus on forward and backward stepwise selection
## that explore a restricted set of models.

## Data from Exercise 9 page 266.
# install.packages("ISLR")
library(ISLR)

data <- College

? ISLR:: College

attach(data)

names(data)
head(data)
dim(data)

# Which is the difference between forward and backward stepwise selection?

## The FORWARD stepwise selection starts from a model with no predictors and
## compares it with models obtained by adding predictors one-at-a-time. 
## At each step the variable that gives the greatest additional
## improvement to the fit is added to the model (1+p*(p+1)/2 
## evaluated models).

# install.packages("leaps")
library(leaps)

? regsubsets

reg.fwd <- regsubsets(Apps ~ ., data = data, nvmax = ncol(data) - 1, method = "forward")
summary(reg.fwd)

# How to choose the "best" model?
sum.reg.fwd <- summary(reg.fwd)
names(sum.reg.fwd)

sum.reg.fwd$rss

## Why do we use other measures than the RSS or R^2?
## Because they depends on the number of variables.

## Instead, the Adjusted R^2, Cp, BIC adjust the training error for the
## model size (number of variables) and thus can be used to select among different
## models WITH DIFFERENT NUMBER OF VARIABLES.

## A. Adjusted R^2 = 1 - ((n-1)/(n-p-1))*(1-R^2)
##    --> The penalty increases as the number of predictors in the model increases.
##    Choice: max(Adjusted R^2)
## B. Cp adds a penalty of 2*p*sigmahat^2 to the training RSS.
##    --> The penalty increases as the number of predictors in the model increases.
##    Choice: min(Cp)
## C. BIC adds a penalty of p*sigmahat^2*log(n) to the training RSS.
##    --> The BIC generally places a heavier penalty on models with many
##    variables, and hence results in the selection of smaller models than Cp.
##    Choice: min(BIC)


# Represent the results in terms of RSS, Adjusted R^2, Cp, BIC.

par(mfrow = c(2,2))
# RSS
plot(sum.reg.fwd$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", xlim = c(0, 17))
points(which.min(sum.reg.fwd$rss), sum.reg.fwd$rss[which.min(sum.reg.fwd$rss)], col = "red", cex = 2,
       pch = 20)
# Adjusted R^2 with its maximum
plot(sum.reg.fwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l", xlim = c(0, 17))
points(which.max(sum.reg.fwd$adjr2), sum.reg.fwd$adjr2[which.max(sum.reg.fwd$adjr2)], col = "red", cex = 2, 
       pch = 20)
# Cp with its minimum
plot(sum.reg.fwd$cp, xlab = "Number of Variables", ylab = "Cp - AIC", type = "l", xlim = c(0, 17))
points(which.min(sum.reg.fwd$cp), sum.reg.fwd$cp[which.min(sum.reg.fwd$cp)], col = "red", cex = 2,
       pch = 20)
# BIC with its minimum
plot(sum.reg.fwd$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", xlim = c(0, 17))
points(which.min(sum.reg.fwd$bic), sum.reg.fwd$bic[which.min(sum.reg.fwd$bic)], col = "red", cex = 2,
       pch = 20)

which.max(sum.reg.fwd$adjr2)
which.min(sum.reg.fwd$cp)
which.min(sum.reg.fwd$bic)

## What does it change with a backward stepwise selection?
## The BACKWARD stepwise selection starts from the full model 
## containing all predictors and compares it with models obtained by 
## removing predictors one-at-a-time. 
## The number of the evaluated models is the same as the forward stepwise selection.

reg.bwd <- regsubsets(Apps ~ ., data = data, nvmax = ncol(data) - 1, method = "backward")
sum.reg.bwd <- summary(reg.bwd)

which.max(sum.reg.bwd$adjr2)
which.min(sum.reg.bwd$cp)
which.min(sum.reg.bwd$bic)

# Compare the results with the forward stepwise selection, 
# considering to choose the best model according to the BIC.
names(coef(reg.fwd, 10))
names(coef(reg.bwd, 10))

# Explore the results.
str(data)
coef(reg.fwd, which.min(sum.reg.fwd$bic))
names(coef(reg.fwd, which.min(sum.reg.fwd$bic)))[-1]

data.fwd <- colnames(data) %in% names(coef(reg.fwd, 10))[-1]
colnames(data[data.fwd])

data.fwd[1] <- TRUE

summary(lm(Apps ~ ., data = data[, data.fwd]))
coef(reg.fwd, which.min(sum.reg.fwd$bic))

# How many models do I test with the best subset selection method?
# sum(choose(17, 1:17))+1
2^17

## NOW, CHOOSE AMONG MODELS USING CROSS-VALIDATION WITH THE BEST SUBSET SELECTION 
## APPROACH.
## We can compute the cross-validation error for each model with a fixed number of predictors
## and then select the the number of predictors for which the resulting estimated test error is
## smallest.
## This is an alternative to AIC, BIC and adjusted R2, since it provides a *direct* estimate of
## the test error, and does not require an estimate of the error variance (needed for the computation
## of BIC, AIC etc.).

# We must perform the best selection WITHIN each of the k training sets. 
set.seed(12424)
k <- 10
folds <- sample(1:k, nrow(data), replace = TRUE)
cv.errors <-  matrix(NA, k, ncol(data) - 1, dimnames = list(NULL, paste(1:(ncol(data) - 1))))

# In the j-th fold, the elements of folds are in the test set and the remaining are
# in the training set.

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

for (j in 1:k) {
  best.fit <- regsubsets(Apps ~ ., data = data[folds!=j, ], nvmax = ncol(data) - 1, method = "exhaustive")
  for (i in 1:(ncol(data) - 1)) {
    pred <- predict(best.fit, data[folds == j, ], id = i)
    cv.errors[j,i] <- mean((data$Apps[folds == j] - pred)^2)
  }
}

# ATTENTION

# What is cv.errors?
cv.errors

# A matrix of dimension (10 x 17), where each element [i, j] corresponds to the test 
# MSE for the i-th cross-validation fold (i in 1:10) and the best model with 
# j (j in 1:17) variables.

# Since we want to choose the best model according to the test MSE, we need to
# compute the mean of cv.errors for each column. 

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# Represent the results.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b", pch = 20, xlab = "Number of variables", ylab = "Mean CV Error")

which.min(mean.cv.errors)
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col = "red", pch = 20)

# HOMEWORK: Explore the results of the best model selected.
