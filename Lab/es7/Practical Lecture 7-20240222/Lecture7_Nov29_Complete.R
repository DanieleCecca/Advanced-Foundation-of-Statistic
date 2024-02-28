################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 7                                                    #
# Wednesday, November 29, 2023                                 #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Dimension reduction methods: PCR and PLS
##  2. Logistic regression (logit model)

cat("\014")
rm(list = ls())

################################################################ 
# 1. Principal Component Regression and Partial Least Squares  #
################################################################
## Exercise 9 page 266 (continued...)
## QUESTIONS
## (e) Fit a PCR model on the training set, with M chosen by cross-validation.
##     Report the test error obtained, along with the value of M selected 
##     by cross-validation.
## (f) Fit a PLS model on the training set, with M chosen by cross-validation.
##     Report the test error obtained, along with the value of M selected by cross-validation.
## (g) Comment on the results obtained. How accurately can we predict
##     the number of college applications received? Is there much difference among the test errors resulting 
##     from these five approaches?

# 0. Upload the data set used in the last lecture.
library(ISLR)
data <- College
? ISLR:: College
data <- data[, -1]
names(data)
dim(data)
attach(data)

set.seed(1) 
train <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))

x <- model.matrix (Apps ~ ., data)[, -1]
y <- Apps

## (e) Principal Components Regression
# install.packages("pls")
library(pls)
? pcr

## See some inputs:
## scale
## 

pcr.mod <- pcr(y ~ x, subset = train, scale = TRUE, validation = "CV")
names(pcr.mod)
pcr.mod$ncomp
summary(pcr.mod)

library(corrplot)
corrplot(cor(data[, -1]), method = "color", number.digits = 2, number.cex = 0.8, tl.cex = 0.8,
         addCoef.col = "black", tl.col = "black", tl.srt = 45, is.corr = T)
# There are high correlations between predictors.

## Make predictions and evaluate the results.
pcr.pred <- predict(pcr.mod, x[!train, ], ncomp = 6)
pcr.mse <- mean((pcr.pred - y[!train])^2)

## Analyse the results.
pcr.best <- pcr(y ~ x, scale = TRUE, ncomp = 6)
summary(pcr.best)

names(pcr.best)
pcr.best$loadings

#############################################################################

## (f) Partial Least Squares

? plsr

pls.mod <- plsr(y ~ x, subset = train, scale = TRUE, validation = "CV")
summary(pls.mod)

## Make predictions and evaluate the results.
pls.pred <- predict(pls.mod, x[!train, ], ncomp = 8)
pls.mse <- mean((pls.pred - y[!train])^2)

## Analyse the results.
pls.best <- plsr(y ~ x, scale = TRUE, ncomp = 8)
summary(pls.best)

names(pls.best)

pls.best$loadings

##################################################################
## (g) Compare all the results.
## N.B. Combine these results with those obtained in Lecture 6.
pcr.mse
pls.mse

################################################################ 
# 2. Logistic regression                                       #
################################################################
# Consider the data set "TitanicSurvival" in library(carData).
# If necessary, 'install.packages("carData")'.

library(carData)

? TitanicSurvival

data <- TitanicSurvival

head(data)

names(data)

# ATTENTION: there are some missing values in the data set (see the 'Help'). 
# Remove the corresponding rows.

data <- data[-which(is.na(data$age)),]
str(data)

# Compute descriptive statistics.

summary(data)

boxplot(age ~ survived, data = data)

# Explore how R codes the variable 'survived'.
contrasts(data$survived)

# Fit a logistic regression model.
? glm

glm.titanic <- glm(survived ~ ., data = data, family = binomial)

# QUESTIONS --> Analyse the results:
# A. Coefficients' interpretation.
# B. Coefficients' significance.
# C. Model fit.

# coef(glm.titanic)

summary(glm.titanic)

# Predict the probability to survive for all the passengers.
? predict.glm

# You can use the 'predict' function with a glm output! 
predict(glm.titanic, type = "response") 

# Which is the difference between...
predict(glm.titanic, type = "response") # It gives the predicted probabilities in the RESPONSE VARIABLE SCALE.

# and...
predict(glm.titanic, type = "link") # It gives the probabilities on the LOGIT SCALE.

# Interpret the difference for Allen, Miss. Elisabeth Walton and Connors (1), Mr. Patrick (667).
predict(glm.titanic, type = "response")[1]
# predict(glm.titanic, type = "link")[1]
data[1, ]

predict(glm.titanic, type = "response")[667]
# predict(glm.titanic, type = "link")[667]
data[667, ]

# Analyse the prediction of the model by building a new variable called "surv.pred" that, for each observation, is
# equal to "yes" if the predicted probability of surviving is > 0.5, "no" otherwise.
pred.probs <- predict(glm.titanic, type = "response")
surv.pred <- rep("no", dim(data)[1]) 
surv.pred[pred.probs > 0.5] <- "yes"

# How many "errors" occur by estimating the survival variable according to the fitted model?
# This means to determine HOW MANY OBSERVATIONS ARE CORRECTLY CLASSIFIED (OR NOT) according to the fitted model.
# Compute the CONFUSION MATRIX.
conf.mat <- table(data$survived, surv.pred)
conf.mat

# CORRECTLY CLASSIFIED OBSERVATIONS
sum(diag(conf.mat))/dim(data)[1]
# OR
mean(surv.pred == data$survived)

# INCORRECTLY CLASSIFIED 
(conf.mat[1,2] + conf.mat[2,1])/dim(data)[1]
# OR
mean(surv.pred != data$survived)

# Is this possible to more precisely evaluate the accuracy of the fitted model?
# --> USE a TRAINING AND TEST DATA SET BY SPITTING THE ORIGINAL DATA SET (HOLDING-OUT SOME DATA FOR FITTING THE MODEL)
samp <- sample(1:dim(data)[1], 800)
train <- rep(FALSE, dim(data)[1])
train[samp] <- TRUE 

length(which(train == T))
length(which(train == F))

# length(which(train == T)) + length(which(train == F)) == dim(data)[1]
glm.train <- glm(survived ~ ., data = data, family = binomial, subset = train)

glm.test <- predict(glm.train, data[!train,],type = "response")

length(glm.test)

surv.pred.test <- rep("no", length(glm.test)) 
surv.pred.test[glm.test > 0.5] <- "yes"

table(data$survived[!train], surv.pred.test)
mean(data$survived[!train] == surv.pred.test)*100
mean(data$survived[!train] != surv.pred.test)*100

# Good result!


