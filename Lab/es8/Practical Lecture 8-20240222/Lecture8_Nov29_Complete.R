################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 8                                                    #
# Wednesday, November 29, 2023                                 #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Linear Discriminant Analysis 
##  2. Quadratic Discriminant Analysis

cat("\014")
rm(list = ls())

################################################################ 
# 1. Linear Discriminant Analysis                              #
################################################################
# Exercise 13 page 175
## Using the Boston data set, fit classification models in order to predict
## whether a given suburb has a crime rate above or below the median.
## Explore LDA and QDA models using various subsets of the predictors. 
## Describe your findings.

# We want to fit a classification model, starting from a LDA, in order to predict 
# whether a given suburb has a crime rate above or below the median.

## ATTENTION::
## This is a classification problem and not a clustering problem because we have 
## observed the response variable. We use classification models for predictions
## by building a good model on the observed data and use it for future predictions
## on data where only the predictors are known.

library(MASS)

? MASS:: Boston

data.city <- Boston

head(data.city)

attach(data.city)

# See the variables crim.
summary(crim)

# We want to fit a classification model in order to predict whether a given suburb has a crime rate above or 
# below the median.
# The variable "crim" is numeric, whereas the request is "to fit a classification model,
# starting from a LDA, in order to predict  whether a given suburb has a CRIME RATE
# ABOVE OR BELOW THE MEDIAN".
# ATTENTION: first of all, we need to create a new variable that specifies if a suburb has a crime rate above or 
# below the median, i.e., a dummy variable.

crim01 <- rep("no", nrow(data.city)) # nrow(data.city) = dim(data.city)[1]
crim01[crim > median(crim)] <- "yes"

crim01 <- as.factor(crim01)

data.city <- data.frame(data.city, crim01)

head(data.city)

# Compute descriptive statistics.
summary(data.city)

# Recall the 'psych' library, after having installed if necessary.
# install.packages("psych")
library(psych)

pairs.panels(data.frame(data.city$zn, data.city$indus, data.city$dis, data.city$ptratio, data.city$black, data.city$lstat),
             gap = 0,
             bg = c("red", "green")[data.city$crim01],
             pch = 21)

# Fit a Linear Discriminant Analysis on these data.

# ATTENTION: 
# 1. We have to select only continuous predictors because of the Gaussian assumption!
# 2. We need to split the data set into training and test.

? sample
train <- sample(c(TRUE, FALSE), nrow(data.city), replace = TRUE, prob = c(0.7, 0.3))

library(MASS)

? lda

lda.city <- lda(crim01 ~ zn + indus + dis + ptratio + black + lstat, data = data.city, subset = train)

# Number of observations on which the model has been fitted.
lda.city$N

# Interpretation of the results.
lda.city

# A. Coefficients
lda.city$scaling 
# They represent the LINEAR COMBINATION OF PREDICTORS used to form the LDA
# decision rule (coefficients x predictors).
# We have only one discriminant function since the categories of crim01 are 2 
# (two classes). 

# B. Prior probabilities
lda.city$prior 
contrasts(crim01)
# Remember that...
summary(crim01)

# C. Group means
? Boston
lda.city$means

# Interpretation Example:
# There is a tendency for the proportion of non-retail  business acres 
# per town to be higher for suburbs with a crime rate above the median.

# GRAPHICAL REPRESENTATIONS

plot(lda.city)

# PREDICTION ON THE TEST SET

pred.lda <- predict(lda.city, data.city[!train,])

# See the output.
names(pred.lda)
pred.lda$class

# How is the class membership assessed?
cbind(pred.lda$class[1:10], pred.lda$posterior[1:10,]) # pred.lda$class > 0.5

# MODEL EVALUATION
# Compute the confusion matrix on the test data 
Cmatrix <- addmargins(table(True = crim01[!train], Estimated = pred.lda$class))
Cmatrix

# Compute the % of correct classifications.
mean(pred.lda$class == crim01[!train])*100

# Overall error rate
(1-mean(pred.lda$class == crim01[!train]))*100 # Misclassification

# Sensitivity (True positive rate): TP/P
Cmatrix[2, 2]/Cmatrix[2, 3] 

# Specificity: TN/N
Cmatrix[1, 1]/Cmatrix[1, 3] 

# False positive rate
1 - (Cmatrix[1, 1]/Cmatrix[1, 3])

################################################################ 
# 2. Quadratic Discriminant Analysis                           #
################################################################
## Which is the difference between QDA and LDA?
## 1. Each class has its own covariance matrix (\Sigma depends on k, k = 1,...,K).
## 2. The QDA decision rule depends on x only through a QUADRATIC FUNCTION of x.

# Compute a Quadratic Discriminant Analysis
? qda

qda.city <- qda(crim01 ~ zn + indus + dis + ptratio + black + lstat, data = data.city, subset = train)
qda.city

names(qda.city)

## INTERPRETATION::
qda.city$prior
qda.city$means

# MODEL EVALUATION
qda.class <- predict(qda.city, data.city[!train,])$class

Cmatrix.qda <- addmargins(table(True = crim01[!train], Estimated = qda.class))
Cmatrix.qda

# Compare this results with that one obtained with LDA.
mean(qda.class == crim01[!train])*100
## INTERPRETATION:
## QDA seems to be more accurate in detecting the true relationship than LDA.

# Sensitivity (True positive rate): TP/P
Cmatrix.qda[2, 2]/Cmatrix.qda[2, 3] 

# Specificity: TN/N
Cmatrix.qda[1, 1]/Cmatrix.qda[1, 3] 

# False positive rate
1 - (Cmatrix.qda[1, 1]/Cmatrix.qda[1, 3])

