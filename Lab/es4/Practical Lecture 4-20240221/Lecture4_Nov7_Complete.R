################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 4                                                    #
# Tuesday, November 7, 2023                                    #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  0. Data set uploading
##  1. Multiple Linear Regression (model parameter estimates, confidence intervals,
##     test, prediction, evaluation, qualitative predictors)

# 0. Re-load the Automobile data set we use during the last lectures.
#    Run the following lines, that we have already introduced before.

NAval <- which(Automobiles == "?", arr.ind=T)
for (i in 1:dim(NAval)[1]){
  Automobiles[NAval[i,1],NAval[i,2]] <- NA
}

################################################################ 
# 1. Multiple Linear Regression                                #
################################################################
# Consider now the variables 'horsepower' (V22), 'highway-mpg' (V25), 
# 'length' (V11), 'stroke' (V20) and 'price' (V26) and remove the observations 
# with NA values.
data.mult <- as.data.frame(cbind(as.numeric(Automobiles$V22), as.numeric(Automobiles$V25),
                             as.numeric(Automobiles$V11), as.numeric(Automobiles$V20),
                             as.numeric(Automobiles$V26)))

data.mult <- as.data.frame(data.mult[-which(is.na(data.mult), arr.ind = T)[,1],])
colnames(data.mult) <- c("Horsepower", "Highway", "Length", "Stroke", "Price")

str(data.mult)

# Represent the data.
pairs(data.mult)

# Compute a multiple linear regression.
reg.mult <- lm(Price ~ ., data = data.mult)
summary(reg.mult)

# QUESTIONS
# 1. Are the coefficients statistically significant?
# 2. Interpret the coefficients.
# 3. What does the F statistic represent? What can we conclude by looking at the F-statistic?
# 4. Compute the confidence intervals for the coefficient at confidence level 90%.
# 5. Model evaluation. Which assumptions seem to not hold? Are there vertical outliers or leverage points?
# 6. Are the predictors collinear? Which is the problem related to collinearity?

# 1. Are the coefficients statistically significant?
# Look at... 
summary(reg.mult)

# Perform variable selection.
reg.mult2 <- lm(Price ~ Horsepower + Length, data = data.mult)
summary(reg.mult2)

# 2. Interpret the coefficients.
# Considering fixed all the other predictors, an increase of ten units in Horsepower corresponds to an average increase
# in price of $1307.37.

# Considering fixed all the other predictors, an increase of ten units (cm) in Length corresponds to an average increase
# in price of $2173.20. 

# 3. What does the F statistic represent? What can we conclude by looking at the F-statistic?
# We use the F statistic to test the hypothesis system:
# H0: all the regression coefficients are equal to zero.
# H1: at least one regression coefficient differs from zero.

# We reject here the null hp (H0) since the p-value is lower than any significance level alpha (strong evidence).

# 4. Compute the confidence intervals for the coefficient at confidence level 90%.
confint(reg.mult2, level = 0.90)

# 5. Model evaluation. Which assumptions seem to not hold? Are there vertical outliers or leverage points?
par(mfrow = c(2,2))
plot(reg.mult2)

# 75 --> Vertical outlier
# 129 --> Bad leverage point
# 128 --> Good leverage point

# 6. Are the predictors collinear? Which is the problem related to collinearity?
# Simplest ways
# A
par(mfrow = c(1,1))
plot(data.mult$Horsepower, data.mult$Length, pch = 20) 
# B
cor(data.mult$Horsepower, data.mult$Length)

# With more than 2 predictors...(recall the complete model)
# VARIANCE INFLATION FACTOR 
# The smallest possible value for VIF is 1, which indicates the complete absence of collinearity.
# As a RULE OF THUMB, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
library(car)
vif(reg.mult)
vif(reg.mult2)

# ATTENTION: 
# Collinearity poses problems in the regression model, since it can be difficult to separate out the individual
# effects of collinear variables on the response.
# Uncertainty on the regression coefficients corresponds in an increase of the standard error and a 
# decrease of the t-statistic. As a result, we may fail to reject the null hp on a regression coefficient.

# How to fix the multicollinearity problem?
# A. Drop one of the problematic variables from the regression.
# B. Combine the collinear variables together into a single predictor.

# QUALITATIVE PREDICTORS
# Consider also the variable 'fuel.type' (V4).
data.new <- as.data.frame(cbind(as.numeric(Automobiles$V22), as.numeric(Automobiles$V11), 
                                as.numeric(Automobiles$V26), as.character(Automobiles$V4))) # Because of the missing data

data.new <- as.data.frame(data.new[-which(is.na(data.new), arr.ind = T)[,1],])
colnames(data.new) <- c("Horsepower", "Length", "Price", "Fuel")

str(data.new)

data.new$Horsepower <- as.numeric(data.new$Horsepower)
data.new$Length <- as.numeric(data.new$Length)
data.new$Price <- as.numeric(data.new$Price)

str(data.new)

# R automatically create a dummy variable for "Fuel": 1 for gas, 0 for diesel.
? contrasts
contrasts(as.factor(data.new$Fuel))

reg.mult.qual <- lm(Price ~ ., data = data.new)
summary(reg.mult.qual)

# INTERPRETATION OF THE QUALITATIVE VARIABLE COEFFICIENT.
# Thus, the negative 'Beta Fuelgas' coefficients indicates that a car with gas fuel is associated with a lower
# price with respect to a car with diesel fuel.

