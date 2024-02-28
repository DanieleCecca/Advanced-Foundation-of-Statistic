################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 3                                                    #
# Wednesday, October 25, 2023                                  #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################
##  1. Simple Linear Regression (model parameter estimates, confidence intervals,
##     test, prediction)
##  2. Introduction to the definition of outliers: their classification 

################################################################ 
# 1. Simple Linear Regression                                  #
################################################################
# Reload the Automobile data set we use at the end of the last lecture.
# Run the following lines, that we have already introduced before.

NAval <- which(Automobiles == "?", arr.ind=T)
for (i in 1:dim(NAval)[1]){
  Automobiles[NAval[i,1],NAval[i,2]] <- NA
}

data <- as.data.frame(cbind(as.numeric(Automobiles$V22), as.numeric(Automobiles$V26)))
colnames(data) <- c("Horsepower", "Price")
head(data)

data <- as.data.frame(data[-which(is.na(data), arr.ind = T)[,1],])

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20)

reg <- lm(Price ~ Horsepower, data = data)
summary(reg)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20)
abline(reg, col = "red", lwd = 2)
points(mean(data$Horsepower),mean(data$Price), pch = "+", col = "purple")

# Compute confidence intervals for the model parameters and interpret them.
confint(reg)
# INTERPRETATION: For each increase of ten units of Horsepower, there will be an *average* increase
# in the car price between 1547 and 1897 dollars.

# Compute a test on the model parameters and the model fit.
# H0: beta = 0
# H1: beta != 0
summary(reg)

cor(data)[1,2]^2

# *Difference between confidence intervals and prediction intervals*
# Prediction of the price when the horsepower is 200, given the estimated model.
? predict

# Confidence interval
predict(reg, data.frame(Horsepower = (c(200))),
        interval = "confidence")

# Prediction interval
predict(reg, data.frame(Horsepower = (c(200))),
        interval = "prediction")

# INTERPRETATION: they are centered around the same point (prediction, a predicted value of $29879.08 
# when the horsepower is 200, but the prediction interval is wider than the confidence one.
# Indeed, the *prediction interval* reflects the increased uncertainty about the price for a 
# *given automobile*;
# whereas the *confidence interval* reflects the increased uncertainty about the 
# *average prices* over different automobiles.

# Notice that...
t(as.vector(coef(reg)))%*%rbind(1,200)

# *Model assessment plots*
# Problems when fitting linear regressions:
# 1. Non-linearity of the response-predictor relationships
# 2. Correlation of error terms
# 3. Non-constant variance of error terms
# 4. Outliers
# 5. High-leverage points
# Also collinearity, but in multiple linear regression...

## 1. Why is the non-linearity a problem?
# Because the linear regression model assumes that there is a straight-line (linear) relationship
# between the predictor(s) and the response/outcome!!!
# If the underlying (theoretical, for the population) model is not linear, the conclusions we draw
# from the data are not representative of the reality!

# RESIDUAL PLOT (predictors vs residuals)

# Remember that residuals = y_{i} - hat{y}_{i}
plot(predict(reg), residuals(reg), xlab = "Fitted values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "lightgray")

# INTERPRETATION
# The presence of a *specific pattern* may indicate that the linear model is not appropriate.
# If points are randomly dispersed, the linear model is appropriate.


## 2. Why is the correlation of error terms a problem?
# Because one the assumption of the linear model is that residuals are i.i.d. (independent and [...]),
# thus uncorrelated.
# What does it mean?
# It means that if the errors are uncorrelated, then the fact that one residual is positive provides
# no or little information about the sign of another residual.
# PAY ATTENTION ON THE DIFFERENCE (AND RELATIONSHIP) BETWEEN INDEPENDENCE AND CORRELATION.
# --> Erroneous conclusions, for instance, on the significance of a model parameter!!!

## 3. Why is the non-constant variance (heteroscedasticity) of the errors a problem?
# Example: the variances of the error terms increase with the value of the response.
reg.nl <- lm(log(Price) ~ Horsepower, data = data)
plot(predict(reg.nl), residuals(reg.nl), xlab = "Fitted values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "lightgray")

## 4. Why is the presence of outliers a problem? What is an outlier?
# Example:
par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black")
abline(reg, col = "blue", lwd = 2)
points(x = 75, y = 40000, pch = "+", col = "red")

# HOMEWORK: Try to fit the regression line including the aforementioned outlier and comment the results
# in comparison with the original analysis.

################################################################################
# 2.  Introduction to the definition of outliers: their classification         #
################################################################################
## Three different types of outliers exist:
# A. VERTICAL/ORTHOGONAL OUTLIERS
# B. BAD LEVERAGE POINTS
# C. GOOD LEVERAGE POINTS

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black")
abline(reg, col = "blue", lwd = 2)

# EXAMPLE OF VERTICAL/ORTHOGONAL OUTLIERS (REGRESSION OUTLIERS)
v.out <- as.data.frame(cbind(rnorm(mean = 100, sd = 5, 5), rnorm(mean = 40000, sd = 2, 5)))

colnames(v.out) <- colnames(data)

data.vout <- rbind(data, v.out)

reg.vout <- lm(Price ~ Horsepower, data = data.vout)

coefficients(reg)
coefficients(reg.vout)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black", 
     main = "VERTICAL OUTLIERS")
abline(reg, lwd = 2, col = "blue")
points(v.out, pch = 20, col = "red")
abline(reg.vout, lwd = 2, col = "red")

# ATTENTION: See what happens if...
v.out2 <- as.data.frame(cbind(rnorm(mean = 100, sd = 5, 5), rnorm(mean = 30000, sd = 2, 5)))

colnames(v.out2) <- colnames(data)

data.vout2 <- rbind(data, v.out2)

reg.vout2 <- lm(Price ~ Horsepower, data = data.vout2)

coefficients(reg)
coefficients(reg.vout2)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black", 
     main = "VERTICAL OUTLIERS")
abline(reg, lwd = 2, col = "blue")
points(v.out2, pch = 20, col = "red")
abline(reg.vout2, lwd = 2, col = "red")

# QUESTION: why in this case does the regression line change?

# EXAMPLE OF BAD LEVERAGE POINTS (REGRESSION OUTLIERS)
bl.out <- as.data.frame(cbind(rnorm(mean = 300, sd = 3, 5), rnorm(mean = 5000, sd = 2, 5)))

colnames(bl.out) <- colnames(data)

data.blout <- rbind(data, bl.out)

reg.blout <- lm(Price ~ Horsepower, data = data.blout)

coefficients(reg)
coefficients(reg.blout)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black", 
     xlim = c(0, 500), ylim = c(0, 40000), main = "BAD LEVERAGE POINTS")
abline(reg, lwd = 2, col = "blue")
points(bl.out, pch = 20, col = "green")
abline(reg.blout, lwd = 2, col = "green")

# EXAMPLE OF GOOD LEVERAGE POINTS 
gl.out <- as.data.frame(cbind(rnorm(mean = 300, sd = 3, 5), rnorm(mean = 50000, sd = 0.09, 5)))

colnames(gl.out) <- colnames(data)

data.glout <- rbind(data, gl.out)

reg.glout <- lm(Price ~ Horsepower, data = data.glout)

coefficients(reg)
coefficients(reg.glout)

par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20, col = "black", 
     xlim = c(0,400), ylim = c(0, 55000), main = "GOOD LEVERAGE POINTS")
abline(reg, lwd = 2, col = "blue")
points(gl.out, pch = 20, col = "violet")
abline(reg.glout, lwd = 2, col = "violet", lty = 2)


# STUDENTIZED RESIDUAL PLOT
# The studentized residuals are computed by dividing each residual by its estimated standard error.

plot(predict(reg), rstudent(reg), xlab = "Fitted values", ylab = "Studentized residuals", pch = 20)

par(mfrow = c(2, 2))
plot(hatvalues(reg), rstudent(reg), xlab = "Leverage", ylab = "Studentized residuals")
which.max(hatvalues(reg))
abline(h = -2, col = "lightgray")
abline(h = 2, col = "lightgray")

plot(hatvalues(reg.vout), rstudent(reg.vout), xlab = "Leverage", ylab = "Studentized residuals")
abline(h = -2, col = "lightgray")
abline(h = 2, col = "lightgray")

plot(hatvalues(reg.blout), rstudent(reg.blout), xlab = "Leverage", ylab = "Studentized residuals")
abline(h = -2, col = "lightgray")
abline(h = 2, col = "lightgray")

plot(hatvalues(reg.glout), rstudent(reg.glout), xlab = "Leverage", ylab = "Studentized residuals")
abline(h = -2, col = "lightgray")
abline(h = 2, col = "lightgray")

# FINALLY! Interactive plot
par(mfrow = c(2, 2))
plot(reg)

