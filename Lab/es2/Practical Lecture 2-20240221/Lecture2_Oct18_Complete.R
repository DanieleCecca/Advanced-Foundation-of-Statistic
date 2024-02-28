################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 2                                                    #
# Wednesday, October 18, 2023                                  #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Point Estimation and Confidence Intervals
##  2. Hypothesis testing
##  3. Simple Linear Regression (model parameter estimates, confidence intervals, test, prediction)
##  4. EXERCISES FOR HOME 

################################################################ 
# 1. Point Estimation and Confidence Intervals                 #
################################################################
# *Exercise*
# Introduction to ggplot2
# https://ggplot2-book.org/introduction.html
# Confidence intervals interpretation
# TEXT 
# Consider 200 samples of size 20 generated from a normal distribution with mean = 2 and 
# sd = 0.09.
# QUESTION: 
# Compute the 95% confidence intervals for the sample mean and plot them by highlighting those 
# that do not contain the true value of the parameter.
library(dplyr)
library(ggplot2)
library(gridExtra)

samp <- vector(mode = "list", length = 0) 
xmean <- matrix(0, 200, 1)

for (s in 1:200){
  samp[[s]] <- rnorm(20, 2, 0.09)
  xmean[s,1] <- mean(samp[[s]])
}

mean.int <- as.data.frame(xmean)
mean.int <- mean.int %>% mutate(upper = mean.int$V1 + (0.09/sqrt(20))*qnorm(0.975)) %>% mutate(lower = mean.int$V1 - (0.09/sqrt(20))*qnorm(0.975))
id.samp <- seq(1,200,1)
id.samp <- as.data.frame(id.samp)

CI95 <- cbind(id.samp, mean.int)
CI95 <- CI95 %>% mutate(Contain = ifelse(lower < 2, ifelse(upper > 2, 1, 0), 0))
CI95$Contain <- factor(CI95$Contain, levels = c(0,1))
colorset = c('0'='red','1'='black')

CI_plot_95 <- CI95 %>% ggplot(aes(x = id.samp, y = mean.int$V1)) + geom_point(aes(color = Contain)) + geom_errorbar(aes(ymin = lower, ymax = upper, color = Contain)) + scale_color_manual(values = colorset) + coord_flip() + geom_hline(yintercept = 2, linetype = "dashed", color = "blue") + labs(title = "95% Confidence Intervals for the Sample Mean") + theme(plot.title = element_text(hjust = 0.5)) + ylim(1.9,2.1)
grid.arrange(CI_plot_95)

(length(which(CI95$Contain == 0))/200)*100

################################################################ 
# 2. Hypothesis testing                                        #
################################################################
# *Ex. 35 page 345 (modified)*
# TEXT
# The following data concerns time  (min) to repair a rail break in the high rail on a curved 
# track of a certain railway line.
rail <- as.vector(c(159, 120, 480, 149, 270, 547, 340, 43, 228, 202, 240, 218))

# QUESTIONS:
# (a) Describe the data.
# (b) Assuming that it is plausible that the population of repair time is at least approximately 
# normal, is there compelling evidence for concluding that true average repair time is different 
# form 200 min? Carry out a test of hypotheses using a significance level of .05.
# HOMEWORK:
# Do exercise 35 as it is in the book and compute point c.

# (a) Descriptive Statistics
length(rail)

summary(rail)

# (b) Hypothesis testing
# H0: mu = 200
# H1: mu != 200
# Since n < 30, we have to use the Student's t statistic.
tm <- mean(rail)
tsd <- sd(rail)

t.stat <- (tm-200)/(tsd/sqrt(length(rail)))

p.val <- 2*pt(abs(t.stat), length(rail)-1, lower.tail = F)

p.val

p.val < 0.05

# Thus, we do not reject H0 at alpha = 0.05. 
# This means that we have insufficient evidence to conclude that the true average repair time
# differs from 200 minutes.

#####################################################
# EXAMPLE
# Represent the difference between a t distribution and a standard normal distribution as n grows.
x_dt <- seq(- 5, 5, by = 0.001)
plot(x_dt, dt(x_dt, df = 3), type = "l", main = "t distribution vs standard normal Distribution", xlab = "x", ylab = "Density", col = "gray", ylim = c(0, 0.45))
lines(x_dt, dt(x_dt, df = 5), type = "l", col = "lightblue", lwd = 1, add = T)
lines(x_dt, dt(x_dt, df = 10), type = "l", col = "red", lwd = 1, add = T)
lines(x_dt, dt(x_dt, df = 30), type = "l", col = "blue", lwd = 1, add = T)
lines(x_dt, dnorm(x_dt), type = "l", col = "black", lwd = 1, add = T)
legend("topright", c("n = 3", "n = 5", "n = 10", "n = 30", "N(0,1)"), col = c("gray", 
                                                                              "lightblue",
                                                                              "red",
                                                                              "blue",
                                                                              "black"),
       bty = "n", pch = 19, lty = 1, border = "white")  

################################################################ 
# 3. Simple Linear Regression                                  #
################################################################
# Go to UCI repository
# https://archive.ics.uci.edu/ml/datasets/Automobile
# and download the data set and its description.

# Import the data set. I call it "Automobiles".

# Replace the values "?" with NA.
? which

NAval <- which(Automobiles == "?", arr.ind=T)
for (i in 1:dim(NAval)[1]){
  Automobiles[NAval[i,1],NAval[i,2]] <- NA
}
View(Automobiles)

# Consider the variables 'horsepower' (V22) and 'price' (V26).
str(Automobiles)
data <- as.data.frame(cbind(as.numeric(Automobiles$V22), as.numeric(Automobiles$V26)))
colnames(data) <- c("Horsepower", "Price")
head(data)

# Remove the NA values.
data <- as.data.frame(data[-which(is.na(data), arr.ind = T)[,1],])

str(data)

# Data description.
summary(data)

# Is there a relationship between the two variables?
par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20)

# Which kind of relationship?
cor(data) 

# Fit a simple linear regression model and interpret the results.
? lm

reg <- lm(Price ~ Horsepower, data = data)
reg

names(reg)

# Which is the interpretation of the coefficient?
coef(reg)

# Plot the regression line.
par(mfrow = c(1,1))
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", pch = 20)
abline(reg, col = "red", lwd = 2)
points(mean(data$Horsepower),mean(data$Price), pch = "+", col = "purple")

################################################################ 
# 4. EXERCISES FOR HOME                                        #
################################################################
# 1) Consider the exercise in Part 5. Increase the number of generated values to 100000 and comment the results.
# 2) *Ex. 71 page 130*
#     TEXT
#     A geologist has collected 10 specimens of basaltic rock and 10 specimens of granite. The geologist 
#     instructs a laboratory assistant to randomly select 15 of the specimens for analysis.
#     QUESTIONS:
#     (a) What is the pmf of the number of granite specimens selected for analysis?
#     (b) What is the probability that all specimens of one of the two types of rock are selected for analysis?
#     (c) What is the probability that the number of granite specimens selected for analysis is within 1 
#     standard deviation of its mean value?
# 3) *Exercise (TEXT AND QUESTIONS)*
#     (a) Compute the probability of getting 4 heads when 7 coins are tossed.
#     (b) Suppose 35% of all households in an Italian big city have three cars.
#     what is the probability that a random sample of 80 households will contain at least 30 
#     households that have three cars?
#     (c) Is it possible to use the normal approximation? If yes, in which way?
# 4) *Exercise 5 page 285*
#     TEXT
#     Assume that the helium porosity (in percentage) of coal samples taken from any particular seam
#     is normally distributed with true standard deviation .75.
#     QUESTIONS:
#     (c) How large a sample size is necessary if the width of the 95% interval is to be .40?
#     (d) What sample size is necessary to estimate true average porosity to within .2 with 99% 
#     confidence?
# 5) *Ex. 20 page 294 (modified)*
#     TEXT
#     TV advertising agencies face increasing challenges in reaching audience members because viewing
#     TV programs via digital streaming is gaining in popularity. 
#     The Harris poll reported on November 13, 2012, that 53% of 2343 American adults surveyed said 
#     they have watched digitally streamed TV programming on some type of device.
#     QUESTIONS:
#     (a) Calculate and interpret a confidence interval at the 99% confidence level for the 
#     proportion of all adult Americans who watched streamed programming up to that point in time.
#     (b) Compute also 90% and 95% confidence intervals. Which are the differences? 
#     (c) What sample size would be required for the width of a 99% CI to be at most .05 irrespective
#     of the value of phat? 
# 6) *Ex. 21 page 334*
#     TEXT
#     The desired percentage of SiO_2 in a certain type of aluminous cement is 5.5. 
#     To test whether the true average percentage is 5.5 for a particular production facility, 
#     16 independently obtained samples are analyzed. 
#     Suppose that the percentage of SiO_2 in a sample is normally distributed with sigma = 0.3 
#     and that xbar = 5.25.
#     QUESTIONS:
#     (a) Does this indicate conclusively that the true average percentage differs from 5.5?
#     (b) If the true average percentage is mu = 5.6 and a level alpha =  0.01 test based on n = 16
#     is used, what is the probability of detecting this departure from H0?
#     (c) What value of n is required to satisfy alpha = 0.01 and beta(5.6) = .01?
# 7) *Ex. 37 page 345*
#     TEXT
#     The following data concerns the cube compressive strength (MPa) of concrete specimens 

data <- c(112.3, 97.0, 92.7, 86.0, 102.0, 99.2, 95.8, 103.5, 89.0, 86.7)

#     QUESTIONS:
#     (a) Is it plausible that the compressive strength for this type of concrete is normally 
#     distributed?
#     (b) Suppose the concrete will be used for a particular application unless there is strong 
#     evidence that true average strength is less than 100 MPa. Should the concrete be used? 
#     Carry out a test of appropriate hypotheses.
