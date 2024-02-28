################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 1                                                    #
# Wednesday, October 11, 2023                                  #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  0. Introduction to R Studio
##  1. Objects in R
##  2. Vectors and Matrices
##  3. Operations among scalars, vectors and matrices in R 
##  4. Descriptive Statistics
##  5. Discrete Random Variable and Probability Distribution
##  6. Continuous Random Variables, Random Samples and Sampling Distributions
##  A. Appendix: data.frame and list    

################################################################ 
# 0. Introduction to R Studio                                  #
################################################################ 

## The R Studio window is composed of: 
# TL: R Script, where you can write your codes to be run.
# TR: Environment (where you can save your objects)/History (list of run commands).
# BL: Console, where you can run your commands.
# BR: Files/Plots/Packages/Help (where you can find information on function, packages
#     etc.)/Viewer. 

# Commands in R Studio can be run: with "Run" (top right in the R script),
# OR "Copy and Paste" the command into the console, 
# OR using "ctrl + Enter" (Windows)/"cmd + Enter" (macOS)

## Useful commands
# Clear Console
cat("\014")  
# Remove all the objects from the Environment
rm(list = ls())
# Open the help from the console 
# ? "Name Function"

## When you work in R Studio, it is useful to set the working directory, i.e., the 
## folder where you save your scripts, have your data and save your results.

## Set the working directory: "Session" --> "Set Working Directory" --> "Choose Directory..."
## OR
setwd("Name of the Directory") # Insert your directory
getwd() 
# Example: setwd("C:/Users/User/Desktop")
# Attention:: you have to use "/" instead of "\".


################################################################ 
# 1. Objects in R                                              #
################################################################ 

## R Studio can be used as calculator
100+120
100*3
100/5
100-20

## Assign a number to a variable 
a = 3
# OR
a <- 3
# How to see what "a" contains
a


## See also: assign a character to a variable
# b <- "Statistics"
# b

## See also: assign a logical element to a variable
# c <- FALSE
# c

# See also:
# d <- 8>4
# d

## See also: verify the nature of objects
# ? mode
# mode(a)
# mode(b)
# mode(c)

# mode(d)

# OR, asking a "direct question"
# is.numeric(a)
# is.character(a)
# is.logical(a)
# 
# is.character(b)
# is.numeric(b)
# is.logical(b)
# 
# is.logical(c)
# is.numeric(c)
# is.character(c)

## Change the nature of an object
# as.character(a)
# as.logical(b) ## ATTENTION:: NA = Not Available --> b cannot be changed into a logical variable

# ? NA

## Relational Operators
# Numeric
# Recall a
a
a == 3 # "==" equal to
a != 3 # "!" different from
a > 2 # ">" greater than
a <= 4 # "<=" lower or equal to

# See also: the Boolean case
# !TRUE # "!" NOT
# TRUE & FALSE # "&" AND
# TRUE & TRUE # "&" AND
# TRUE | FALSE # "|" OR


################################################################ 
# 2. Vectors and Matrices                                      #
################################################################ 

## Vectors
# Create a numeric vector with *pre-defined* elements
v1 <- c(2,5,4,15,35,14,25,44,50,23)
v1
? str
str(v1)

# OR
# ? matrix
v11 <- matrix(data = v1, nrow = 10, ncol = 1) # OR v11 <- matrix(v1,10,1), see "matrix" in the help
v11
str(v11)

# See also: create a character vector with *pre-defined* elements
# v12 <- c("Mathematics", "Statistics", "Probability")
# v12
# str(v12)

# Create a sequence
v2 <- seq(from = 1, to = 10, by = 1) # OR v2 <- seq(1,10,1)
v2

# OR 
v21 <- seq(from = 1, to = 10, length = 10)
v21
# Is v22 equal to v2?
v21 == v2

# See also:
# seq(from = 1, to = 10, by = 1)
# seq(from = 1, to = 10, length = 5)

# Initialize a numeric null vector
v3 <- c()
v3
# OR
v31 <- vector(mode = "numeric", length = 0) 
v31
str(v31)

# See also: how to initialize a character null vector
# v32 <- vector(mode = "character", length = 0)
# v32
# str(v32)

# See also: initialize a numeric vector whose the dimension is known a priori
# v33 <- vector(mode = "numeric", length = 10) 
# v33  
# Alternatively, rep(0, 10) OR matrix(data = 0, nrow = 1, ncol = 10). ATTENTION: if you use "matrix", its structure is different!
str(rep(0, 10))
str(matrix(data = 0, nrow = 1, ncol = 10))

## Matrices
# Create a matrix with *pre-defined* elements
# Recall v1
v1
m1 <- matrix(v1,5,2)
m1

m2 <- matrix(v1,5,2, byrow = T) # Different elements allocation
m2

# Is m1 equal to m2?
m1 == m2
m1

## Dimensions of vectors, matrices...
# Vectors
? length
length(v1)
? dim
dim(v1)
#...ops!
str(v1)
dim(as.matrix(v1)) # We transform v1 into a matrix
# Matrices
dim(m1)
# See also:
# nrow(v11)
# ncol(v11)


## See also: other useful notions on matrix initialization
# Initialize a diagonal matrix
# d1 <- diag(x = 1, nrow = 10, ncol = 10) #OR d1 <- diag(1,10,10)
# d1
# 
# # Create a diagonal matrix with pre-defined diagonal elements
# d2 <- diag(v1,10,10)
# d2

# Create a matrix with equal elements 
# me1 <- matrix(2,5,3)
# me1
# 
# me2 <- matrix(T,5,2)
# me2


## Selection of elements in a vector or matrix
# Recall v1
v1
# Select the second element of v1
v1[2]

# Recall m1
m1
# Select the element [3,2], i.e., third row, second column
m1[3,2]
# Select the first column of m1
m1[,1]
# Select the fifth row of m1
m1[5,]
# Select the first, third and fifth element of the first column, i.e., [1,1], [3,1], [5,1], of  m1
m1[c(1,3,5),1]
# Select from the second to the fourth elements of the second column, [2,2], [3,2], [4,2], of m1
m1[2:4,2]

## See also: combine vectors to build a matrix
# ? rbind # cbind
# Recall v1
# v1
# Divide V1 in two parts of equal dimension and build a matrix by...
# ...adding the second part to the first part of v1 by column
# m3 <- cbind(v1[1:5],v1[6:10])
# m3==m1

# ...adding the second part to the first part of v1 by row
# m4 <- rbind(v1[1:5],v1[6:10])
# m4

# Recalling m1
# m1
# m4 == t(m1)
# ? t

# See also: select elements with respect to a logical condition
# For instance
# v1[v1>15]

################################################################ 
# 3. Operations between scalars, vectors and matrices in R     #
################################################################
## Vectors
# Recall v2
v2
sum(v2)
prod(v2)
min(v2)
max(v2)
log(v2)
which(v2>6)

# Element-by-element operations with vectors
height = c(1.70, 1.85, 1.60, 1.92, 1.74, 1.92)
weight = c(60, 74, 53, 90, 95, 72)
bmi = weight/height^2 # Body Mass Index
bmi

# Which is the difference between bmi and...
diff <- (weight/height)^2
diff
diff != bmi

## What happens with missing data?
vm <- c(1,5,NA,2,4)
vm

is.na(vm)
which(is.na(vm))

# See also:
# !is.na(vm)
# which(!is.na(vm))

## See also:
## Scalars
# Recall a
# a
# a*100
# ((a/10)*4)-0.2
# sqrt(a)
# log(a) # See the log function in the help 
# -a
# abs(-a)

## Boolean vectors
# vl <- c(T,T,F,F,T)
# vl

## Useful functions
# ? any
# any(vl)
# ? all
# all(vl)
# ? which
# which(vl)

## Matrices
# Recall m1
m1
m1*3

# Matrix multiplication
# Element-by-element multiplication
m1*m2
# *Pay attention to the matrices' dimensions! If an element-wise multiplication has to be performed,
# the matrices must have the same dimensions.
# dim(m1) == dim(m2)

# Matrix multiplication
? '%*%'
m1%*%m2
# ...ops!
# The matrix dimensions must be conformable, i.e., [5,2] x [5,2] doesn't work!
m1%*%t(m2)

# See also: inversion of a square matrix
# Create a matrix by merging three vectors by row
# vec1 <- c(2,4,5)
# vec2 <- c(10,13,15)
# vec3 <- c(-2,-5,-4)
# vectot <- rbind(vec1,vec2,vec3)
# vectot
# det(vectot)
# solve(vectot)  

################################################################ 
# 4. Descriptive Statistics                                     #
################################################################
# Start working with data
# *Ex. 46 page 44 (modified)* 
# TEXT
# The article "Effects of Short-Term Warming on Low and High Latitude Forest Ant Communities" (Ecosphere,
# May 2011, Article 62) described an experiment in which observations on various characteristics were 
# made using minichambers of three different types: (1) cooler (PVC frames covered with shade cloth), 
# (2) control (PVC frames only), and (3) warmer (PVC frames covered with plastic). One of the article's 
# authors kindly supplied the accompanying data on the difference between air and soil temperatures (?C).

# QUESTIONS
# (d) Construct a comparative boxplot and comment on any interesting features.
# (b) Calculate, interpret, and compare the standard deviations for the three different samples.
# (new) By replacing the NA value of 'Warmer' with 2.30, compute the correlation coefficient between
#       'Cooler' and 'Warmer'.

# Import the data set named "WarmingEffects.txt"
data <- WarmingEffects

# (d) Compute the descriptive statistics for this data set
? summary
summary(data)

# How to represent descriptive statistics?
# BOXPLOT
? boxplot
boxplot(data)
? boxplot.stats

# If we want to display also the mean...
? apply
mean.data <- apply(data, 2, function (x) mean(x, na.rm = T)) 
mean.data
# ATTENTION 
# We need to define a function in 'apply' since the length of each column of data is different and 
# it is necessary to remove the NA values --> 'apply(data,2,mean)' does not work.
points(mean.data, pch = 19, col = "red", lwd = 2) 
# See this link https://r-charts.com/base-r/pch-symbols/ for different 'pch' 

# Negative or positive skewness?

# See also:
install.packages("moments")
library(moments)

apply(data, 2, function (x) skewness(x, na.rm = T))

# (b) Compute the standard deviations using the function 'apply'
# ? sd
apply(data, 2, function (x) sd(x, na.rm = T)) 

# (new) Replace the NA value of 'Warmer' with 2.30
data.new <- data
data.new$Warmer[length(data.new$Cooler)] <- 2.30
data.new
cor(data.new$Cooler, data.new$Warmer)

################################################################ 
# 5. Discrete Random Variable and Probability Distribution     #
################################################################ 
# *Exercise* 
# SIMULATING FROM THE POISSON DISTRIBUTION
# TEXT 
# Let assume to consider a Poisson distribution with different values of mu. 
# QUESTIONS: 
# (a) How does this distribution change as mu increases? Choose mu in the set {1:5}.
# (a) Generate 200 random numbers from a Poisson distribution with mu = 2.
# (b) Display the results in a plot.

# (a) Define the sequence of the parameter values
param <- seq(1, 5, by = 1)

? dpois

# Use the plot function
max.range = 15
plot(0:max.range, dpois(0:max.range, lambda = param[1]), xlim = c(0, max.range), type = "b", col = "red",
     lwd = 2, pch = 19, main = "Pois(1:5)", xlab = "x", ylab = "Probability")
lines(0:max.range, dpois(0:max.range, lambda = param[2]), type = "b", col = "lightblue", lwd = 2, pch = 19)
lines(0:max.range, dpois(0:max.range, lambda = param[3]), type = "b", col = "yellow", lwd = 2, pch = 19)
lines(0:max.range, dpois(0:max.range, lambda = param[4]), type = "b", col = "green", lwd = 2, pch = 19)
lines(0:max.range, dpois(0:max.range, lambda = param[5]), type = "b", col = "violet", lwd = 2, pch = 19)
legend("topright", c("Mu = 1", "Mu = 2", "Mu = 3", "Mu = 4", "Mu = 5"), col = c("red", 
                                                                      "lightblue",
                                                                      "yellow",
                                                                      "green",
                                                                      "violet"),
       bty = "n", pch = 19, lty = 1, border = "white")  

# N.B. The Poisson distribution is a discrete distribution. The plotted lines are used only for a more 
#      comprehensible representation.

# (b) Check for the correct function
? rpois 
# Initialization
nsim <- 200

pois.mu <- 2

pois.sim <- vector(mode = "numeric", length = 200)

# Random generation
pois.sim <- rpois(nsim, pois.mu)

# (c) Use the plot function to show both the theoretical and the empirical distribution
max.range = 8
par(mfrow = c(1,1))
plot(0:max.range, dpois(0:max.range, lambda = pois.mu), xlim = c(0, max.range), type = "p", col = 2, lwd = 2,
     pch = 19, main = "Pois(2), 200 simulations", xlab = "x", ylab="Probability")
lines(sort(unique(pois.sim)), prop.table(table(pois.sim)), type = "h")
legend("topright", c("Theorical", "Empirical"), col = c(2, 1), bty = "n", pch = 19, lty = 1)



######################################################################################## 
# 6. Continuous Random Variables, Random Samples and Sampling Distributions            #
########################################################################################
# *Ex. 43 page 230 (modified)*
# TEXT
# Suppose the amount of liquid dispensed by a certain machine is: 
# (a) uniformly distributed with lower limit A = 5 oz and upper limit B = 10 oz. 
# (b) normally distributed with mean equal to 6.5 and variance equal to 3.25 (HOME)
# QUESTION:
# Describe how you would carry out simulation experiments to compare the sampling distribution
# of the (sample) fourth spread for sample sizes N = 5, 10, 20, 50 in both cases (a) and (b).
n <- c(5, 10, 20, 100)

# Initialization
sim.unif <- vector(mode = "list", length = 0)
stat <- matrix(0, 500, length(n))

? runif
A = 5
B = 10
set.seed(123)

for (i in 1:length(n)){
  for (s in 1:500){
    sim.unif[[s]] <- runif(n[i], A, B)
    stat[s,i] <- quantile(sim.unif[[s]], 0.75) - quantile(sim.unif[[s]], 0.25)
  }
}

par(mfrow = c(4,2))
for (i in 1:length(n)){
  hist(stat[,i], xlab = "", prob = T, main = n[i])
  curve(dnorm(x, mean = mean(stat[,i]), sd = sd(stat[,i])), add = T)
  qqnorm(stat[,i], plot.it = T, pch = 19, lwd = 2)
  qqline(stat[,i], col = "red", lwd = 2)
}


################################################################ 
# A. Appendix: data.frame and list                             #
################################################################
# We saw how to combine this two matrices together.
# Alternatively...
? data.frame
dataf <- data.frame(m1, m2)
dataf
dim(dataf)
str(dataf) 

str(cbind(m1,m2)) # Different object!

# Labels of rows and columns
rownames(dataf)
colnames(dataf)
# Rename the columns of dataf
colnames(dataf) <- c("c1","c2","c3","c4")
colnames(dataf)
dataf

# See row and column names
dimnames(dataf)

# Inspect the difference between cbind(m1,m2)) and dataf
? class
class(cbind(m1,m2))
? mode
mode(cbind(m1,m2))

class(dataf)
mode(dataf)

# Select the element of a data frame
dataf[2,3]
# Select the second column of dataf
dataf[,2]
# OR by name (for columns) --> USEFUL!!
dataf$c2

# What is a list? Which is the difference between a list and a vector/matrix?
# The list can contain objects of different types!
mix <- list("Statistics", 28, TRUE)
mix
mode(mix)

# Which is the difference with the use of "c"?
mix1 <- c("Statistics", 28, TRUE)
mix1
mode(mix1) # All elements are considered as "character"!

mix2 <- list(c("Statistics", "Mathematics"), c(28,30), c(TRUE,TRUE))
mix2

# Select the elements from a list
mix[1]
mix[2]
mix[3]
# This syntax is not completely correct...
mix[[1]]
mix[[2]]
mix[[3]]

# Recall mix2
mix2
# Select the character element of the first element of the list, i.e., Statistics.
mix2[[1]][1]
# Select the numeric element corresponding to the mark of Mathematics, i.e., 30.
mix2[[2]][2]


