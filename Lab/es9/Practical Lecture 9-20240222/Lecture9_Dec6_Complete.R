################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 9                                                    #
# Wednesday, December 6, 2023                                  #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Factor Analysis 
##  2. Clustering Methods: K-means

cat("\014")
rm(list = ls())
library(mclust)
library(tclust)
library(MASS) 
library(ggplot2)
library(MixSim)
library(corrplot)

################################################################ 
# 1. Factor Analysis                                           #
################################################################
## Load the data set "Bechtoldt.1" from "library(psych)" and analyze it.
## Look also at file saved as "Bechtoldt Data Set Description.png".
? psych:: Bechtoldt.1

data(Bechtoldt.1)
# This is a correlation matrix.

corrplot(Bechtoldt.1, method = "color", number.digits = 2, number.cex = 0.8, tl.cex = 0.8,
         addCoef.col = "black", tl.col = "black", tl.srt = 45, is.corr = T)

## Select the number of factors.
? eigen 
eigC <- eigen(Bechtoldt.1)$values
plot(eigC, type = "b")

## Fit a Factor Analysis model on these data by selecting the number of factors.
? fa
fa.bech <- fa(Bechtoldt.1, nfactors = 5, n.obs = 212, rotate = "varimax", scores = c("Thompson", "regression", "Bartlett"), fm = "ml")
fa.bech
fa.bech$weights

## Assign each variable to only one factor and interpret the result.
vc <- apply(abs(fa.bech$weights), 1, which.max)

load.disj <- matrix(0, nrow(Bechtoldt.1), 5)
for (i in 1:nrow(Bechtoldt.1)) {
  load.disj[i, vc[i]] <-fa.bech$weights[i, vc[i]]
}

# See the variance explained by the five factors.
fa.bech$Vaccounted

## Fit the model with the theoretical number of factors.
fa.bechc <- fa(Bechtoldt.1, nfactors = 6, n.obs = nrow(Bechtoldt.1), rotate = "varimax", scores = c("Thompson", "regression", "Bartlett"), fm = "ml")
fa.bechc
fa.bechc$weights
vc <- apply(abs(fa.bechc$weights), 1, which.max)

load.disjc <- matrix(0, nrow(Bechtoldt.1), 6)
for (i in 1:nrow(Bechtoldt.1)) {
  load.disjc[i, vc[i]] <-fa.bechc$weights[i, vc[i]]
}

fa.bechc$Vaccounted

orderedfact <- cbind(fa.bechc$weights[, 6], fa.bechc$weights[, 1:5])

library(MASS)
plot(orderedfact, xlim = c(-1, 1), ylim = c(-1, 1))
abline(h = 0, v = 0)
text(orderedfact, dimnames(Bechtoldt.1)[[1]])

orderedfact.disj <- cbind(load.disjc[, 6], load.disjc[, 1:5])
rownames(orderedfact.disj) <- dimnames(Bechtoldt.1)[[1]]

# FOR HOME: See also the "factanal" function in "stats" package (base one).

##################################################################################### 
# 2. Clustering Methods: K-means                                                    #
#####################################################################################
## Assume to generate a bivariate data set as follows.
mu1 <- c(0, 0)
Sigma1 <- diag(2)
mu2 <-  c(7, 5)
Sigma2 <- diag(2) + 0.75 *(matrix(1, 2, 2) - diag(2))
X1 <- mvrnorm(50, mu1, Sigma1)
X2 <- mvrnorm(70, mu2, Sigma2)
XX <- data.frame(rbind(X1, X2))
cluster <- factor(c(rep(1, 50), rep(2, 70)))

ggplot(XX, aes(x = X1, y = X2)) + 
  geom_point(aes(colour = cluster), alpha = 0.7) +
  scale_colour_manual(values = c("purple", "green"))+
  stat_ellipse(data = XX, aes(color = cluster), linetype = 4) +
  ggtitle("Cluster generation")

## Introduce contamination
X3 <- mvrnorm(10, c(5, 0), 30*diag(2))
XX.out <- data.frame(rbind(X1, X2, X3))
cluster.out <- factor(c(rep(1, 50), rep(2, 70), rep(3, 10)))

ggplot(XX.out, aes(x = X1, y = X2)) + 
  geom_point(aes(colour = cluster.out), alpha = 0.7) +
  scale_colour_manual(values = c("purple", "green", "black")) +
  stat_ellipse(data = XX, aes(color = cluster), linetype = 4) +
  ggtitle("Cluster generation")

## What happens if we apply k-means clustering?
? kmeans
km <- kmeans(XX.out, centers = 2, nstart = 30)
cluster.km <- factor(km$cluster)
cluster.outkm <- as.factor(c(cluster.km[1:120], as.numeric(cluster.km[121:130]) + 2))

ggplot(XX.out, aes(x = X1, y = X2)) + 
  geom_point(aes(colour = cluster.outkm, shape = cluster.outkm), alpha = 0.7) +
  scale_colour_manual(values = c("purple", "green", "purple", "green")) +
  scale_shape_manual(values = c(19, 19, 17, 17)) +
  stat_ellipse(data = XX.out, aes(color = cluster.km), linetype = 5) +
  stat_ellipse(data = XX, aes(color = cluster), linetype = 4) +
  ggtitle("K-means")

## Compare the true cluster center with the estimated one.
rbind(mu1, mu2)
km$centers

