################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 10                                                   #
# Tuesday, December 12, 2023                                   #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Clustering Methods: K-means and trimmed K-means

cat("\014")
rm(list = ls())
library(mclust)
library(tclust)
library(MASS) 
library(ggplot2)
library(MixSim)
library(corrplot)

##################################################################################### 
# 1. Clustering Methods: K-means and trimmed K-means                                #
#####################################################################################
## Recall the simulation design we generate during the last lecture.
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

km.base <- kmeans(XX, centers = 2, nstart = 30)
adjustedRandIndex(cluster, km.base$cluster)
table(cluster, km.base$cluster)

# ATTENTION: Label switching problem! This is not really a problem.
# You have to pay attention to the label of the clusters: they could be reversed.
# Not really a problem, you have to pay attention to the cluster interpretation.

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

## Compare the true cluster centers with the estimated ones.
rbind(mu1, mu2)
km$centers

## Robust approach: Trimmed K-means 
# Real contamination
nrow(X3)/nrow(XX.out)*100 # The percentage of outliers in the data set is this.
tkm <- tkmeans(XX.out, 2, nstart = 30, alpha = 0.10) # Setting alpha = 0.10 (greater than nrow(X3)/nrow(XX.out)*100) is not a problem: trimming more is better than trimming less!
cluster.tkm <- factor(tkm$cluster)

# Which are the identified outliers and which are the points that are identified or not as outliers?
out.th <- c(cluster, rep(0, 10))
table(out.th, cluster.tkm) # All the true outliers are identified.

ggplot(XX.out, aes(x = X1, y = X2)) + 
  geom_point(aes(colour = cluster.tkm), alpha = 0.7) +
  scale_colour_manual(values=c("black", "purple", "green")) +
  stat_ellipse(data = XX.out[-which(as.numeric(cluster.tkm) - 1 == 0), ], aes(color = cluster.tkm[-which(as.numeric(cluster.tkm) - 1 == 0)]), linetype = 5) +
  stat_ellipse(data = XX, aes(color = factor(3 - as.numeric(cluster))), linetype = 4) +
  ggtitle("Trimmed K-means")

# See also...
? plot.tclust
plot(tkm, jitter = TRUE)

## Compare the true cluster centers with the estimated ones.
rbind(mu1, mu2)
t(tkm$centers) # ATTENTION: Label switching.
km$centers

# Let measure the accuracy of the cluster center estimation.
# Cluster 1
mean((km$centers[1, ] - mu1)^2)
mean((t(tkm$center)[2, ] - mu1)^2) # Label switching, thus we use t(tkm$center)[2, ].

# Cluster 2
mean((km$centers[1, ] - mu1)^2)
mean((t(tkm$center)[1, ] - mu2)^2) # Label switching, thus we use t(tkm$center)[1, ].

