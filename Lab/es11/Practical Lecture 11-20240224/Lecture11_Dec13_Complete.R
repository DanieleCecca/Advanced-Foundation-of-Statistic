################################################################ 
# ADVANCED FOUNDATIONS OF STATISTICS FOR AI                    # 
# Practical lectures                                           #
# Lecture 11                                                   #
# Wednesday, December 13, 2023                                 #
# Giorgia Zaccaria <giorgia.zaccaria@unimib.it>                #
################################################################

### Lecture Overview
##  1. Model-based clustering: Gaussian Mixture Models
##  2. Model-based clustering: comparison between non-robust and robust approaches

cat("\014")
rm(list = ls())
library(mclust)
library(tclust)
library(MASS) 
library(ggplot2)
library(MixSim)
library(corrplot)

#######################################################################################
# 1. Model-based clustering: Gaussian Mixture Models                                  #
#######################################################################################
? iris
str(iris)

table(iris$Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = Species), alpha = 0.7) +
  scale_colour_manual(values=c("purple", "green", "red")) +
  ggtitle("Iris data set")


# a) Fit a GMM with G = 3 and see the difference with a GMM where G is not fixed a 
# priori.
gmm1 <- Mclust(iris[, -5], G = 3)

names(gmm1)

## Which model is the best and what "best" means?
gmm1$modelName
? mclustModelNames
# See here https://journal.r-project.org/archive/2016/RJ-2016-021/RJ-2016-021.pdf.
plot(gmm1)

gmm1$parameters$pro

## Does the model recover the clustering structure "defined" by the species?
table(Theoretical = iris$Species, EstimatedGMM = gmm1$classification)
adjustedRandIndex(iris$Species, gmm1$classification)

GMM.cluster <- factor(gmm1$classification)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = GMM.cluster), alpha = 0.7) +
  scale_colour_manual(values=c("purple", "green", "red")) +
  ggtitle("GMM on Iris data set with G = 3")

# See what happens if G is not fixed a priori?
gmm2 <- Mclust(iris[, -5])

gmm2$G
gmm2$BIC
gmm2$modelName
gmm2$bic

plot(gmm2) # 1 and 2

table(Theoretical = iris$Species, EstimatedGMM2 = gmm2$classification)

GMM.cluster2 <- factor(gmm2$classification)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = GMM.cluster2), alpha = 0.7) +
  scale_colour_manual(values=c("purple", "red")) +
  ggtitle("GMM on Iris data set with G = 2")

# b) What happens if we fit k-means on iris?
# G = 3
km.iris <- kmeans(iris[, -5], 3,nstart = 30)
table(Theoretical = iris$Species, EstimatedKMEANS = km.iris$cluster)
adjustedRandIndex(iris$Species, km.iris$cluster)
adjustedRandIndex(iris$Species, gmm1$classification)

KMEANS.cluster <- factor(km.iris$cluster)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = KMEANS.cluster), alpha = 0.7) +
  scale_colour_manual(values=c("purple", "green", "red")) +
  ggtitle("K-means on Iris data set with G = 3")

# G = 2
km.iris2 <- kmeans(iris[, -5], 2, nstart = 30)
table(Theoretical = iris$Species, EstimatedKMEANS2 = km.iris2$cluster)
table(EstimatedGMM = gmm2$classification, EstimatedKMEANS2 = km.iris2$cluster)
adjustedRandIndex(gmm2$classification, km.iris2$cluster)

KMEANS.cluster2 <- factor(km.iris2$cluster)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = KMEANS.cluster2), alpha = 0.7) +
  scale_colour_manual(values=c("purple", "red")) +
  ggtitle("K-means on Iris data set with G = 2")

## ATTENTION! 
## K-means is NOT SCALE INVARIANT!!!!!!!
var(iris[, -5])

## See what happens by standardizing the data set.
km.iriss <- kmeans(scale(iris[, -5]), 3, nstart = 30)
adjustedRandIndex(iris$Species, km.iriss$cluster)
adjustedRandIndex(iris$Species, gmm1$classification)

km.iris2s <- kmeans(scale(iris[, -5]), 2, nstart = 30)
table(Theoretical = iris$Species, EstimatedKMEANS2 = km.iris2s$cluster)
table(EstimatedGMM = gmm2$classification, EstimatedKMEANS2 = km.iris2s$cluster)

#######################################################################################
# 2. Model-based clustering: comparison between non-robust and robust approaches      #
#######################################################################################
## Analyze the data set "swissbank" in 'tclust' package by comparing GMM with
## trimmed GMM.
? tclust:: swissbank
data("swissbank", package = "tclust")
str(swissbank)

label <- c(rep(1, 100), rep(2, 100))

par(mfrow = c(1,1))
pairs(swissbank, col = c("purple", "cornflowerblue")[label],
      pch = c(8, 18)[label]) # ? pch

## a) GMM by choosing G
swiss.gmm <- Mclust(swissbank)
swiss.gmm$G # Chosen G...
table(Theoretical = label, GMM = swiss.gmm$classification)

pairs(swissbank, col = c("red", "purple", "cornflowerblue")[swiss.gmm$classification],
      pch = c(8, 18, 1)[swiss.gmm$classification]) # ? pch

# b) Robust model-based clustering with tclust choosing G and alpha
? ctlcurves
plot(ctlcurves(swissbank, k = 1:4, alpha = seq (0, 0.3, by = 0.025)))

? tclust
swiss.tclus <- tclust (swissbank, k = 2, alpha = 0.1, restr.fact = 50)

names(swiss.tclus)

table(Theoretical = label[swiss.tclus$cluster != 0], GMM = swiss.tclus$cluster[swiss.tclus$cluster != 0])

plot(swiss.tclus, jitter = TRUE)

swiss.tclus$mah
which(is.na(swiss.tclus$mah))

? DiscrFact
plot(DiscrFact(swiss.tclus, threshold = 0.0001))

# Observations with large Discriminant Factor (DF) values (i.e., values close to zero,
# since DF <= 0) indicate doubtful assignments or trimming decisions.

# The choice threshold = 0.0001 means that a decision on a particular 
# observation is considered as doubtful, if the quality of the second best 
# possible decision is larger than one thousandth of the quality of the
# actually made decision.                                                                                 ; θ, b πb) or

## HOMEWORK 
# 1. Upload the data set "ckd" in the package teigen.
# 2. Fit a GMM on this data set by letting the model to select the number of clusters.
# 3. Comment the results in terms of the selected number of clusters.
# 4. Comment the results in terms of the clustering structure.
# 5. Compare the results by fitting a GGM with a fixed (choose it properly) number of clusters.
# 6. Compare the results of GMM with those obtained via a robust methodology.


