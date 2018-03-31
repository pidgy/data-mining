Student.Number <- "100925862"
ASorLAB <- "LAB"
Assignment.Number <-"2"
Student.info <- paste(Student.Number, ASorLAB, Assignment.Number, sep="-")
# Set The Path up until the folder 100925862-LAB-2
drive=""
path.upto <- paste("set","me", sep="/" )
code.dir <- paste(drive, path.upto, Student.info, "Code", sep="/")
data.dir <- paste(drive, path.upto, Student.info, "Data", sep="/")
work.dir <- paste(drive, path.upto, Student.info, "Work", sep="/")
setwd(work.dir)
# File constants 
wine.data       <- paste(data.dir, "wine.data",          sep="/")
wine.cols      <- paste(data.dir, "wine.cols",         sep="/")

library(stats)
library(caret)
library(fifer)
################################
# Load data
d.wine <- read.table(wine.data, sep=",")
d.wine.cols <- scan (wine.cols, what ="a", sep ="\n")
colnames(d.wine) <- d.wine.cols
##########################################################################
# CONSTANT VARIABLES
##########################################################################
CULTIVAR         <- "Cultivar"
ALCOHOL          <- "Alcohol"
MALIC_ACID       <- "Malic acid"
ASH              <- "Ash"
ALCA_ASH         <- "Alcalinity of ash"
TOTAL_PHENOLS    <- "Total phenols"
NF_PHENOLS       <- "Nonflavanoid phenols"
PROANT           <- "Proanthocyanins"
COLOR_INTENSITY  <- "Color intensity"
HUE              <- "Hue"
DILUTED          <- "OD280/OD315 of diluted wines"
PROLINE          <- "Proline"
##########################################################################
# GETTERS FOR WINE PROPERTIES
##########################################################################
get.cultivar        <- function(r,d=d.wine)     {d[r, CULTIVAR]}
get.alcohol         <- function(r,d=d.wine)     {d[r, ALCOHOL]}
get.malic_acid      <- function(r,d=d.wine)     {d[r, MALIC_ACID]}
get.ash             <- function(r,d=d.wine)     {d[r, ASH]}
get.alca_ash        <- function(r,d=d.wine)     {d[r, ALCA_ASH]}
get.total_phenols   <- function(r,d=d.wine)     {d[r, TOTAL_PHENOLS]}
get.nf_phenols      <- function(r,d=d.wine)     {d[r, NF_PHENOLS]}
get.proant          <- function(r,d=d.wine)     {d[r, PROANT]}
get.color_intensity <- function(r,d=d.wine)     {d[r, COLOR_INTENSITY]}
get.hue             <- function(r,d=d.wine)     {d[r, HUE]}
get.diluted         <- function(r,d=d.wine)     {d[r, DILUTED]}
get.proline         <- function(r,d=d.wine)     {d[r, PROLINE]}
get.row             <- function(r,d=d.wine)     {d[r, ]}
get.which           <- function(r,w,d=d.wine)   {d[r, w]}
##########################################################################
# - Question 1 -
# Divide the dataset into 2/3 for training and 1/3 for testing, 
# making sure that the distribution of the three cultivars is 
# roughly the same in the training set and in the testing set.
# - Solution 1 -
# Stratified random sampling will achieve this goal
##########################################################################
strata <- stratified(d.wine, CULTIVAR, 0.66)
d.wine.trainset <- d.wine[as.integer(rownames(strata)),]
d.wine.testset <- d.wine[-as.integer(rownames(strata)),]
##########################################################################
# - Question 2 -
# Use k-means clustering with initial centroids chosen randomly, 
# then loop to try clustering with different random seeds and choose 
# the seed that gives the most accurate result.
##########################################################################
Davies.Bouldin <- function(A, SS, m) {
  # A  - the centres of the clusters
  # SS - the within sum of squares
  # m  - the sizes of the clusters
  N <- nrow(A)   # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}
##
# plotClusters <- function(km, d)
# Clustering/VQ/Image Compression 
# 250 © Mills2018
# Data Mining 2018
##
plotClusters <- function(d, s) {
  set.seed(s)
  
  base.colors <- c("#FF0000","#00FF00","#0000FF","#FF00FF","#00FFFF","#FFFF00",
                   "#800000","#008000","#000080","#FF0080","#408080","#804000",
                   "#004080", "#FF8000")
  colors <- rep(base.colors, ceiling(15/length(base.colors)))
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  errs <- rep(0, 8)
  DBI <- rep(0, 8)
  for (i in 2:8) {
    KM <- kmeans(d, i, 6)
    plot(d, col=colors[KM$cluster], pch=KM$cluster, main=paste(i," clusters"))
    errs[i-1] <- sum(KM$withinss)
    DBI[i-1] <- Davies.Bouldin(KM$centers, KM$withinss, KM$size)
  }
#  par(oldpar)
  list('errs' = errs, 'dbi' = DBI)
}
# Test Set
errs.dbi <- plotClusters(d.wine.testset, i)
errs <- errs.dbi[[1]]
dbi <- errs.dbi[[2]]
# Training Set
errs.dbi <- plotClusters(d.wine.trainset, i)
errs <- errs.dbi[[1]]
dbi <- errs.dbi[[2]]

par(mar=c(2,1,2,1))
plot(1:8, errs, type="l", main=paste("SS"))
plot(1:8, dbi, type="l", main="Davies-Bouldin")

##########################################################################
# - Question 3 -
# Do PCA on this data and use the first two or three PCs to do clustering 
# on the wine data. Compare your results with those found by using the 
# actual data instead of the principal components for clustering.
##########################################################################
d.wine.pca <- prcomp(d.wine)
d.wine.pca.first3 <- d.wine.pca$x[,1:3]
plotClusters(d.wine.pca.first3, 3)
plotClusters(d.wine, 4)
