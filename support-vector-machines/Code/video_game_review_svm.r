#######################################################
# File and Directory setup 
#######################################################
dir.root    <- paste("C:","Users", "trash", "dev", "data-mining", "support-vector-machines", sep="/")
dir.code    <- paste(dir.root, "Code", sep="/")
dir.data    <- paste(dir.root, "Data", sep="/")
file.amazon.counts.csv <- paste("bought_with_count_categories_asin_metadata_small","csv",sep=".")
file.amazon.counts.data <- paste(dir.data, file.amazon.counts.csv, sep="/")
file.amazon.video.game.review.csv <- paste("video_games","csv",sep=".")
file.amazon.video.game.review.data <- paste(dir.data, file.amazon.video.game.review.csv, sep="/")
################################
# Load data
################################
data.amazon <- read.csv(file.amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
data.amazon.video.game.review <- read.csv(file.amazon.video.game.review.data, header = TRUE, fill = TRUE, sep=",")
#################################
library(e1071)
library(RTextTools)
library(tm)
library(dplyr)

library(fifer)
strata <- stratified(data.amazon.video.game.review, "r_rating", size=400)

data.amazon.video.game.review.strata <- data.amazon.video.game.review[as.integer(rownames(strata)),]
data.amazon.video.game.review.testset <- data.amazon.video.game.review[-as.integer(rownames(strata)),]

##############
# 5 Clusters #
##############
data.amazon.strata.5.cluster <- data.amazon.video.game.review %>% mutate(r_rating_new = r_rating)
##############
# 4 Clusters #
##############
data.amazon.strata.4.cluster.1.2 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating > 2, r_rating, 1))
data.amazon.strata.4.cluster.2.3 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating %in% c(2, 3), 2, r_rating))
data.amazon.strata.4.cluster.3.4 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating %in% c(3, 4), 4, r_rating))
data.amazon.strata.4.cluster.4.5 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating %in% c(4, 5), 5, r_rating))
##############
# 3 Clusters #
##############
data.amazon.strata.3.cluster.1.2.3<- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating %in% c(1,2,3), 1, r_rating))
data.amazon.strata.3.cluster.3.4.5<- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating %in% c(3,4,5), 5, r_rating))
##############
# 2 Clusters #
##############
data.amazon.strata.2.cluster.less.3 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating < 3, 1, 0))
data.amazon.strata.2.cluster.greater.3 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating > 3, 1, 0))
data.amazon.strata.2.cluster.not.5 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating != 5, 1, 0))
data.amazon.strata.2.cluster.not.1 <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating != 1, 1, 0))



svm.predict.rating.from.review <- function(data) {
  product.matrix <- create_matrix(data$r_text, language = "English", 
                                  removeNumbers = TRUE, 
                                  removePunctuation = TRUE, 
                                  removeStopwords = FALSE, stemWords = FALSE)
  
  product.matrix <- removeSparseTerms(product.matrix, 0.99)
  
  product.container <- create_container(product.matrix,
                                        data$r_rating_new, 
                                        trainSize = 1:750, testSize = 751:1000, 
                                        virgin = FALSE)
  
  product.model <- train_model(product.container, algorithm = "SVM")
  
  product.result <- classify_model(product.container, product.model)
  
  x <- as.data.frame(cbind(data$r_rating_new[751:1000], product.result$SVM_LABEL))
  colnames(x) <- c("actual.count", "predicted.count")
  x <- x %>% mutate(predicted.count = predicted.count - 1)
  print(round(prop.table(table(x$actual.count == x$predicted.count)), 3))
}
#################################
# review -> rating [5 categories]
#
# SVM Prediction Model
svm.predict.rating.from.review(data.amazon.strata.5.cluster)  
#################################
# review -> rating [4 categories]
#
# SVM Prediction Model
svm.predict.rating.from.review(data.amazon.strata.4.cluster.1.2)  
svm.predict.rating.from.review(data.amazon.strata.4.cluster.2.3)  
svm.predict.rating.from.review(data.amazon.strata.4.cluster.3.4)  
svm.predict.rating.from.review(data.amazon.strata.4.cluster.4.5)
#################################
# review -> rating [3 categories]
#
# SVM Prediction Model
svm.predict.rating.from.review(data.amazon.strata.3.cluster.1.2.3)  
svm.predict.rating.from.review(data.amazon.strata.3.cluster.3.4.5)  
#################################
# review -> rating [2 categories]
#
# SVM Prediction Model
svm.predict.rating.from.review(data.amazon.strata.2.cluster.less.3)  
svm.predict.rating.from.review(data.amazon.strata.2.cluster.greater.3)  
svm.predict.rating.from.review(data.amazon.strata.2.cluster.not.5)  
svm.predict.rating.from.review(data.amazon.strata.2.cluster.not.1)
########################################################################################################################
########################################################################################################################
########################################################################################################################
svm.predict.rating.from.summary <- function(data) {
  product.matrix <- create_matrix(data$r_summary, language = "English", 
                                  removeNumbers = TRUE, 
                                  removePunctuation = TRUE, 
                                  removeStopwords = FALSE, stemWords = FALSE)
  
  product.matrix <- removeSparseTerms(product.matrix, 0.99)
  
  product.container <- create_container(product.matrix,
                                        data$r_rating_new, 
                                        trainSize = 1000:1750, testSize = 1751:2000, 
                                        virgin = FALSE)
  
  product.model <- train_model(product.container, algorithm = "SVM")
  
  product.result <- classify_model(product.container, product.model)
  
  x <- as.data.frame(cbind(data$r_rating_new[751:1000], product.result$SVM_LABEL))
  colnames(x) <- c("actual.count", "predicted.count")
  x <- x %>% mutate(predicted.count = predicted.count - 1)
  print(round(prop.table(table(x$actual.count == x$predicted.count)), 3))
}
#################################
# review -> summary [5 categories]
#
# SVM Prediction Model
svm.predict.rating.from.summary(data.amazon.strata.5.cluster)  
#################################
# review -> summary [4 categories]
#
# SVM Prediction Model
svm.predict.rating.from.summary(data.amazon.strata.4.cluster.1.2)  
svm.predict.rating.from.summary(data.amazon.strata.4.cluster.2.3)  
svm.predict.rating.from.summary(data.amazon.strata.4.cluster.3.4)  
svm.predict.rating.from.summary(data.amazon.strata.4.cluster.4.5)
#################################
# review -> summary [3 categories]
#
# SVM Prediction Model
svm.predict.rating.from.summary(data.amazon.strata.3.cluster.1.2.3)  
svm.predict.rating.from.summary(data.amazon.strata.3.cluster.3.4.5)  
#################################
# review -> summary [2 categories]
#
# SVM Prediction Model
svm.predict.rating.from.summary(data.amazon.strata.2.cluster.less.3)  
svm.predict.rating.from.summary(data.amazon.strata.2.cluster.greater.3)  
svm.predict.rating.from.summary(data.amazon.strata.2.cluster.not.5)  
svm.predict.rating.from.summary(data.amazon.strata.2.cluster.not.1)
################################
