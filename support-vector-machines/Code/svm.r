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
#################################
# review -> rating
#
# SVM Prediction Model
#################################
data.amazon.video.game.review.w.sparse <- data.amazon.video.game.review %>% mutate(r_rating_new = if_else(r_rating > 3, 1, 0))

product.matrix <- create_matrix(data.amazon.video.game.review.w.sparse$r_text, language = "English", 
                                removeNumbers = TRUE, 
                                removePunctuation = TRUE, 
                                removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                      data.amazon.video.game.review.w.sparse$r_rating_new, 
                                      trainSize = 1:500, testSize = 7601:8000, 
                                      virgin = FALSE)

table(data.amazon.video.game.review.w.sparse$r_rating)

product.model <- train_model(product.container, algorithm = "SVM")

product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.amazon.video.game.review.w.sparse$r_rating_new[7601:8000], product.result$SVM_LABEL))
colnames(x) <- c("actual.count", "predicted.count")
x <- x %>% mutate(predicted.count = predicted.count - 1)
round(prop.table(table(x$actual.count == x$predicted.count)), 3)

########################################################################################################################
########################################################################################################################
########################################################################################################################
#################################
# title -> also_bought_count
#
# SVM Prediction Model
#################################
#
# Clean the text
#
data.amazon.w.sparse <- data.amazon %>% mutate(sparse_count_new = if_else(also_bought_count > 1, 1, 0))
product.matrix <- create_matrix(data.amazon.w.sparse$title, language = "English", 
                                       removeNumbers = TRUE, 
                                       removePunctuation = TRUE, 
                                       removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                      data.amazon.w.sparse$sparse_count_new, 
                                       trainSize = 1:1000, testSize = 1051:1074, 
                                       virgin = FALSE)

product.model <- train_model(product.container, algorithm = "SVM")
product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.amazon.w.sparse$sparse_count_new[1051:1074], product.result$SVM_LABEL))

colnames(x) <- c("actual.count", "predicted.count")

x <- x %>% mutate(predicted.count = predicted.count - 1)

round(prop.table(table(x$actual.count == x$predicted.count)), 3)
########################################################################################################################
########################################################################################################################
########################################################################################################################
data.amazon <- read.csv(amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
data.clean <- data.amazon %>% mutate(count_new = if_else(also_viewed_count > 1, 1, 0))
product.matrix <- create_matrix(data.clean$title, language = "English", 
                                       removeNumbers = TRUE, 
                                       removePunctuation = TRUE, 
                                       removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                       data.clean$count_new, 
                                       trainSize = 1:1000, testSize = 2102:2148, 
                                       virgin = FALSE)

product.model <- train_model(product.container, algorithm = "SVM")

product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.clean$count_new[2102:2148], product.result$SVM_LABEL))

colnames(x) <- c("actual.count", "predicted.count")

x <- x %>% mutate(predicted.count = predicted.count - 1)

round(prop.table(table(x$actual.count == x$predicted.count)), 3)
########################################################################################################################
########################################################################################################################
########################################################################################################################
data.amazon <- read.csv(amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
data.clean <- data.amazon %>% mutate(count_new = if_else(bought_together_count > 1, 1, 0))
product.matrix <- create_matrix(data.clean$title, language = "English", 
                                       removeNumbers = TRUE, 
                                       removePunctuation = TRUE, 
                                       removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                       data.clean$count_new, 
                                       trainSize = 1:1000, testSize = 2102:2148, 
                                       virgin = FALSE)

product.model <- train_model(product.container, algorithm = "SVM")

product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.clean$count_new[2102:2148], product.result$SVM_LABEL))

colnames(x) <- c("actual.count", "predicted.count")

x <- x %>% mutate(predicted.count = predicted.count - 1)

round(prop.table(table(x$actual.count == x$predicted.count)), 3)
########################################################################################################################
########################################################################################################################
########################################################################################################################
data.amazon <- read.csv(amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
data.clean <- data.amazon %>% mutate(count_new = if_else(buy_after_viewing_count > 1, 1, 0))
product.matrix <- create_matrix(data.clean$title, language = "English", 
                                       removeNumbers = TRUE, 
                                       removePunctuation = TRUE, 
                                       removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                       data.clean$count_new, 
                                       trainSize = 1:1000, testSize = 2102:2148, 
                                       virgin = FALSE)

product.model <- train_model(product.container, algorithm = "SVM")

product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.clean$count_new[2102:2148], product.result$SVM_LABEL))

colnames(x) <- c("actual.count", "predicted.count")

x <- x %>% mutate(predicted.count = predicted.count - 1)

round(prop.table(table(x$actual.count == x$predicted.count)), 3)

########################################################################################################################
########################################################################################################################
########################################################################################################################
# description -> count
########################################################################################################################
########################################################################################################################
########################################################################################################################