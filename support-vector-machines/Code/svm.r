#######################################################
# File and Directory setup 
#######################################################
dir.root    <- "set me"
dir.code    <- "Code"
dir.data    <- paste(dir.root, "Data", sep="/")
amazon.counts.csv <- paste("bought_with_count_categories_asin_metadata_small","csv",sep=".")
amazon.counts.data <- paste(dir.data, amazon.counts.csv, sep="/")
################################
# Load data
################################
data.amazon <- read.csv(amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
#################################
library(e1071)
library(RTextTools)
library(tm)
library(dplyr)
#################################
# title -> also_bought_count
#
# SVM Prediction Model
#################################
#
# Clean the text
#
testset.corpus <- Corpus(VectorSource(testset$title))
testset.dtm <- DocumentTermMatrix(testset.corpus, control=list(dictionary = Terms(trainset.dtm)))
#
# Convert to matrix, append sparse values as column y
#
testset.dtm <- as.matrix(dtm.test)
predict.result <- predict(svm.model.title.also.bought.count, newdata = testset.dtm)
#
# Revert back to a dataframe, add accuracy and precision
#
accuracy <- as.data.frame(cbind(prediction = predict.result, also_bought = testset$also_bought_count_new))
accuracy <- accuracy %>% mutate(prediction = as.integer(prediction) - 1)
accuracy$accuracy <- if_else(accuracy$prediction == accuracy$also_bought, 1, 0)
#
# Get the False/True Values
#
round(prop.table(table(categories.accuracy$accuracy)), 5)
########################################################################################################################
########################################################################################################################
########################################################################################################################
data.amazon <- read.csv(amazon.counts.data, header = TRUE, fill = TRUE, sep=",")
data.clean <- data.amazon %>% mutate(count_new = if_else(also_bought_count > 1, 1, 0))
product.matrix <- create_matrix(data.clean$title, language = "English", 
                                       removeNumbers = TRUE, 
                                       removePunctuation = TRUE, 
                                       removeStopwords = FALSE, stemWords = FALSE)

product.container <- create_container(product.matrix,
                                       data.clean$count_new, 
                                       trainSize = 1:1000, testSize = 1051:1074, 
                                       virgin = FALSE)

product.model <- train_model(product.container, algorithm = "SVM")
product.result <- classify_model(product.container, product.model)

x <- as.data.frame(cbind(data.clean$count_new[1051:1074], product.result$SVM_LABEL))

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