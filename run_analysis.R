# Final project for Coursera course "Getting and Cleaning Data"
#
# The purpose of this project is to demonstrate your ability to 
# collect, work with, and clean a data set. The goal is to prepare 
# tidy data that can be used for later analysis. You will be graded 
# by your peers on a series of yes/no questions related to the project. 
#
# You will be required to submit: 
#       1) a tidy data set as described below, 
#       2) a link to a Github repository with your script for performing the analysis, and 
#       3) a code book that describes the variables, the data, and any transformations 
#          or work that you performed to clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.
#
# A full description is available at the site where the data was obtained:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# Citation:
# Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
# A Public Domain Dataset for Human Activity Recognition Using Smartphones. 
# 21th European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning, 
# ESANN 2013. Bruges, Belgium 24-26 April 2013.


## --------------------------------------------------------------
## set workspace
## --------------------------------------------------------------

ls()  ## lists variables in my workspace
rm(list=ls())  ## clears all variables in the workspace


## --------------------------------------------------------------
## load packages
## --------------------------------------------------------------

library(dplyr)


## --------------------------------------------------------------
## get data
## --------------------------------------------------------------

#my_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#temp_zip <- tempfile()
#download.file(my_url,temp_zip)
#unzip(temp_zip)
#unlink(temp_zip)

features <- read.table("features.txt", sep = "")  # List of all features. n=561
        features[,1] <- NULL
        features <- as.vector(unlist(features))

activity_raw <- read.table("activity_labels.txt", sep = "")  # Links the class labels with their activity name. n=6
        activity_labels <- activity_raw
        activity_labels[,1] <- NULL
        activity_labels <- as.vector(unlist(activity_labels))

train <- read.table("X_train.txt", sep = "")  # Training set. dim 7352 x 561
train_labels <- as.vector(unlist(read.table("Y_train.txt", sep = "")))  # Training labels. dim 7352 x 1
subject_train <- read.table("subject_train.txt", sep = "")  # Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
        subject_train <- as.vector(unlist(subject_train))  # length = 7352

test <- read.table("X_test.txt", sep = "")  # Test set. dim 2947 x 561
test_labels <- as.vector(unlist(read.table("Y_test.txt", sep = "")))  # Test labels. dim 2947 x 1
subject_test <- read.table("subject_test.txt", sep = "")  # Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
        subject_test <- as.vector(unlist(subject_test))  # length = 2947



## --------------------------------------------------------------
## 1. Merge the training and the test sets to create one dataset.
## --------------------------------------------------------------

full_tidy_df <- dplyr::full_join(train, test)
dim(full_tidy_df)  # dim 10299 x 561



## --------------------------------------------------------------
## 4. Appropriately label the data set with descriptive variable names.
## --------------------------------------------------------------

colnames(full_tidy_df) <- tolower(features)
colnames(full_tidy_df) <- tolower(features) %>%
        gsub("^t", "time\\.", .) %>%
        gsub("^f", "freq\\.", .) %>%
        gsub("-", ".", .) %>%
        gsub(",", ".", .) %>%
        gsub("\\(", ".", .) %>%
        gsub("\\)", ".", .) %>%
        gsub("\\.\\.\\.", ".", .) %>%
        gsub("\\.\\.", ".", .) %>%
        gsub("\\.$", "", .) %>%
        paste(index(features), .,  sep=".")

activity <- c(train_labels, test_labels)  # length 10299
full_tidy_df <- cbind(activity, full_tidy_df)  # adds activity code (i.e., subject number 1-6 as first column)

subject <- c(subject_train, subject_test)  # length 10299
full_tidy_df <- cbind(subject, full_tidy_df)  # adds subject code (i.e., subject number 1-30 as first column)



## --------------------------------------------------------------
## 3. Use descriptive activity names to name the activities in the data set
## --------------------------------------------------------------

activity_codebook <- t(data.frame(1:6))
colnames(activity_codebook) <- activity_labels  # n=6

full_tidy_df$activity <- colnames(activity_codebook)[match(
        full_tidy_df$activity, activity_codebook
        )]
dim(full_tidy_df)



## --------------------------------------------------------------
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## --------------------------------------------------------------

means_sd_df <- colnames(full_tidy_df) %>%
        grep("([Mm][Ee][Aa][Nn])|([Ss][Tt][Dd])", .) %>%
        c(1, 2, .) %>%
        full_tidy_df[, .]

dim(means_sd_df)  # 10299 x 88
names(means_sd_df)



## --------------------------------------------------------------
## 5. From the dataset in step 4, create a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
## --------------------------------------------------------------

means_by_subject_activity <- full_tidy_df %>%
        dplyr::group_by(subject, activity) %>%  # n=30 subjects, n=6 activities
        summarize_all(mean)
means_by_subject_activity

write.table(means_by_subject_activity, "means_by_subject_activity.csv", row.name=FALSE)
