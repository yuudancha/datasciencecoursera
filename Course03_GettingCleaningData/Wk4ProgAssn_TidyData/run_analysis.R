# run_analysis.R
# Course/Assignment: Coursera > Johns Hopkins > Course: Getting & Cleaning Data > Week 4 Programming Assignment 
# Creator: Dan Charlson
# Date Created: 2023-01-03
# 
## Intro
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Assumptions
# 1. You have created a project working directory.
# 2. You have already downloaded the provided Zip archive and expanded it into the project working directory
# 3. You are running the run_analysis.R script in the project working directory; i.e., the unzipped data is in a directory at the same level as the script.

## Preliminary setup
# get any needed packages
install.packages("data.table")


## 1. Merges the training and the test sets to create one data set.
# subjects
subject_data_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_data_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_data_merged <- rbind(subject_data_train, subject_data_test)
rm(subject_data_train, subject_data_test)
# measurements from activities
measurement_data_train <- read.table("UCI HAR Dataset/train/X_train.txt")
measurement_data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
measurement_data_merged <- rbind(measurement_data_train, measurement_data_test)
rm(measurement_data_train, measurement_data_test)
# labels/activities
activity_data_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_data_test <- read.table("UCI HAR Dataset/test/y_test.txt")
activity_data_merged <- rbind(activity_data_train, activity_data_test)
rm(activity_data_train, activity_data_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features_all <- read.table("UCI HAR Dataset/features.txt")
features_required <- grep("(mean|std)\\(\\)", features_all[, 2])
measurement_data_merged <- measurement_data_merged[, features_required]
names(measurement_data_merged) <- features_all[features_required, 2]
names(measurement_data_merged) <- gsub("\\(|\\)", "", names(measurement_data_merged))

## 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
activity_labels[, 2] = gsub("_", "", as.character(activity_labels[, 2]))
activity_data_merged[,1] = activity_labels[activity_data_merged[,1], 2]
names(activity_data_merged) <- "activity"

## 4. Appropriately labels the data set with descriptive variable names. 
names(subject_data_merged) <- "subject"
tidied <- cbind(subject_data_merged, activity_data_merged, measurement_data_merged)
write.table(tidied, "merged_and_tidied_data.txt")


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# I really would have liked to use tidyr or dplyr but this was going to be fastest. "Next time!"
unique_subjects = unique(subject_data_merged)[,1]
num_subjects = length(unique(subject_data_merged)[,1])
num_activities = length(activity_labels[,1])
num_cols = dim(tidied)[2]
result = tidied[1:(num_subjects*num_activities), ]
row = 1
for (subject in 1:num_subjects) {
    for (activity in 1:num_activities) {
        result[row, 1] = unique_subjects[subject]
        result[row, 2] = activity_labels[activity, 2]
        temp_storage <- tidied[tidied$subject==subject & tidied$activity==activity_labels[activity, 2], ]
        result[row, 3:num_cols] <- colMeans(temp_storage[, 3:num_cols])
        row = row+1
    }
}
write.table(result, "final_second_tidy_data_set_with_averages.txt")

## Cleanup the environment
rm(list = ls())

## Testing
# Obviously, this script produces two output TXT files.
# On visual inspection, the first one looks correct; it's *tidy*
# I tried to verify the averages in the second file by opening the first file in Excel, but wasn't sure. Hmm...
