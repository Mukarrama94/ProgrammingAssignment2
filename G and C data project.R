# Getting and Cleaning Data Project (John Hopkins Coursera)
# Author: MUKARRAMA SIDDIQUA

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy 
   #data set with the average of each variable for each activity and each subject.

#Library used
library(data.table)
library(dplyr)

# directory for dataset
setwd("UCI HAR Dataset")

# Train Data read
X_train   <- read.table("./train/X_train.txt")
Y_train   <- read.table("./train/Y_train.txt") 
Sub_train <- read.table("./train/Subject_train.txt")

# Test data read 
X_test   <- read.table("./test/X_test.txt")
Y_test   <- read.table("./test/Y_test.txt") 
Sub_test <- read.table("./test/Subject_test.txt")

# feature discription  
features <- read.table("./features.txt") 

# read activity labels 
activity__labels <- read.table("./activity__labels.txt") 

# merge of training and test sets
X_total   <- rbind(X_train, X_test)
Y_total   <- rbind(Y_train, Y_test) 
Sub_total <- rbind(Sub_train, Sub_test) 

# keeping measurements for standard deviation and mean 
Selected_Features <- Variable_names[grep(".*mean\\(\\)|std\\(\\)", 
                                         features[,2], ignore.case = FALSE),]
X_total      <- X_total[,Selected_Features[,1]]

# columns
colnames(X_total)   <- Selected_Features[,2]
colnames(Y_total)   <- "activity"
colnames(Sub_total) <- "subject"

# Final dataset merging
total <- combind(Sub_total, Y_total, X_total)

# turn activities & subjects into factors 
total$activity <- factor(total$activity, 
                         levels = activity__labels[,1]],

                         labels = activity__labels[,2]]) 

total$subject  <- as.factor(total$subject) 

# create a summary independent tidy dataset from final dataset 
# with the average of each variable for each activity and each subject. 
total_mean <- total %>%group_by(activity, subject) %>% summarize_for_all(funs(mean)) 

# summary dataset export
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE) 

