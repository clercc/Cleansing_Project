################
#       Load Libraries for Project
################

library(tidyr)
library(dplyr)
library(reshape2)


###############
#       Get file
###############

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fpath <- "C:/Users/Christian/github/datasciencecoursera/CleanseProject.zip"
download.file(fileURL, fpath)


###############
#       load data files
###############

train_data <- read.table(unz(fpath, "UCI HAR Dataset/train/X_train.txt"))
trainfeatures <- read.table(unz(fpath, "UCI HAR Dataset/train/y_train.txt"))
trainsubject <- read.table(unz(fpath, "UCI HAR Dataset/train/subject_train.txt"))

test_data <- read.table(unz(fpath, "UCI HAR Dataset/test/X_test.txt"))
testfeatures <- read.table(unz(fpath, "UCI HAR Dataset/test/y_test.txt"))
testsubject <- read.table(unz(fpath, "UCI HAR Dataset/test/subject_test.txt"))

activity_names <- read.table(unz(fpath, "UCI HAR Dataset/activity_labels.txt"))
column_features <- read.table(unz(fpath, "UCI HAR Dataset/features.txt"))


##############
#       Create mean and std extract vectors, clean feature labels, and create activity names
##############

activity_names[,2] <- as.character(activity_names[,2])
column_features[,2] <- as.character(column_features[,2])
subset_features <- grep(".*mean.*|.*std.*", column_features[,2])
subset_names <- column_features[subset_features,2]
subset_names = gsub('-mean', ' Mean', subset_names)
subset_names = gsub('-std', ' Std', subset_names)
subset_names = gsub('[-()]', '', subset_names)


##############
#       Add in Primary Key to join data & label datasets
##############

train_data <- train_data[subset_features]  %>% mutate(ID = seq(1,7352))
trainlabels <- trainlabels %>% mutate(ID = seq(1,7352))

test_data <- test_data[subset_features] %>% mutate(ID = seq(7353,10299))
testlabels <- testlabels %>% mutate(ID = seq(7353,10299))


##############
#       Merge datasets 
##############

train <- inner_join(trainlabels, train_data, by = c("ID" = "ID"))
train <- inner_join(train, activity_names, by = c("V1.x" = "V1"))

test <- inner_join(testlabels, test_data, by = c("ID" = "ID"))
test <- inner_join(test, activity_names, by = c("V1.x" = "V1"))

subject <- rbind(trainsubject, testsubject)

f_data <- merge(train, test, all = TRUE)
f_data <- cbind(subject,f_data[3:82])


##############
#       Re-arrange order
##############

f_data <- f_data[,c(1,81,2:80)]


##############
#       Updating Column Headers
##############

colnames(f_data) <- c("Subject", "Activity", subset_names)


##############
#       Set Activity and Subject variables to factors
##############

f_data$Activity <- factor(f_data$Activity, levels = activity_names[,1], labels = activity_names[,2])
f_data$Subject <- as.factor(f_data$Subject)


#############
#       Taking mean and standard deviation of all the measurements by group
#############

f_data <- melt(f_data, ID = c("Subject", "Activity"))
f_data_mean <- dcast(f_data, Subject + Activity ~ variable, mean)

write.table(f_data_mean, "C:/Users/Christian/github/Cleansing_Project/tidy.txt", row.names = FALSE, quote = FALSE)
