################
#       Load Libraries for Project
################

library(tidyr)
library(dplyr)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
fpath <- "~/github/datasciencecoursera/CleanseProject.zip"
download.file(fileURL, fpath)

###############
#       load data files
###############
x_train <- read.table(unz(fpath, "UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(unz(fpath, "UCI HAR Dataset/train/y_train.txt"))

x_test <- read.table(unz(fpath, "UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(unz(fpath, "UCI HAR Dataset/test/y_test.txt"))

row_activity <- read.table(unz(fpath, "UCI HAR Dataset/activity_labels.txt"))
col_features <- read.table(unz(fpath, "UCI HAR Dataset/features.txt"))

##############
#       Add in Primary Key to join x & y datasets
##############
x_train_n <- x_train %>% mutate(ID = seq(1,7352))
y_train_n <- y_train %>% mutate(ID = seq(1,7352))
x_test_n <- x_test %>% mutate(ID = seq(7353,10299))
y_test_n <- y_test %>% mutate(ID = seq(7353,10299))

##############
#       Merge train, test, and activities data 
##############
train <- inner_join(y_train_n, x_train_n, by = c("ID" = "ID"))
train <- inner_join(train, row_activity, by = c("V1.x" = "V1"))

test <- inner_join(y_test_n, x_test_n, by = c("ID" = "ID"))
test <- inner_join(test, row_activity, by = c("V1.x" = "V1"))

f_data <- merge(train, test, all = TRUE)

##############
#       Updating Column Headers
##############
col_features <- as.list(col_features[,2])
colnames(f_data)[3:563] <- col_features

##############
#       Exploring the data
##############
names(f_data)
str(f_data)

        #       Set V1.x to factors
        f_data$V1.x <- as.factor(f_data$V1.x)

summary(f_data)
head(f_data)
tail(f_data)

############
#       Extract measurements on the mean and standard deviation
############
col_avg <- apply(f_data[,c(3:563)], 2, mean)
col_sd <- apply(f_data[,c(3:563)], 2, sd)









#############
#       Taking mean and standard deviation of all the measurements by group
group_avg <- apply(f_data[,c(3:563)], 2, function(x) tapply(x,f_data$V2.y,mean))
group_sd <- apply(f_data[,c(3:563)], 2, function(x) tapply(x,f_data$V2.y,sd))
