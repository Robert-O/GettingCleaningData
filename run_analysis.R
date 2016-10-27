## script must do the following:

##  1)  Merges the training and the test sets to create one data set.
##  2)  Extracts only the measurements on the mean and standard deviation for 
##      each measurement.
##  3)  Uses descriptive activity names to name the activities in the data set
##  4)  Appropriately labels the data set with descriptive variable names.
##  5)  From the data set in step 4, creates a second, independent tidy data 
##      set with the average of each variable for each activity & each subject.

## set working directory
setwd("~/Rworkspace/Course_3/Peer assignment")

### Load and Merge the test and training sets
## Step 1 - Load the training set 
training_set = read.table('./UCI HAR Dataset/train/X_train.txt')

## Step 2 - Load the test set
test_set = read.table('./UCI HAR Dataset/test/X_test.txt')

## Step 3 - Merge the test sets
training_plus_test_set = rbind(test_set, training_set)

### Load and Merge the test and training set labels
## Step 1 - Load the training set labels 
training_set_labels = read.table('./UCI HAR Dataset/train/Y_train.txt')

## Step 2 - Load the test set labels
test_set_labels = read.table('./UCI HAR Dataset/test/Y_test.txt')

## Step 3 - Merge the training and test set labels
merged_labels_set = rbind(test_set_labels, training_set_labels)

### Load and Merge the test and traing set subjects
## Step 1 - Load the training set subjects 
training_set_subjects = read.table('./UCI HAR Dataset/train/subject_train.txt')

## Step 2 - Load the test set labels
test_set_subjects = read.table('./UCI HAR Dataset/test/subject_test.txt')

## Step 3 - Merge the training and test set labels
merged_subjects_set = rbind(test_set_subjects, training_set_subjects)

### merge resuults, labels, and subjects
superSet = cbind(training_plus_test_set, merged_labels_set, merged_subjects_set)


## extract only the measurements on the mean and standard deviation
### load the names of the features
features = read.table('./UCI HAR Dataset/features.txt', stringsAsFactors = FALSE)

## look for mean and standard deviation column names
meanOrStdColumns <- grep('mean|std', features$V2)
meanOrStdColumnNames <- features[meanOrStdColumns, 'V2']

## extract only columns for mean or std deviation. Plus, can't lose the activity
## and subject columns, which are the last 2 columns. 
activitySuperSetColumn = ncol(superSet) - 1
subjectSuperSetColumn = ncol(superSet)
mergedSetMeanStd <- superSet[, c(meanOrStdColumns, activitySuperSetColumn, subjectSuperSetColumn)]

## load activity labels
activity_labels = read.table('./UCI HAR Dataset/activity_labels.txt', 
                             stringsAsFactors = FALSE)

## set activity label of merged data set to the description from the activty
## label table
count = 0
activityColumn  = ncol(mergedSetMeanStd) - 1
subjectColumn = ncol(mergedSetMeanStd)
for (activity_row_value in mergedSetMeanStd[, activityColumn]){
      count = count + 1
      cat('count: ', count, '\n', 'activity_row: ', activity_row, '\n' )
      activity_description = 
            activity_labels[activity_labels$V1 == activity_row_value, 'V2' ]
      mergedSetMeanStd[count, activityColumn] = activity_description
}

## label the data set with descriptive variable names
colnames(mergedSetMeanStd)[c(activityColumn, subjectColumn)]=
      c('activity', 'subjectId' )
colnames(mergedSetMeanStd)[1:activityColumn-1] = meanOrStdColumnNames


## create independent data set that has averages for each variable for each 
## activity and each subject
library(reshape2)
meltedMergedSet = melt(mergedSetMeanStd, id.vars = c('activity', 'subjectId'))
tidyMeans <- dcast(meltedMergedSet, activity + subjectId ~ variable, mean)

write.csv(tidyMeans, file = 'TidyData.csv')
