## 1) Merges the training and the test sets to create one data set.

XTrainTempo <- read.table("train/X_train.txt")
XTestTempo <- read.table("test/X_test.txt")
X <-rbind(XTrainTempo,XTestTempo)

TrainSubjectTempo <- read.table("train/subject_train.txt") 
TestSubjectTempo <- read.table("test/subject_test.txt")
subject <-rbind(TrainSubjectTempo,TestSubjectTempo)

YTrainTempo <- read.table("train/y_train.txt") 
YTestTempo <- read.table("test/y_test.txt")
Y <-rbind(YTrainTempo,YTestTempo)

## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
meanStdPattern <- "-mean\\(\\)|-std\\(\\)" 
indexToAnalyze <- grep(meanStdPattern,features[, 2])
X <- X[, indexToAnalyze]

names(X) <- features[indexToAnalyze,2]
names(X) <- substr(names(X), 1, nchar(names(X))-2) 

## 3) Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = as.character(activities[, 2])
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

## 4) Appropriately labels the data set with descriptive activity names. 

names(subject) <- "subject"
finalTidyData <- cbind(subject, Y, X)
write.table(finalTidyData, "OutputMergedData.txt")

## 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
numActivities = length(activities[,1])
numCols = dim(finalTidyData)[2]
result = finalTidyData[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- finalTidyData[finalTidyData$subject==s & finalTidyData$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "OutputTidyData.txt")
