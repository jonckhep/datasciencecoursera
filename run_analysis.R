# Step 1 - Merges the training and the test sets to create one data set.
trainingData <- read.table("./data/train/X_train.txt")
#dim(trainingData)
#head(trainingData)
trainingLabel <- read.table("./data/train/y_train.txt")
#table(trainingLabel)
trainingSubject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
#dim(testData)
testLabel <- read.table("./data/test/y_test.txt") 
#table(testLabel) 
testSubject <- read.table("./data/test/subject_test.txt")
joinData <- rbind(trainingData, testData)
#dim(joinData)
joinLabel <- rbind(trainingLabel, testLabel)
#dim(joinLabel)
joinSubject <- rbind(trainingSubject, testSubject)
#dim(joinSubject)

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./data/features.txt")
#dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
#length(meanStdIndices)
joinData <- joinData[, meanStdIndices]
#dim(joinData)
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

# Step 3 - Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step 4 - Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
#dim(cleanedData)
write.table(cleanedData, "merged_data.txt")

# Step 5 - Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subjectLength <- length(table(joinSubject))
activityLength <- dim(activity)[1]
columnLength <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLength*activityLength, ncol=columnLength) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLength) {
    for(j in 1:activityLength) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLength] <- colMeans(cleanedData[bool1&bool2, 3:columnLength])
        row <- row + 1
    }
}
head(result)
write.table(result, "average_data.txt")