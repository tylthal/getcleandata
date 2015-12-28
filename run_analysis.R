
# Load feature names
featureNames <- read.delim("data/features.txt", header = FALSE,sep = " ",colClasses = c("NULL",NA))
labels <- matrix(unlist(featureNames), ncol = 1)
labels <- gsub("mean", "Mean",labels) # capitalize M
labels <- gsub("std", "Std", labels) # capitalize S
labels <- gsub("-", "", labels) # remove "-" in column names 
labels <- gsub("\\(\\)", "", labels) # remove "()"
valid_labels <- make.names(labels, unique = TRUE, allow_ = TRUE)


# get mean and std-deviation measurments label indices
features <- read.table("data/features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", featureNames[, 1])
length(meanStdIndices) #66

# load training set
training_subject <- read.table('data/train/subject_train.txt', header = FALSE)
train_subjLen <- length(table(training_subject))
colnames(training_subject) <- c('subject')
training_set <- read.table('data/train/X_train.txt', header = FALSE)
colnames(training_set) <- valid_labels
training_set <- training_set[,meanStdIndices]
training_labels <- read.table('data/train/Y_train.txt', header = FALSE)
colnames(training_labels) <- c('activity')
training <- cbind(cbind(training_subject, training_labels), training_set)

# load test set
test_subject <- read.table('data/test/subject_test.txt', header = FALSE)
test_subjLen <- length(table(test_subject))
colnames(test_subject) <- c('subject')
test_set <- read.table('data/test/X_test.txt', header = FALSE)
colnames(test_set) <- valid_labels
test_set <- test_set[,meanStdIndices]
test_labels <- read.table('data/test/Y_test.txt', header = FALSE)
colnames(test_labels) <- c('activity')
test <- cbind(cbind(test_subject, test_labels), test_set)

# merge test and training
complete <- rbind(training, test)

dim(complete) # 10299x68


head(complete)

# add the descriptive activity labels
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

# update the activity labels
complete[complete$activity == 1,]$activity = 'WALKING'
complete[complete$activity == 2,]$activity = 'WALKING_UPSTAIRS'
complete[complete$activity == 3,]$activity = 'WALKING_DOWNSTAIRS'
complete[complete$activity == 4,]$activity = 'SITTING'
complete[complete$activity == 5,]$activity = 'STANDING'
complete[complete$activity == 6,]$activity = 'LAYING'

names(complete)
complete

activity <- read.table("data/activity_labels.txt")
as.character(activity[2,2])


#create a summary tidy table 
subjectLen <- train_subjLen + test_subjLen # 30
activityLen <- 6
columnLen <- dim(complete)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(complete)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(complete$subject))[i]
    result[row, 2] <- as.character(activity[j, 2])
    bool1 <- i == complete$subject
    bool2 <- activity[j, 2] == complete$activity
    result[row, 3:columnLen] <- colMeans(complete[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)

write.table(result, "tidy.txt", row.names = FALSE)
