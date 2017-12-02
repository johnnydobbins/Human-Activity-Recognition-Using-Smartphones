library(dplyr)

# load train
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# load test
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# load description
variableNames <- read.table("./UCI HAR Dataset/features.txt")

# load labels
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# 1. merge to create one data set
xTotal <- rbind(xTrain, xTest)
yTotal <- rbind(yTrain, yTest)
subTotal <- rbind(subTrain, subTest)

# 2. extract measurements on the mean and sd
selectedVar <- variableNames[grep("mean\\(\\)|std\\(\\)",variableNames[,2]),]
xTotal <- xTotal[,selectedVar[,1]]

# 3. descriptive names to id the activities
colnames(yTotal) <- "activity"
yTotal$activitylabel <- factor(yTotal$activity, labels = as.character(activityLabels[,2]))
activitylabel <- yTotal[,-1]

# 4. labels with descriptive names
colnames(xTotal) <- variableNames[selectedVar[,1],2]

## tidy up the names
names(xTotal) <- gsub("\\(\\)", "", names(xTotal)) # remove "()"
names(xTotal) <- gsub("mean", "Mean", names(xTotal)) # capitalize M
names(xTotal) <- gsub("std", "Std", names(xTotal)) # capitalize S
names(xTotal) <- gsub("-", "", names(xTotal)) # remove "-" in column names 

# 5. create tidy data set with the avg of each variable per activity per subject.
colnames(subTotal) <- "subject"
total <- cbind(xTotal, activitylabel, subTotal)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
