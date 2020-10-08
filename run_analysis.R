
#Packages load
library(dplyr)
 
# get dataset from web
if (!file.exists(filename)){
     File_Folder <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     download.file(File_Folder, filename, method="curl")
}  
  
if (!file.exists("UCI HAR Dataset")) { 
     unzip(filename) 
}

# 1 . merge {train, test} data set
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("index","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("cod", "activity"))
s_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_test")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
s_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_train")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

X_get <- rbind(x_train, x_test)
Y_get <- rbind(y_train, y_test)
Subject <- rbind(s_train, s_test)
Merged_Data <- cbind(Subject, Y_get, X_get)

# 2 . Extracts only the measurements on the mean and standard deviation for each measurement
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# 3 . Uses descriptive activity names to name the activities in the data set.
TidyData$code <- activities[TidyData$code, 2]

# 4 . Appropriately labels the data set with descriptive variable names
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

# 5 . From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Output <- TidyData %>%
     group_by(subject, activity) %>%
     summarise_all(funs(mean))
write.table(Output, "Output.txt", row.name=FALSE)
