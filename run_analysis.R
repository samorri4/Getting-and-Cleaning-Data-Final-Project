library(dplyr)

filename <- "course3finalproject.zip"

#checking if file exists and downloading if not
if(!file.exists(filename)){
	URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(URL, filename)
}

#checking if the folder is there and unzip if it isn't
if(!file.exists("UCI HAR data")){
	unzip(filename)
}

#creating data frames
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Step 1: Merge train and test sets to create one data set
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
data_merged <- cbind(subject, x, y)

#Step 2: Extract only the measurements on the mean and std for each environment
tidydata <- data_merged %>% select(subject, code, contains("mean"), contains("std"))

#Step 3: Use descritive activity names to name the activities in the data set
tidydata$code <- activity_labels[tidydata$code, 2]

#Step 4: Appropriately label the data set with descriptive variable names
names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "Standard Deviation", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))

#Step 5: Create second tidy data set with average of each variable for each activity and subject
seconddata <- tidydata %>%
	group_by(subject, activity) %>%
	summarise_all(funs(mean))
write.table(seconddata, "seconddata.txt", row.name = FALSE)

