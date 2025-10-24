library(dplyr)

# load training data, test data, labels, and features from files
X_train         <- read.table("./train/X_train.txt")
X_test          <- read.table("./test/X_test.txt")
y_train         <- read.table("./train/y_train.txt")
y_test          <- read.table("./test/y_test.txt")
subject_train   <- read.table("./train/subject_train.txt")
subject_test    <- read.table("./test/subject_test.txt")
features        <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")

# label all columns
colnames(X_train)         <- features[, 2]
colnames(X_test)          <- features[, 2]
colnames(y_train)         <- "ActivityNumber"
colnames(y_test)          <- "ActivityNumber"
colnames(subject_train)   <- "Subject"
colnames(subject_test)    <- "Subject"
colnames(activity_labels) <- c("ActivityNumber", "ActivityName")

# 1. Merges the training and the test sets to create one data set.
data_merged <- rbind(cbind(y_train, subject_train, X_train), cbind(y_test, subject_test, X_test))

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
data_mean_std <- data_merged[, grepl("ActivityNumber|Subject|mean\\(\\)|std\\(\\)", colnames(data_merged))]

# 3. Uses descriptive activity names to name the activities in the data set
data_mean_sd_names <- merge(data_mean_std, activity_labels, by = "ActivityNumber", all.x = TRUE)

# 4. Appropriately labels the data set with descriptive variable names. 
colnames(data_mean_sd_names) <- gsub("^t", "Time", colnames(data_mean_sd_names))
colnames(data_mean_sd_names) <- gsub("^f", "Frequency", colnames(data_mean_sd_names))
colnames(data_mean_sd_names) <- gsub("Acc", "Accelerometer", colnames(data_mean_sd_names))
colnames(data_mean_sd_names) <- gsub("Gyro", "Gyroscope", colnames(data_mean_sd_names))
colnames(data_mean_sd_names) <- gsub("Mag", "Magnitude", colnames(data_mean_sd_names))
colnames(data_mean_sd_names) <- gsub("BodyBody", "Body", colnames(data_mean_sd_names))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- data_mean_sd_names %>% group_by(Subject, ActivityNumber, ActivityName) %>% summarise_all(mean)
write.table(tidy_data, "tidy_data.txt", row.names = FALSE)