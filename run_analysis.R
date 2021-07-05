library(reshape2)
library(tidyr)

directory <- getwd()
#Train data
x_train <- read.table(paste(sep = "", directory, "/train/X_train.txt"))
y_train <- read.table(paste(sep = "", directory, "/train/y_train.txt"))
s_data <- read.table(paste(sep = "", directory, "/train/subject_train.txt"))

#Test data
x_test <- read.table(paste(sep = "", directory, "/test/X_test.txt"), fill = TRUE)
y_test <- read.table(paste(sep = "", directory, "/test/y_test.txt"))
s_test <- read.table(paste(sep = "", directory, "/test/subject_test.txt"))

# merging training and test data
shared_x <- intersect(colnames(x_train), colnames(x_test))
x_data = rbind(subset(x_train, select = shared_x),subset(x_test, select = shared_x))
shared_y <- intersect(colnames(y_test), colnames(y_train))
y_data = rbind(subset(y_test, select = shared_y),subset(y_train, select = shared_y))
s_data <- rbind(s_train, s_test)

#activity features
activity <- read.table(paste(sep = "", directory, "/UCI HAR Dataset/features.txt"))

# labeling the variables
label_names <- read.table(paste(sep = "", directory, "/UCI HAR Dataset/activity_labels.txt"))
label_names[,2] <- as.character(label_names[,2])

# extracting mean and std 
measurements <- grep("-(mean|std).*", as.character(activity[,2]))
measurement_names <- activity[measurements, 2]
measurement_names <- gsub("-mean", "Mean", measurement_names)
measurement_names <- gsub("-std", "Std", measurement_names)
measurement_names <- gsub("[-()]", "", measurement_names)

# extracting final data 
x_data <- x_data[measurements]
merged_data <- cbind(s_data, x_data, y_data)
colnames(merged_data) <- c("Subject", "Activity", measurement_names)

merged_data$Activity <- factor(merged_data$Activity, levels = label_names[,1], labels = label_names[,2])
merged_data$Subject <- as.factor(merged_data$Subject)

# putting it all together 
melted_data <- melt(merged_data, id = c("Subject", "Activity"))
cleaned_data <- dcast(melted_data, Subject + Activity ~ variable, mean)

write.table(cleaned_data, "./clean_dataset.txt", row.names = FALSE, quote = FALSE)
