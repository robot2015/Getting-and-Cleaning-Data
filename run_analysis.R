run_analysis <- function() {
  # run_analysis
  # This function will merge the training and the test sets 
  # from the Human Activity Recognition Using Smartphones Data Set
  # (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
  # to create one data set. It will extract only the measurements 
  # on the mean and standard deviation for each measurement. 
  # Output is a second, independent tidy data set with the 
  # average of each variable for each activity and each subject.
  
  ## Load data.
  # List files.
  fileList <- list(list("UCI HAR Dataset/",
                        "activity_labels",
                        "features"),
                    list("UCI HAR Dataset/test/",
                         "subject_test",
                         "x_test",
                         "y_test"),
                    list("UCI HAR Dataset/train/",
                         "subject_train",
                         "x_train",
                         "y_train"))
  
  # Open files.
  for(i in seq_along(fileList)) {
    for(j in seq_along(fileList[[i]])) {
      if(j == 1) {
        directory <- fileList[[i]][[j]]
      } else {
        filePath <- paste(directory, fileList[[i]][[j]], ".txt", sep = "")
        d <- read.table(filePath)
        assign(as.character(fileList[[i]][[j]]), d)
      }
    }
  }
  
  ## Merges the training and the test sets to create one data set.
  # Create label for each data set.
  set_test <- rep("test", dim(subject_test)[1])
  set_test <- as.data.frame(set_test)
  colnames(set_test) <- "Set"
  
  set_train <- rep("train", dim(subject_train)[1])
  set_train <- as.data.frame(set_train)
  colnames(set_train) <- "Set"
  
  set_all <- rbind(set_test, set_train)
  
  # Join subjects.
  colnames(subject_test) <- "Subject"
  colnames(subject_train) <- "Subject"
  subject_all <- rbind(subject_test, subject_train)
  subject_all$Subject <- factor(subject_all$Subject)
  
  # Join y data.
  colnames(y_test) <- "Activity"
  colnames(y_train) <- "Activity"
  y_all <- rbind(y_test, y_train)
  y_all$Activity <- factor(y_all$Activity,
                      levels = as.numeric(activity_labels$V1),
                      labels = as.character(activity_labels$V2))
  
  # Join x data.
  featureNames <- as.list(features[2])
  colnames(x_test) <- featureNames[[1]]
  colnames(x_train) <- featureNames[[1]]
  x_all <- rbind(x_test, x_train)
  
  # Combine all data.
  harData <- cbind(set_all, subject_all, y_all, x_all)
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement.
  harNames <- colnames(harData)
  labelCols <- as.integer(1:3)
  meanCols <- grep("mean", harNames, ignore.case = TRUE)
  stdCols <- grep("std", harNames, ignore.case = TRUE)
  saveCols <- c(labelCols, meanCols, stdCols)
  saveCols <- sort(saveCols)
  
  harData.sub <- harData[, saveCols]
  
  # From the data set in step 4, creates a second, independent tidy data set 
  # with the average of each variable for each activity and each subject.
  # stack data.
  harData.labels.stack <- harData.sub[rep(row.names(harData.sub), 86), 2:3]
  row.names(harData.labels.stack) <- NULL
  harData.sub.stack <- stack(harData.sub[, 4:89])
  colnames(harData.sub.stack) <- c("Value", "Parameter")
  
  harData2 <- cbind(harData.labels.stack, harData.sub.stack)
  
  # Calculate mean.
  harData2.mean <- tapply(harData2[, 3], harData2[, c(1, 2, 4)], mean)
  harData2.mean <- aperm(harData2.mean, perm = c(3, 2, 1))
  harData2.mean <- as.data.frame(harData2.mean)
  harData2.mean <- t(harData2.mean)
  
  # Get row names.
  meanNames <- rownames(harData2.mean)
  meanNames2 <- strsplit(meanNames, "[.]")
  
  # Set activity names.
  actNames <- sapply(meanNames2, "[", 1)
  actNames <- factor(actNames,
                     levels = as.character(activity_labels$V2))
  actNames <- as.data.frame(actNames)
  colnames(actNames) <- "Activity"
  
  # Set subject names.
  subjectNames <- sapply(meanNames2, "[", 2)
  subjectNames <- as.integer(subjectNames)
  subjectNames <- factor(subjectNames)
  subjectNames <- as.data.frame(subjectNames)
  colnames(subjectNames) <- "Subject"
  
  # Combine data.
  rownames(harData2.mean) <- NULL
  harData2.mean.final <- cbind(subjectNames, actNames, harData2.mean)
}