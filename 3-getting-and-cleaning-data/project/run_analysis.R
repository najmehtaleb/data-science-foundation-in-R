
#HELP https://github.com/ajay2589/GettingAndCleaningData

# 1. Merges the training and the test sets to create one data set.
mergeData = function(){
  
  filesPath = "UCI HAR Dataset/"
  
  features = read.table(paste0(filesPath, "features.txt"))
  
  subject_train = read.table(paste0(filesPath, "train/subject_train.txt"))
  X_train = read.table(paste0(filesPath, "train/X_train.txt"))
  y_train = read.table(paste0(filesPath, "train/y_train.txt"))
  
  subject_test = read.table(paste0(filesPath, "test/subject_test.txt"))
  X_test = read.table(paste0(filesPath, "test/X_test.txt"))
  y_test = read.table(paste0(filesPath, "test/y_test.txt"))

  subject = rbind(subject_train, subject_test)
  X = rbind(X_train, X_test)
  y = rbind(y_train, y_test)
  
  colnames(X) = features[, 2]
  colnames(y) = "activity"
  colnames(subject) = "subject"
  
  data = cbind(X, y, subject)
  return(data)
}


# 2. Extracts only the measurements on the mean and standard deviation 
# for each measurement. 
extractColumns = function(dataframe){
  
  pat = ".*mean.*|.*std.*"
  extractedColumns = grep(pat, colnames(dataframe), ignore.case = TRUE)
  df = dataframe[, c(extractedColumns, 562, 563)]
  return(df)
}


# 3. Uses descriptive activity names to name the activities in the data set.
renameActivities = function(dataframe){
  
  activity_lables = read.table(paste0("UCI HAR Dataset/activity_labels.txt"))
  dataframe$activity = sapply(dataframe$activity, function(x) x=activity_lables[x, 2])
  return(dataframe)
}


# 4. Appropriately labels the data set with descriptive variable names.
renameVariables = function(dataframe){
  
  colnames(dataframe) = gsub("ACC", "Accelerometer", colnames(dataframe))
  colnames(dataframe)<-gsub("Gyro", "Gyroscope", colnames(dataframe))
  colnames(dataframe)<-gsub("BodyBody", "Body", colnames(dataframe))
  colnames(dataframe)<-gsub("Mag", "Magnitude", colnames(dataframe))
  colnames(dataframe)<-gsub("^t", "Time", colnames(dataframe))
  colnames(dataframe)<-gsub("^f", "Frequency", colnames(dataframe))
  colnames(dataframe)<-gsub("tBody", "TimeBody", colnames(dataframe))
  colnames(dataframe)<-gsub("-mean()", "Mean", colnames(dataframe), ignore.case = TRUE)
  colnames(dataframe)<-gsub("-std()", "STD", colnames(dataframe), ignore.case = TRUE)
  colnames(dataframe)<-gsub("-freq()", "Frequency", colnames(dataframe), ignore.case = TRUE)
  colnames(dataframe)<-gsub("angle", "Angle", colnames(dataframe))
  colnames(dataframe)<-gsub("gravity", "Gravity", colnames(dataframe))
  return(dataframe)
}

 
# 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.
takeMean = function(dataframe){
  tidyData = dataframe %>% group_by(activity, subject) %>% 
    summarise(across(everything(), mean), .groups = 'drop') %>% as.data.frame()
  write.table(tidyData, file = "tidyData.txt")
  return(tidyData)
}


# Run the script
runScript = function(){
  
  df1 = mergeData()
  df2 = extractColumns(df1)
  df3 = renameActivities(df2)
  df4 = renameVariables(df3)
  df5 = takeMean(df4)
  return(df5)
}









