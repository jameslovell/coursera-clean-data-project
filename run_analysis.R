library(reshape2)

#set up utility functions
loadData <- function(dir){
  dataHome <- "UCI HAR Dataset"
  path <- paste(getwd(), dataHome, dir, sep="/")
  xdata <- paste(path, "/X_", dir, ".txt", sep="")
  ydata <- paste(path, "/y_", dir, ".txt", sep="")
  subject <- paste(path, "/subject_", dir, ".txt", sep="")
  data <- cbind(read.csv(xdata, sep="", header=FALSE),
                read.csv(ydata, sep="", header=FALSE),
                read.csv(subject, sep=" ", header=FALSE))
  
  return(data)
}

getColumnHeaders <- function(){
  features <- read.csv(paste(getwd(), "UCI HAR Dataset", "features.txt", sep="/"), 
                       sep=" ", header=FALSE)
  features <- gsub("-", ".", gsub("[\\(\\)]", "", as.character(features$V2)))
  headers <- c(features, "activity", "subject")
  
  return(headers)
}

subsetData <- function(data, headers){
  targetColumns <- headers[grep("mean\\.|mean$|std\\.|std$|activity|subject", headers)]
  targetData <- subset(data, select=targetColumns)
  
  return(targetData)
}

getActivities <- function(){
  a <- read.csv(paste(getwd(), "UCI HAR Dataset", "activity_labels.txt", sep="/"), 
                sep = " ", header = FALSE)
  
  return(as.character(a$V2))
}

setActivities <- function(data, act){
  for(i in 1:length(act)){
    data$activity[data$activity==i] <- as.character(act[i])
  }
  
  return(data)
}

makeTidyData <- function(data){
  numVars <- length(names(data))-2
  melted <- melt(data, id=c("subject", "activity"), measure.vars=colnames(data)[1:numVars])
  tidyData <- dcast(melted, subject + activity ~ variable, mean)
  
  return(tidyData)
}

# Merge the training and the test sets to create one data set
mergedData <- rbind(loadData("test"), loadData("train"))

# Appropriately label the data set with descriptive variable names.
colHeaders <- getColumnHeaders()
colnames(mergedData) <- colHeaders

# Extract only the measurements on the mean and standard deviation for each measurement.
targetData <- subsetData(mergedData, colHeaders)

# Use descriptive activity names to name the activities in the data set
newData <- setActivities(mergedData, getActivities())

# Create a second, independent tidy data set with the average of each variable for each activity and each subject.
write.csv(makeTidyData(newData), "tidyData.csv")


