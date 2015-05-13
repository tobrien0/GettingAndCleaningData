### set project environment: libraries and  directories ###########################################
library(dplyr)
library(Hmisc)
dirData <- getwd()
dirDataSet <- paste(dirData,"UCI HAR Dataset", sep = "/")

### set project file names ########################################################################
fileActivityLabels <- paste(dirDataSet, "activity_labels.txt", sep = "/")
fileFeature <- paste(dirDataSet, "features.txt", sep = "/")
fileSubjectTest <- paste(dirDataSet, "test/subject_test.txt", sep = "/")
fileSubjectTrain <- paste(dirDataSet, "train/subject_train.txt", sep = "/")
fileXTest <- paste(dirDataSet, "test/X_test.txt", sep = "/")
fileXTrain <- paste(dirDataSet, "train/X_train.txt", sep = "/")
fileYTest <- paste(dirDataSet, "test/y_test.txt", sep = "/")
fileYTrain <- paste(dirDataSet, "train/y_train.txt", sep = "/")
fileZip <- paste(dirData, "dataset.zip", sep = "/")

### get the source data ###########################################################################
### not needed for project. there is an assumption that the data will be in working diretory.
#library(downloader)
#url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#if(!file.exists(dirData)){dir.create(dirData)}
#download(url, dest=fileZip, mode="wb")
### record date downloaded
#dateDownloaded <- date()
#dateDownloaded
#unzip(fileZip, exdir = dirData)

### 0.Load the raw data into data frames ##########################################################
### Load the labels first
dataFeatures <- read.table(fileFeature, colClasses = c("character"))
dataActivityLabels <- read.table(fileActivityLabels, col.names = c("ActivityId", "Activity"))

### Load the data frames and add the column headers
dataSubjectTest <- read.table(fileSubjectTest, col.names = "Subject")
dataSubjectTrain <- read.table(fileSubjectTrain, col.names = "Subject")
dataXTest <- read.table(fileXTest, col.names = dataFeatures$V2)
dataXTrain <- read.table(fileXTrain, col.names = dataFeatures$V2)
dataYTest <- read.table(fileYTest, col.names = "ActivityId")
dataYTrain <- read.table(fileYTrain, col.names = "ActivityId")

### 1.Merge the training and the test sets to create one data set. ################################
dataSensorTraining <- cbind(cbind(dataSubjectTrain,dataYTrain), dataXTrain)
dataSensorTest <- cbind(cbind(dataSubjectTest, dataYTest), dataXTest)
dataSensor <- rbind(dataSensorTraining, dataSensorTest)

### 2.Extract only the measurements on the mean and standard deviation for each measurement.#######
### I don't think the headers with angle are considered means
dataSensorMeanAndStdDev <- select(dataSensor, matches("Subject"), matches("ActivityId"), contains("mean",ignore.case = FALSE), contains("std",ignore.case = FALSE), -starts_with("angle"))
rowCount <- nrow(dataSensorMeanAndStdDev)

### 3.Use descriptive activity names to name the activities in the data set########################
dataSensorMeanAndStdDev <- merge(dataActivityLabels, dataSensorMeanAndStdDev,by.x="ActivityId",by.y="ActivityId",all=TRUE)
if (rowCount != nrow(dataSensorMeanAndStdDev)) {
        print("merge did not result in a complete inner join!")
}

### 4.Appropriately label the data set with descriptive variable names.############################
### I already named the columns above in step 0 because it made the merge easier
### So now I just need to make the names a bit more readable
names(dataSensorMeanAndStdDev) <- gsub('^f',"FrequencyDomain",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('^t',"TimeDomain",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('Acc',"Acceleration",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('Freq\\.',"Frequency.",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('Freq$',"Frequency",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('GyroJerk',"AngularAcceleration",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('Gyro',"AngularSpeed",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('Mag',"Magnitude",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('\\.mean',"Mean",names(dataSensorMeanAndStdDev))
names(dataSensorMeanAndStdDev) <- gsub('\\.std',"StandardDeviation",names(dataSensorMeanAndStdDev))

### 5.From the data set in step 4, ################################################################
### create a second, independent tidy data set with the average of each variable for each activity and each subject.
dataTidySet <- group_by(dataSensorMeanAndStdDev, Subject, Activity)
dataTidySet <- summarise_each(dataTidySet, funs(mean))
write.table(dataTidySet,paste(dirData,"tidy.txt", sep = "/"),row.name=FALSE)
