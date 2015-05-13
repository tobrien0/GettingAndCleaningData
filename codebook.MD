---
title: "codebook"
author: "Tim O'Brien"
date: "Monday, May 11, 2015"
output: html_document
---

Raw Data
=================================================================================================
The raw data was provided by the project using the following URL. This data is 
documented within the zip fie. 
        https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The following description was provided within the raw data.
---------------------------------------------------------------
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
---------------------------------------------------------------
- Its activity label.
- An identifier of the subject who carried out the experiment.
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 

Tidy Data
=================================================================================================
- ActivityId
- Activity label
- Subject identified
- Tidy data set with the average of each of 79 variables for each activity and each subject.
  The 79 variables resulted from the mean and standard deviation from the original data set
  (training and test data) but did not include variables beginning with "angle". Each column
  has a descriptive header.

Variables used in transformation
=================================================================================================
The following variables were used in transforming the data sets via run_analysis.R
as described in readme.MD. The transformation generally combines the raw test and training
data, adds column headings, extracts a subset of the data, groups, summarizes, and 
calculates a mean for each column. This tidy data set is then output to a txt file.

Variable                Description
-------------           -------------------------------------------------------
dirData	                directory which contains the Samsung Data. This is used
                        in some of the project file names.
dirDataSet	        The directory which contains the unzipped UCI HAR Dataset
fileActivityLabels	The fully qualified file name for activity_labels.txt
fileFeature	        The fully qualified file name for features.txt
fileSubjectTest	        The fully qualified file name for subject_test.txt
fileSubjectTrain	The fully qualified file name for subject_train.txt
fileXTest	        The fully qualified file name for X_test.txt
fileXTrain	        The fully qualified file name for X_train.txt
fileYTest	        The fully qualified file name for y_test.txt
fileYTrain	        The fully qualified file name for y_train.txt
fileZip	                The fully qualified file name for dataset.zip
dateDownloaded	        The documentation of the data the data set was downloaded
url	                The provided URL of the UCI HAR dataset
dataFeatures	        A dataframe containing the feature labels and related 
                        column headings
dataActivityLabels	A dataframe containing the Subjects and related column 
                        headings
dataSubjectTest	        A dataframe containing the test subjects and related 
                        column heading
dataSubjectTrain	A dataframe containing the training subjects and related
                        column heading
dataXTest	        A dataframe containing the X test measurements and 
                        related column headings
dataXTrain	        A dataframe containing the X train measurements and 
                        related column headings
dataYTest	        A dataframe containing the Y test measurements and 
                        related column headings
dataYTrain	        A dataframe containing the Y train measurements and related column headings
dataSensorTraining	A dataframe containing the column binding of 
                        dataSubjectTrain, dataYTrain and dataXTrain
dataSensorTest	        A dataframe containing the column binding of 
                        dataSubjectTest, dataYTest and dataXTest
dataSensor	        A dataframe containing the row bimding of 
                        dataSensorTraining and dataSensorTest
dataSensorMeanAndStdDev	A dataframe containing a selection of columns matching 
                        Subject, ActivityID and those columns contain "mean" 
                        with the exception of those columns starting with "angle".
rowCount	        Is an integer containing the row count from 
                        dataSensorMeanAndStdDev. It is used later to ensure a 
                        merge does not eliminate any rows during an inner join.
dataTidySet	        A grouped dataframe that is used to store the results 
                        of the grouping dataSnesorMeanAndStdDev by Subject and 
                        Activity. It is then used to store this data which is 
                        summarized and means are calculated.
