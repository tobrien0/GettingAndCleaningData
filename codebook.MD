---
title: "codebook"
author: "Tim O'Brien"
date: "Monday, May 11, 2015"
output: html_document
---

The raw data was provided by the project using the following URL. This data is 
documented within the zip fie. 
        https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

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
