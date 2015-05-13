---
title: "readme"
author: "Tim O'Brien"
date: "Monday, May 13, 2015"
output: html_document
---
There are three files contained in the project:
  - readme.MD - this document
  - run_analysis.R - the script documented below.
  - codeBook.MD - the codebook documenting variables used in the script run_analysis.R


There are several steps followed in run_analysis.R to create a tidy.txt output 
file.

Set the environment -   load the dplyr and Hmisc libraries, set two variables for 
                        directories used in the script.

Set the project file names - concatenate the file names from the UCI HAR Dataset
                        with the directory in which the data is stored.

Get the source data -   this was not used since the project assumed the data was 
                        already in the working directory. If it were used, it
                        would download a zip file from the provided URL, record
                        the date, the file was downloaded and unzip it into the
                        dirData directory.

Load the raw data -     Loading the raw data occurs in two main steps: loading
                        the column labels into variables and loading the data 
                        frames. I added the column headers during this step 
                        because it made the merge in a later step easier.

Merge training and test data - using cbind, I added the subject, Y and X
                        training data into sensor training data set using cbind. 
                        The subject data set was bound first to make the data set
                        easier to read. I then added the subject, Y and X test 
                        data into a sensor test data set, also using cbind. And, 
                        then finally combined both the raw training and test data 
                        into a single data set using rbind.

Extract measurements -  Performed a select from the combined raw data set by 
                        matching on the Subject, ActivityID, and columns
                        containing "mean" and "std". I excluded the columns that 
                        began with "angle" since these did not appear to be 
                        means. This data was extracted into a new data set named 
                        "dataSensorMeanAndStdDev".

Use descriptive activity names - joined the dataActivityLabels created in loading
                        the raw data with the measurements data created above. I 
                        used the dataActivityLabels data set first because it 
                        made the resulting data set easier to read. I also tested 
                        to ensure the inner join resulting from the merge had the
                        same number of rows as the original data set.

Label the data set -    Since I already named the data set during the loading
                        phase, I renamed some of the columns to make them more
                        descriptive and readable. I used camel case. A list of
                        replacements using gsub includes:
                                ^f for "FrequencyDomain"
                                n^t for "TimeDomain"
                                nAcc for "Acceleration"
                                nFreq\\. for "Frequency."
                                nFreq$ for "Frequency"
                                nGyroJerk for "AngularAcceleration"
                                nGyro for "AngularSpeed"
                                nMag for "Magnitude"
                                n\\.mean for "Mean"
                                n\\.std for "StandardDeviation"

Output a tidy data set - using the data set from the prior step, obtain the 
                        average of each variable for each activity and subject.
                        This was accomplished by grouping by subject and activity
                        then summarising each column and applying a mean.
                        Finally, this tidy data set was output to a file tidy.txt.
