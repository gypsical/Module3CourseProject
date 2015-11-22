#CodeBook

This provides additional information on the data, variables, and transformations performed to produce the tidy dataset. 

## About the Data
### Data Source
The dataset used collects measurements on human activity recognition using smartphones. 

A full description of the dataset used can be found from the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)  

[Click here to download the dataset for project](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 

Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
*Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
*Triaxial Angular velocity from the gyroscope. 
*A 561-feature vector with time and frequency domain variables. 
*Its activity label. 
*An identifier of the subject who carried out the experiment.

## Data Transformation
### Part 1: Merge training and test datasets 
Download and unzip the data, and read into tables the following files located in the UCI HAR Dataset folder:

* features.txt
* activity_labels.txt
* X_train.txt
* y_train.txt
* subject_train.txt
* X_test.txt
* y_test.txt
* subject_test.txt

Merge the datasets into one dataset and assign appropriate column names using the features, subject and activityID.

### Part 2: Extract only meansurements on mean and standard deviation for each measurement
Create a logcal vector that contains TRUE values for columns that collect the mean() and std() of measurements and FALSE values for the others (i.e. column names containing mean() and std() only). Subset the data to keep only these columns including the subject and activity ID. 

### Part 3: Use descriptive activity names to name the activities in the dataset
Merge the data subsetted in Part 2 with the activity types using the activity ID. 

### Part 4: Label the dataset with descriptive variable names
Clean up the column names by using the gsub function

### Part 5: Create a tidy dataset that summarises the average of each variable for each activity and each subject.
Use the ddply function from the plyr package to summarise the means of each variable for each subject and each activity type. 



