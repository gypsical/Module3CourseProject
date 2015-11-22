##You should create one R script called run_analysis.R that does the following. 
##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##load packages
library(plyr)
library(data.table)
library(dplyr)

#download and read in data from files
fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile="SmartPhonesDataset.zip")
unzip(zipfile="SmartPhonesDataset.zip")

x_train<-read.table("./train/X_train.txt")
y_train<-read.table("./train/y_train.txt")
subject_train<-read.table("./train/subject_train.txt")
x_test<-read.table("./test/X_test.txt")
y_test<-read.table("./test/y_test.txt")
subject_test<-read.table("./test/subject_test.txt")

features<-read.table("features.txt")
activity<-read.table("activity_labels.txt")

## Part 1 ##
#merge datasets
train_data<-cbind(y_train,subject_train,x_train)
test_data<-cbind(y_test,subject_test,x_test)
all_data<-rbind(train_data,test_data)

#assign column names to dataset
featureNames<-as.character(features[,2])
labels<-c("ActivityID","Subject",featureNames)
names(all_data)<-labels

## Part 2 ##
#extract obs for mean and std
MeanSTD<- all_data[,(grepl("-mean|-std|ActivityID|Subject", names(all_data),fixed=FALSE, ignore.case=TRUE)
                    &!grepl("meanFreq",names(all_data),fixed=FALSE,ignore.case=TRUE))]

## Part 3 ##
#Naming activity type in dataset
colnames(activity)<-c("ActivityID","ActivityType")
MeanSTDData<- merge(MeanSTD, activity, by = "ActivityID", all.x=TRUE)

## Part 4 ##
#Label dataset with descriptive variable names
head(str(MeanSTDData),2)
NewColNames<-colnames(MeanSTDData)

for (i in 1:length(NewColNames)) {
  NewColNames[i] <- gsub("\\(\\)","",NewColNames[i])
  NewColNames[i] <- gsub("-mean","Mean",NewColNames[i])
  NewColNames[i] <- gsub("-std","StandardDeviation",NewColNames[i])  
  NewColNames[i] <- gsub("^t","Time",NewColNames[i])
  NewColNames[i] <- gsub("^f","Frequency",NewColNames[i])
  NewColNames[i] <- gsub("Acc","Acceleration",NewColNames[i])
  NewColNames[i] <- gsub("Mag","Magnitude",NewColNames[i])
  NewColNames[i] <- gsub("Gyro","Gyroscope",NewColNames[i])
  NewColNames[i] <- gsub("BodyBody","Body",NewColNames[i])
}

names(MeanSTDData)<-NewColNames
head(str(MeanSTDData),2)

## Part 5 ##
SummaryData = ddply(MeanSTDData, .(Subject,ActivityType), numcolwise(mean))
write.table(SummaryData,"Tidydata.txt",row.name=FALSE)
