library("stringr")

analysis<-function(dir){
  ####### Merges the training and the test sets to create one data set. ##################################
  testSet<-read.table(paste(dir,"\\test\\X_test.txt",sep=""))
  trainSet<-read.table(paste(dir,"\\train\\X_train.txt",sep=""))
  mergedSet<-rbind(trainSet,testSet)
  
  ####### Extracts only the measurements on the mean and standard deviation for each measurement #########
  features<-read.table(paste(dir,"\\features.txt",sep=""))
  names(mergedSet)<-features[,2]
  mergedSet<-mergedSet[, c(which(str_detect(features[,2],"std")),which(str_detect(features[,2],fixed("mean()"))))]
  
  
  trainSub<-read.table(paste(dir,"\\train\\subject_train.txt",sep=""))
  testSub<-read.table(paste(dir,"\\test\\subject_test.txt",sep=""))
  mergedSub<-c(trainSub[,1],testSub[,1])
  
  
  
  ####### Uses descriptive activity names to name the activities in the data set ########################
  trainA<-read.table(paste(dir,"\\train\\y_train.txt",sep=""))
  testA<-read.table(paste(dir,"\\test\\y_test.txt",sep=""))
  mergedA<-c(trainA[,1],testA[,1])
  activities <- read.table(paste(dir,"\\activity_labels.txt",sep=""))
  mergedA <- activities[mergedA, 2]
  
  
  ####### Appropriately labels the data set with descriptive variable names. ############################
  mergedSet$subject<-mergedSub
  mergedSet$activities<-mergedA
  
  
  ####### creates tidy data set with the average of each variable for each activity and each subject.###
  tidyData<-aggregate(data=mergedSet,FUN=mean, .~subject+activities)
  
  ####### reshape data to long form ####################################################################
  
  
  write.table(tidyData, paste(dir,"\\output.txt",sep=""), row.name=F)
}

analysis(dir="D:\\Documents\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")
