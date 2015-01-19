#Merges the training and the test sets to create one data set.
subject_train<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt",
                          nrows=7352,col.names="subject")

subject_test<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt",
                         nrows=2947,col.names="subject")

subject<-rbind(subject_train,subject_test)

rm("subject_train")
rm("subject_test")

#Merges the training and the test sets to create one data set.
y_train<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt",
                    nrows=7352,col.names="activity")

y_test<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt",
                   nrows=2947,col.names="activity")

y<-rbind(y_train,y_test)
rm("y_train")
rm("y_test")
#Merges the training and the test sets to create one data set.
X_train<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt",
                    nrows=7352)

X_test<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt",
                   nrows=2947)

X<-rbind(X_train,X_test)
rm("X_train")
rm("X_test")
#

features<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\features.txt",
                     nrows=561,header=F)


names(X)<-features$V2

rm("features")
#Extracts only the measurements on the mean and standard deviation for each measurement.
X_mean_std<-X[,sort(c(grep("mean()",names(X)),grep("std()",names(X))))]
rm("X")
#Merges the training and the test sets to create one data set.
merged.dataset<-cbind(subject,y,X_mean_std)
rm("subject")
rm("y")
rm("X_mean_std")
#Uses descriptive activity names to name the activities in the data set
activitylabels<-read.table(".\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt",
                     nrows=6,header=F)
tolower(activitylabels$V2)
activitylabels$V2<-sub("_"," ",tolower(activitylabels$V2))

descripnames<-function(num){(activitylabels$V2)[num]}
merged.dataset$activity<-sapply(merged.dataset$activity,descripnames)

#Appropriately labels the data set with descriptive variable names. 
names(merged.dataset)<-tolower(names(merged.dataset))

names(merged.dataset)<-gsub("[-|()]","",names(merged.dataset))


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
newset=c()

for (sub in 1:30){
        temp1<-merged.dataset[merged.dataset$subject==sub,]
        for(acti in c("walking","walking upstairs","walking downstairs",
                      "sitting","standing","laying")){
                temp2=temp1[temp1$activity==acti,]
                newset<-rbind(newset,lapply(temp2[3:81],mean))
        }
}

colsubj<-rep(1:6,30)
colacti<-rep(c("walking","walking upstairs","walking downstairs","sitting","standing","laying"),30)

temp<-cbind(colsubj,colacti)
finalset<-cbind(temp,newset)

#write.table
write.table(finalset,"tidydata.txt",row.names=F)