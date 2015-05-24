library(plyr)
## Importing feature and activity_labels data:

## features has a list that contains all the features
features=read.table('./features.txt',header=FALSE)

## Links the class labels with their activity name
activity_labels=read.table('./activity_labels.txt',header=FALSE)

## Importing data from the train folder:
subject_train=read.table('./train/subject_train.txt',header=FALSE)
x_train=read.table('./train/x_train.txt',header=FALSE)
y_train=read.table('./train/y_train.txt',header=FALSE)

## Importing data from the test folder:
subject_test=read.table('./test/subject_test.txt',header=FALSE)
x_test=read.table('./test/x_test.txt',header=FALSE)
y_test=read.table('./test/y_test.txt',header=FALSE)

## Now, all the data is imported.

## Lets assign names to the new train data:
colnames(activity_labels)=c("Activity_id","Activity_type") 
colnames(subject_train)="subject_id"

## To relate the features with the training set
colnames(x_train)=features[,2]
colnames(y_train)="Activity_id"

## To merge all the training data into one:
training_data=cbind(y_train,subject_train,x_train)

## Lets assign names to the new test data:
colnames(subject_test)="subject_id"
colnames(x_test)=features[,2]
colnames(y_test)="Activity_id"

## To merge all the test data into one:
test_data=cbind(y_test,subject_test,x_test)

## Finally, lets merge the training and test data into one called "Data":
Data=rbind(training_data,test_data)

col_Names=colnames(Data)
info_Vector = (grepl("Activity..",col_Names) | grepl("subject..",col_Names) | grepl("-mean..",col_Names) & !grepl("-meanFreq..",col_Names) & !grepl("mean..-",col_Names) | grepl("-std..",col_Names) & !grepl("-std()..-",col_Names))
Data=Data[info_Vector==TRUE]
Data=merge(Data,activity_labels,by='Activity_id',all.x=TRUE)

col_Names=colnames(Data)

for (i in 1:length(col_Names)) 
{
  col_Names[i] = gsub("\\()","",col_Names[i])
  col_Names[i] = gsub("-std$","StdDev",col_Names[i])
  col_Names[i] = gsub("-mean","Mean",col_Names[i])
  col_Names[i] = gsub("^(t)","time",col_Names[i])
  col_Names[i] = gsub("^(f)","freq",col_Names[i])
  col_Names[i] = gsub("([Gg]ravity)","Gravity",col_Names[i])
  col_Names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_Names[i])
  col_Names[i] = gsub("[Gg]yro","Gyro",col_Names[i])
  col_Names[i] = gsub("AccMag","AccMagnitude",col_Names[i])
  col_Names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_Names[i])
  col_Names[i] = gsub("JerkMag","JerkMagnitude",col_Names[i])
  col_Names[i] = gsub("GyroMag","GyroMagnitude",col_Names[i])
}

colnames(Data) = col_Names

Data_no_activity_labels=Data[,names(Data)!='Activity_labels']

tidy_Data=aggregate(Data_no_activity_labels[,names(Data_no_activity_labels)!= c('Activity_id','subject_id')],by=list(Activity_id=Data_no_activity_labels$Activity_id,subject_id=Data_no_activity_labels$subject_id),mean)

tidy_Data=merge(tidy_Data,activity_labels,by='Activity_id',all.x=TRUE)

write.table(tidy_Data,'./tidyData.txt',row.names=TRUE,sep='\t')