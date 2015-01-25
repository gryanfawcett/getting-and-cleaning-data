#### Cousera Getting and Cleaning Data Project runanalysis.r 




# The following script does the following on the UCI Dataset provided via samsung 
# 1. Merges training and test data sets
# 2. Extracts the messurements for the man and standard devation for each record
# 3. Change feature names to be more Discriptive for the activitys on the data set
# 4. Changes the names for the labels as well
# 4. Creates a second, new tidy data set with the averages of the varables for activities and subjet

#sets the working directory and reads from the data files
setwd('/Users/gryanfawcett/JHDataScience/UCI-HAR-Data')
feat = read.table('./features.txt',header=FALSE)
acttype = read.table('./activity_labels.txt', header=FALSE)
subjtrain = read.table('./train/subject_train.txt', header=FALSE)
xtrain = read.table('./train/x_train.txt',header=FALSE)
ytrain = read.table('./train/y_train.txt',header=FALSE)
subjecttest =read.table('./test/subject_test.txt',header=FALSE)
xtest = read.table('./test/X_test.txt', header=FALSE)
ytest = read.table('./test/y_test.txt', header=FALSE)

#sets column names for test and training set data
colnames(acttype) = c('activityid','activitytype')
colnames(subjtrain) = "subjectid"
colnames(xtrain) = feat[,2]
colnames(ytrain) = "activityid"
colnames(subjecttest) = "subjectid"
colnames(xtest) = feat[,2]
colnames(ytest) = "activityid"


# define test and training data merging datasets
trainingdata = cbind(ytrain,subjtrain,xtrain)
testdata = cbind(ytest,subjecttest,xtest)

#Creates final data set out of test and train and creates a vector of column names
finaldata = rbind(trainingdata,testdata)
colNames = colnames(finaldata)

# create a logical vector that contains TRUE values for the ID, mean & stdev columns and False for all others
logVect = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#subsets the data based on the logical vector to keep needed columns and merges it activitytype
finaldata = finaldata[logVect==TRUE]
finaldata = merge(finaldata,acttype,by='activityid',all.x=TRUE)
colNames = colnames(finaldata)

#cleans up the data names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

#applys new column named to the data set
colnames(finaldata) = colNames

#creates a new table without the activitytype column
finalDataNoacttype = finaldata[,names(finaldata) != 'acttype']


tidydata = aggregate(finalDataNoacttype[,names(finalDataNoacttype) != c('activityid','subjectid')],by=list(activityid=finalDataNoacttype$activityid,subjectid = finalDataNoacttype$subjectid),mean)

tidydata = merge(tidydata,acttype,by='activityid',all.x=TRUE)

write.table(tidydata, './tidyData.txt',row.names=TRUE,sep='\t');