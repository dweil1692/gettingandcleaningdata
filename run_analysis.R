#Set the default working directory
directory = getwd()
#Extract data variable names from the appropriate directory and convert to a 
#character vector
dataset_directory = c(paste0(directory, '/UCI HAR Dataset'))
setwd(dataset_directory)
featurenames = read.table('features.txt')
featurenames = as.character(featurenames$V2)
#Extract activity variable names and convert to a character vector
activitynames = read.table("activity_labels.txt")
activitynames = as.character(activitynames$V2)
setwd(directory)
#Extract training dataset, activity labels, and subject IDs from the appropriate directory
train_directory = c(paste0(getwd(), '/UCI HAR Dataset/train'))
setwd(train_directory)
dat1 = read.table('X_train.txt')
train_activities = read.table("y_train.txt")
train_activities = as.numeric(train_activities$V1)
train_subjects = read.table('subject_train.txt')
train_subjects = as.numeric(train_subjects$V1)
setwd(directory)
#Extract test dataset, activity labels, and subject IDs from the appropriate directory
test_directory = c(paste0(getwd(), '/UCI HAR Dataset/test'))
setwd(test_directory)
dat2 = read.table('X_test.txt')
test_activities = read.table("y_test.txt")
test_activities = as.numeric(test_activities$V1)
test_subjects = read.table('subject_test.txt')
test_subjects = as.numeric(test_subjects$V1)
setwd(directory)
#Combine the test and training datasets and assign each column the appropriate name
combined = rbind(dat1, dat2)
colnames(combined) = featurenames
print(head(combined[1:6]))
#Assign activity labels for each row of data in combined dataset
activities = c(train_activities, test_activities)
Activity = numeric(length(activities))
for (i in 1:length(activities)) {
  if (activities[i] == 1) {
    Activity[i] = activitynames[1]
  }
  else if (activities[i] == 2) {
    Activity[i] = activitynames[2]
  }
  else if (activities[i] == 3) {
    Activity[i] = activitynames[3]
  }
  else if (activities[i] == 4) {
    Activity[i] = activitynames[4]
  }
  else if (activities[i] == 5) {
    Activity[i] = activitynames[5]
  }
  else if (activities[i] == 6) {
    Activity[i] = activitynames[6]
  }
}
Activity = as.data.frame(Activity)
combined = cbind(Activity, combined)
#Add in a column for subject IDs
Subject = c(train_subjects, test_subjects)
Subject = as.data.frame(Subject)
combined = cbind(Subject, combined)
#Extract out a new data frame containing only means and standard deviations
final = cbind(combined['Subject'], combined['Activity']) #first two columns
names = colnames(combined)
meannames = names[grep(pattern = '-mean()', names, fixed = TRUE)]
stdnames = names[grep(pattern = '-std()', names, fixed = TRUE)]
for (i in 1:length(meannames)) {
  final = cbind(final, combined[meannames[i]], combined[stdnames[i]])
}
#Creates an independent tidy data set containing the average for each measure for each
#activity and each subject
usubject = unique(as.character(final$Subject))
uactivity = unique(as.character(final$Activity))
averages = ncol(final)
newfinal = as.data.frame(matrix(nrow = 0, ncol = length(names(final)))) #initalize empty data frame
for (i in 1:length(usubject)) {
  sbst = subset(final, final$Subject == usubject[i])
  for (j in 1:length(uactivity)) { #subset by subject and activity, find column averages for each subset.
    sbst2 = subset(sbst, sbst$Activity == uactivity[j])
    means = colMeans(sbst2[3:length(names(final))])
    colnames(means) = NULL
    trans = as.data.frame(t(means))
    tmp = cbind(usubject[i], uactivity[j], trans)
    newfinal = rbind(newfinal, tmp) #append averages to initialized data frame
  }
}
colnames(newfinal) = names(final)
print(head(newfinal[1:6]))
write.table(newfinal, 'tidy.txt', row.name = FALSE)
