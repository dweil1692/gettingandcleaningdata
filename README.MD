Author: Dylan Weil

Project: Getting and Cleaning Data Coursera Course Project

This script reads in all files containing the raw data and labels/identifiers associated with the raw data including activity labels, subject IDs, and column names. 
Directory switching is built into the script when reading in these files, so as long as the dataset is in your default working directory, you will not need to worry about switching directories manually.

The script operates by first importing all necessary files from the ‘UCI HAR Dataset’ folder.
It then uses the rbind() operation to merge together both the raw data files, and converts the .txt file containing the feature names into a character vector to be used as the column names.  
In addition, the activity numbers associated with each raw data set are read in as numerical vectors and converted to their corresponding descriptive names. 
This, along with a vector of the subject IDs, is appended to the merged data frame using the cbind() operation. 
The grep() function is then used to find feature names containing “mean” or “std” and the data frame is subsetted according to names containing those expressions.

In order to derive a new data frame containing all mean/std averages for each activity for each subject, the unique() operation is used to find all unique subject IDs and activity names, and a double nested for loop is used to iterate through these vectors and subset the data frame accordingly.  
For each subset present in each iteration, the colMeans() function is used to find the average present in each column, and the t() function is used to reverse the rows and columns of the colMeans() output. 
This is then appended to the current subject ID and activity name, which is then in turn appended using the rbind() operation to an empty data frame initialized prior to the for loops. 
The resulting data frame is then printed to the console as well as written to a text file to be saved in the original working directory.
