#!/usr/bin/env Rscript

#Loading thge dplyr package for data manipulation
require(dplyr)

#Accepting the Samsung data directory as a command line input
args <- commandArgs(trailingOnly = TRUE)
workingDirectory <- args[1]

#Check if Directory exists and proceed
#Or else print Usage and exit

if(!file.exists(workingDirectory))
{
  stop("Input Directory Does Not Exist! Usage: run_analysis.R <Directory_Path>")
}
#Reading the activity labels and names into a data frame and change header appropriately
ActivityMap <- read.table(file = sprintf("%s/activity_labels.txt", workingDirectory),
                          sep = "",
                          stringsAsFactors = FALSE,
                          header = FALSE)

names(ActivityMap) <- c("ActivityLabel", "ActivityName")

#Reading the features into a data frame 
#convertingto Vector
#Adding Subject and ActivityName to the first couple of columns to add as header for the tidy dataframe
#Use make.names() to make column names Unique
features <- read.table(file = sprintf("%s/features.txt", workingDirectory),
                       sep = "",
                       stringsAsFactors = FALSE,
                       header = FALSE)

namesVector <- features[,2]
namesVector <- rbind(c("Subject", "Activity", namesVector))
namesVector <- make.names(namesVector, unique = TRUE)

#Reading data from test and train folders
#Subject Names, Observsations and Activity Labels stored into individual dataframes
#Adding header to Activity Label dataframes
testSubjects <- read.table(file = sprintf("%s/test/subject_test.txt", workingDirectory),
                           sep = "",
                           header = FALSE,
                           stringsAsFactors = FALSE)

testX <- read.table(file = sprintf("%s/test/X_test.txt", workingDirectory),
                    sep = "",
                    header = FALSE,
                    stringsAsFactors = FALSE)

testY <- read.table(file = sprintf("%s/test/y_test.txt", workingDirectory),
                           sep = "",
                           header = FALSE,
                           stringsAsFactors = FALSE)

names(testY) <- c("ActivityLabel")

trainSubjects <- read.table(file = sprintf("%s/train/subject_train.txt", workingDirectory),
                           sep = "",
                           header = FALSE,
                           stringsAsFactors = FALSE)

trainX <- read.table(file = sprintf("%s/train/X_train.txt", workingDirectory),
                    sep = "",
                    header = FALSE,
                    stringsAsFactors = FALSE)

trainY <- read.table(file = sprintf("%s/train/y_train.txt", workingDirectory),
                    sep = "",
                    header = FALSE,
                    stringsAsFactors = FALSE)

names(trainY) <- c("ActivityLabel")

#Mapping ActivityLabels to corresponding ActivityNames from the ActivityMap
testActivityNames <- left_join(ActivityMap, testY, by = "ActivityLabel") %>% select(ActivityName)
trainActivityNames <- left_join(ActivityMap, trainY, by = "ActivityLabel") %>% select(ActivityName)

#Merging Subject and Activity information to Test and Train Observations
#Merging Test and Train Observations in Tidy Dataframe format to create one large tidy Samsung DataFrame
tidyTestData <- cbind(testSubjects, testActivityNames, testX)
tidyTrainData <- cbind(trainSubjects, trainActivityNames, trainX)
tidySamsungData <- rbind(tidyTestData, tidyTrainData)
names(tidySamsungData) <- namesVector

#Writing the resulting Tidy Dataframe to Disk in Text Format
write.table(tidySamsungData,
            file = sprintf("%s/SamsungData.txt", workingDirectory), 
            sep = "\t",
            row.names = FALSE)

#Extracing Columns containing Observation Means and Std. Devs into a new tidy Data Frame
tidySamsungDataMeanStds <- tidySamsungData %>%
  select(matches("mean|std"))

#Creating another DataFrame by summarizing the means of all variables for each subject and activity name 
tidySamsungDataColMeans <- tidySamsungData %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))