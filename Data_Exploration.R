#setting the working directory to be home directory prior to beginning 
setwd("~/")

#reading the training data

training_data <- read.csv("train.csv")

#looking at structure and first few entries of training data

str(training_data)
head(training_data,5)

#reading the testing data

testing_data <- read.csv("test.csv")

#looking at structure and firs few entries of testing data

str(testing_data)
head(testing_data,5)

#looking store data information

store_data <- read.csv("store.csv")
str(store_data)
head(store_data,5)

#looking at sample_submission 

sample_submission <- read.csv("sample_submission.csv")
str(sample_submission)
head(sample_submission,5)

#Preprocessing with training data

#attaching the data sets to be used to directly refer to their column names

attach(training_data)
attach(testing_data)
attach(store_data)

#converting Store, DayOfWeek, Open, Promo, SchoolHoliday variables into factor 
#variables as it does not make sense to treat them as integers in further analysis.

#factorization with training data
training_data$Store <- as.factor(training_data$Store)
training_data$DayOfWeek <- as.factor(training_data$DayOfWeek)
training_data$Open <- as.factor(training_data$Open)
training_data$Promo <- as.factor(training_data$Promo)
training_data$SchoolHoliday <- as.factor(training_data$SchoolHoliday)

#factorization with testing data set
testing_data$Id <- as.factor(testing_data$Id)
testing_data$Store <- as.factor(testing_data$Store)
testing_data$DayOfWeek <- as.factor(testing_data$DayOfWeek)
testing_data$Open <- as.factor(testing_data$Open)
testing_data$Promo <- as.factor(testing_data$Promo)
testing_data$SchoolHoliday <- as.factor(testing_data$SchoolHoliday)

#factorization with store data
store_data$Store <- as.factor(store_data$Store)
store_data$CompetitionDistance <- as.numeric(store_data$CompetitionDistance)
store_data$CompetitionOpenSinceMonth <- as.factor(store_data$CompetitionOpenSinceMonth)
store_data$CompetitionOpenSinceYear <- as.factor(store_data$CompetitionOpenSinceYear)
store_data$Promo2 <- as.factor(store_data$Promo2)
store_data$Promo2SinceWeek <- as.factor(store_data$Promo2SinceWeek)
store_data$Promo2SinceYear <- as.factor(store_data$Promo2SinceYear)

#Factorization ends for all datasets, just for checking if indeed has been done,
#let's check the structure of all three datasets
str(training_data)
str(testing_data)
str(store_data)

#Now we split date column of both testing and training datasets into three separate columns
#for day, month and year respectively, for increasing our predictors for better learning from
#data
training_data$Date <- as.Date(training_data$Date)

training_data <- within(training_data, {
  year <- as.integer(format(training_data$Date, "%Y"))
  month <- as.integer(format(training_data$Date, "%m"))
  day <- as.integer(format(training_data$Date, "%d"))
})

testing_data$Date <- as.Date(testing_data$Date)

testing_data <- within(testing_data, {
  year <- as.integer(format(testing_data$Date, "%Y"))
  month <- as.integer(format(testing_data$Date, "%m"))
  day <- as.integer(format(testing_data$Date, "%d"))
})

#Also we remove the id feature from testing data as it is not telling anything other than
#serving as notation for (store,date) duplet. Along with Id feature, we now, drop down 
#date variable as have already captured it's information separately in day, month and year respectively
testing_data <- within(testing_data,rm(Id,Date))
#Now, we also drop Date column from training_data for similar reasons as was the case with
#testing data
training_data <- within(training_data,rm(Date))

