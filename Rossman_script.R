#setting the working directory to be home directory prior to beginning 
setwd("~/")

#reading the training data
training_data <- read.csv("train.csv")

#looking at structure and first few entries of training data
str(training_data)
dim(training_data)
head(training_data)

#reading the testing data
testing_data <- read.csv("test.csv")

#looking at structure and first few entries of testing data
str(testing_data)
dim(testing_data)
head(testing_data)

#looking store data information
store_data <- read.csv("store.csv")
str(store_data)
dim(store_data)
head(store_data,5)

#looking at sample_submission 
sample_submission <- read.csv("sample_submission.csv")
str(sample_submission)
dim(sample_submission)
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
store_data$Promo2 <- as.factor(store_data$Promo2)
store_data$PromoInterval <- as.factor(store_data$PromoInterval)


#Factorization ends for all datasets, just checking if indeed has been done,
#let's check the structure of all three datasets
str(training_data)
str(testing_data)
str(store_data)

#Note : We can see that there are no customers in testing dataset
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

#we now, drop down date variable as have already captured it's information separately in day, month and year respectively
testing_data <- within(testing_data,rm(Date))
#Now, we also drop Date column from training_data for similar reasons as was the case with testing data
training_data <- within(training_data,rm(Date))

#getting descriptive stats for both training and testing datasets
summary(training_data)
summary(testing_data) 
#Important Observations
#Observation:1 We see here that testing data does not have representation of b and c factor levels in
#the StateHoliday variable.
#Observation 2: Test data has 11 NA's under the Open variable where as training data has none
#Now, let us check out the stores where these NA's are occuring
testing_data[is.na(Open),]
#We see that missing data for Open is only at Store number 622.
#Since we have missing data here, we want to account for this missing data and fill these 
#NA values with some meaningful replacement. So, let us checkout on what all days do the Store 622
#open
subset(testing_data,(testing_data$Store == 622 & testing_data$Open == 1))
#So we see that except on Day 7, which happens to be a Sunday,Store 622 is open on all days.
#Now, let us also check Store 622 data in the training set
subset(training_data,(training_data$Store == 622))
#So, we see here in training data too, that except Sunday, in most cases
#Store 622 has been open on all days of week.
#So, it makes sense to impute the NA's for Store 622 by putting 1 under Open variable in place of NA
testing_data$Open[is.na(testing_data$Open)] <- 1
#Now, let us checkback if indeed we have now finished the imputations and removed all NA's
subset(testing_data,(testing_data$Store == 622))
#Now, we go for merging the data
training_data <- merge(training_data,store_data)
testing_data <- merge(testing_data,store_data)
#After merging the data, let us look at the structure and description of data once again.
str(training_data)
str(testing_data)
dim(training_data)
dim(testing_data)
summary(training_data)
summary(testing_data)
str(testing_data)

#Removing the days from training set when the stores were closed since when stores were closed,
#there cannot be any sales
closed_days <- subset(training_data,(training_data$Open == 0))
sum(closed_days$Sales)
#The above information shows that on days when stores were closed, there was no sales.So now,
#we got to remove this data from training data, i.e. we got to keep only that part of data
#for which we have open days
no_open_days <- dim(training_data)[1] - dim(closed_days)[1]
training_data <- subset(training_data,(training_data$Open == 1))
#We remove closed days similarly for test dataset also.
testing_data <- subset(testing_data,(testing_data$Open == 1))
dim(testing_data)
#Let us check the descriptive statistics after removing closed days sets from both testing 
#and training data
summary(training_data)
summary(testing_data)
#Now, we again do see a lot of NA's in four variables namely CompetitionDistance,
#CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2SinceWeek and Promo2SinceYear
#So, let us see them one by one and handle the missing values in both testing and training data
subset(training_data,(is.na(training_data$CompetitionDistance)))
subset(training_data,(is.na(training_data$CompetitionOpenSinceMonth)))
subset(training_data,(is.na(training_data$CompetitionOpenSinceYear)))
subset(training_data,(is.na(training_data$Promo2SinceWeek)))
subset(training_data,(is.na(training_data$Promo2SinceYear)))
if(!require(dplyr)){
  install.packages("dplyr")
}
library(dplyr)
#checking when all the columns having missing values are simultaneously NA's.
simulatneous_nas  <- filter(training_data,is.na(CompetitionDistance),is.na(CompetitionOpenSinceMonth),is.na(CompetitionOpenSinceYear),is.na(Promo2SinceWeek),is.na(Promo2SinceYear))
#For visualising and imputation of missing data, we install VIM package
#and load the VIM package
if(!require(VIM)){
  install.packages("VIM")
}
library(VIM)
#pulling out the documentation for VIM package
help(package = "VIM")

#getting aggregates for missing data
aggr_train <- aggr(training_data)
aggr_test <- aggr(testing_data)
#creating visuals for missing data in train set
miss_train <- select(training_data,CompetitionDistance,CompetitionOpenSinceMonth,CompetitionOpenSinceYear,Promo2SinceWeek,Promo2SinceYear)
barMiss(miss_train)

#creating visuals for missing data in test sets
miss_test <- select(testing_data,CompetitionDistance,CompetitionOpenSinceMonth,CompetitionOpenSinceYear,Promo2SinceWeek,Promo2SinceYear)
barMiss(miss_test)
#Now we will employ various techniques from here on wards for imputation and
#check our results from time to time.
#Imputing for missing data by performing majority imputation
if(!require(imputeR)){
  install.packages("imputeR")
}
library(imputeR)
#performing majority imputation on train data
major(training_data$CompetitionDistance)
major(training_data$CompetitionOpenSinceYear)
major(training_data$CompetitionOpenSinceMonth)
major(training_data$Promo2SinceWeek)
major(training_data$Promo2SinceYear)
#performing imputation on test data
major(testing_data$CompetitionDistance)
major(testing_data$CompetitionOpenSinceYear)
major(testing_data$CompetitionOpenSinceMonth)
major(testing_data$Promo2SinceWeek)
major(testing_data$Promo2SinceYear)
#checking correlation between customers and sales (remember, going by intuition, there
#indeed be a strong positive correlation)
if(!require(stats)){
  install.packages("stats")
}
library(stats)
cor(training_data$Customers,training_data$Sales)
#So, this means we can do away with Customers data in our model building phase.
#In simple language it means, predicting sales for test data or number of
#customers is more or less the same thing.

#Model Building Stage
training_data <- within(training_data,rm(Customers))
test_data <- within(testing_data,rm(Id))
#Trying regression tree
if(!require(rpart)){
  install.packages("rpart")
}
library(rpart)
reg_tree <- rpart(Sales ~ .,data = training_data,method = "anova")
reg_pred <- predict(reg_tree,test_data)
testing_data$Sales <- reg_pred
final_output <- select(testing_data,Id,Sales)
