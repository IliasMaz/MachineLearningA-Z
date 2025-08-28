#Multiple Linear Regression

#Importing the dataset
dataset = read.csv('50_Startups.csv')

#Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))
#Splitting the data set into Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the Multiple LR into training set
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)

regressor = lm(formula = Profit ~ .,
               data = training_set)

#predicting the test set results

y_pred = predict(regressor, newdata = test_set)

#By using only the R.D.Spend and Linear Regression we get the same results.

#Building the optimal model using Backward Elimination
#step1
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)

summary(regressor)
#step2
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)

summary(regressor)
#step3
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)

summary(regressor)
#step4
regressor = lm(formula = Profit ~ R.D.Spend ,
               data = dataset)

summary(regressor)