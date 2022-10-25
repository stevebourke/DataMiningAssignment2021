#Import the readxl package and place the bean data into dataframe

#install.packages("readxl")
library(readxl)

dfbean <- read_excel("data/Dry_Bean_Dataset.xlsx")

set.seed(12345)

#c50 package has been imported - call it from library
library(C50)

#Examine the data we got from our file
str(dfbean)

#Looking at the numbers of our dependent variable
table(dfbean$Class)
prop.table(table(dfbean$Class))

#Shuffle the 13611 elements in the data set
dfbean_random = dfbean[order(runif(13611)),]

#Split it into training and test data, at row 11500
dfbean_train <- dfbean_random[1:11500,]
dfbean_test <- dfbean_random[11501:13611,]

#Check the proportion of class variable in each set
prop.table(table(dfbean_train$Class))
prop.table(table(dfbean_test$Class))

#Build the model using the training set created above
#Make sure that class of bean is recognised as a factor, needed for c50
bean_model <- C5.0(as.factor(Class) ~ .,data = dfbean_train)

#Examine the model
summary(bean_model)
plot(bean_model)

#Use the model to predict the class of bean in our test data
bean_predictions <- predict(bean_model, dfbean_test)


#Package gmodels has already been imported
#Call it from library
library(gmodels)


#Use crosstable function to easily view predicted vs actual results
#for the class of bean
CrossTable(bean_predictions, dfbean_test$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted class', 'actual class'))


#Try again, this time boosting the model, using 10 trials
bean_model_boost <- C5.0(as.factor(Class) ~ .,data = dfbean_train, trials=10)

#Examine the model
summary(bean_model_boost)
plot(bean_model_boost)

#Use the model to predict the class of bean in our test data
bean_predictions_boost <- predict(bean_model_boost, dfbean_test)


#Use crosstable function to easily view predicted vs actual results
#for the class of bean
CrossTable(bean_predictions_boost, dfbean_test$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted class', 'actual class'))
