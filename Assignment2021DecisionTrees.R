#Place the bean data into dataframe

dfbean_dTree <- read.csv("data/Dry_Bean_Dataset.csv")
set.seed(12345)

#c50 package has been imported - call it from library
library(C50)

#Examine the data we got from our file
str(dfbean_dTree)

#Looking at the numbers of our dependent variable
table(dfbean_dTree$Class)
prop.table(table(dfbean_dTree$Class))

#Shuffle the 13611 elements in the data set
dfbean_random = dfbean_dTree[order(runif(13611)),]

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


#The results are a little easier to see in this table
#We can divide the diagonal by the total to find the
#percentage accuracy of this model
ctable <- table(bean_predictions, dfbean_test$Class)
ctable

#91.663%
sum(diag(ctable)) / sum(ctable)



################################################################


#Try again, this time boosting the model, using 10 trials
bean_model_boost <- C5.0(as.factor(Class) ~ .,data = dfbean_train, trials=10)

#Examine the model
summary(bean_model_boost)
#I have commented out the boosted plot as it can take a while to run
#plot(bean_model_boost)

#Use the model to predict the class of bean in our test data
bean_predictions_boost <- predict(bean_model_boost, dfbean_test)


#Use crosstable function to easily view predicted vs actual results
#for the class of bean
CrossTable(bean_predictions_boost, dfbean_test$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted class', 'actual class'))



#Check the percentage accuracy for the boosted model
ctable_boost <- table(bean_predictions_boost, dfbean_test$Class)
ctable_boost

#92.184%
sum(diag(ctable_boost)) / sum(ctable_boost)

