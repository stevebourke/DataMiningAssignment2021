#Import and place the bean data into dataframe


dfbean_knn <- read.csv("data/Dry_Bean_Dataset.csv")

#Set the seed for random number generation
set.seed(12345)

#Looking at the numbers of our dependent variable, class
table(dfbean_knn$Class)
prop.table(table(dfbean_knn$Class))



#Recode class column as a factor
dfbean_knn$Class <- factor(dfbean_knn$Class)


#Shuffle the 13611 elements in the data set using runif and order
dfbean_rand = dfbean_knn[order(runif(13611)),]



#Let's have a look at a few of our columns to check numeric values
summary(dfbean_rand[c("Area", "Perimeter","Extent")])


#As we can see the values for area are much higher than the others
#As a result we should normalise the data
#Create a function to do this...
normalise <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

#Use this function to normalise our numeric data (not column 17)
#We use list apply to achieve this - note we are losing our labels
dfbean_n <- as.data.frame(lapply(dfbean_rand[1:16], normalise))


#Check that it worked...
summary(dfbean_n[c("Area", "Perimeter","Extent")])



#Split it into training and test data, at row 11500
dfbean_n_train <- dfbean_n[1:11500,]
dfbean_n_test <- dfbean_n[11501:13611,]



#Create labels for the above using the class column...
dfbean_n_train_labels <- dfbean_rand[1:11500, 17]
dfbean_n_test_labels <- dfbean_rand[11501:13611, 17]


#Perform the kNN application to find our predictions
library(class)

preds = knn(train = dfbean_n_train, test = dfbean_n_test,
            cl = dfbean_n_train_labels, k = 107)


#Display a confusion matrix of predicted vs actual results
CrossTable(preds, dfbean_n_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


#The results are a little easier to see in this table
#We can divide the diagonal by the total to find the
#percentage accuracy of this model
ct <- table(preds, dfbean_n_test_labels)
ct

#91.6%
sum(diag(ct)) / sum(ct)





#############################################

#Repeat with different values of k
preds = knn(train = dfbean_n_train, test = dfbean_n_test,
            cl = dfbean_n_train_labels, k = 20)


#Display a confusion matrix of predicted vs actual results
CrossTable(preds, dfbean_n_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)




ct <- table(preds, dfbean_n_test_labels)

#92.1% - We are getting a better rate of correct predictions
#with a lower value of k
sum(diag(ct)) / sum(ct)




###############################################


#Trying with k = 1
preds = knn(train = dfbean_n_train, test = dfbean_n_test,
            cl = dfbean_n_train_labels, k = 1)


#Display a confusion matrix of predicted vs actual results
CrossTable(preds, dfbean_n_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



ct <- table(preds, dfbean_n_test_labels)
ct

#89.9% - A bit lower when k=1 but still quite a good prediction
sum(diag(ct)) / sum(ct)




#####################################


#Try out z-scaling now to see if we can improve the accuracy of our prediction
#We will go back and re-use our original randomised dataframe, dropping the class column
dfbean_z <- as.data.frame(scale(dfbean_rand[-17]))



#Check that it worked, viewing the same three columns we used above
summary(dfbean_z[c("Area", "Perimeter","Extent")])


# Again create training and test data from this dataframe
dfbean_z_train <- dfbean_z[1:11500,]
dfbean_z_test <- dfbean_z[11500:13611,]


#Create labels for the above
dfbean_z_train_labels <- dfbean_rand[1:11500, 17]
dfbean_z_test_labels <- dfbean_rand[11500:13611, 17]


#Train a model on the data
prediction <- knn(train = dfbean_z_train, test = dfbean_z_test,
                   cl = dfbean_z_train_labels, k = 107)



#Display a confusion matrix of predicted vs actual results
CrossTable(prediction, dfbean_z_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



ct <- table(prediction, dfbean_z_test_labels)
ct

#91.5% - Just a small bit worse than when using standard normalisation above
sum(diag(ct)) / sum(ct)




############################################

#With k = 20...
prediction <- knn(train = dfbean_z_train, test = dfbean_z_test,
                  cl = dfbean_z_train_labels, k = 20)



#Display a confusion matrix of predicted vs actual results
CrossTable(prediction, dfbean_z_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



ct <- table(prediction, dfbean_z_test_labels)
ct

#92.38% - Better again, and better than the value returned above when k was also 20!
sum(diag(ct)) / sum(ct)






############################################

#With k = 1...
prediction <- knn(train = dfbean_z_train, test = dfbean_z_test,
                  cl = dfbean_z_train_labels, k = 1)


#Display a confusion matrix of predicted vs actual results
CrossTable(prediction, dfbean_z_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



ct <- table(prediction, dfbean_z_test_labels)
ct

#90% , slightly better than our first result for k = 1
sum(diag(ct)) / sum(ct)




