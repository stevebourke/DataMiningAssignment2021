#Open the data file into a dataframe

df1 <- read.csv("data/communities.csv")
set.seed(42)

#Need to remove columns which have lots of missing values
#I have also removed the first 5 columns which contain the names of the locations
#Perhaps I could have left state...One other column was missing one value
#so I entered in the mean value for the column in the missing field
df1new <- df1[-c(1:5,102:118,122:125, 127)]

#Having trouble plotting all variables at once, break into sets
dftest <- df1new[c(10:20,101)]
pairs(dftest)


#Build a model using all of the variables...
comm_model_all <- lm(ViolentCrimesPerPop ~ ., data = df1new)


#Adjusted R-squared is 0.6785
summary(comm_model_all)

#This will go through each variable, plotting against the output variable
#plot(comm_model_all)
#plot(ViolentCrimesPerPop ~ ., data = df1new)



#This variable has the lowest p-value in the model of all variables
comm_model_black <- lm(ViolentCrimesPerPop ~ RacePctBlack, data = df1new)
#R-squared is 0.3985
summary(comm_model_black)
plot(ViolentCrimesPerPop ~ RacePctBlack, data = df1new)
abline(comm_model_black)



#This variable has one of the lowest p-values in the model of all variables
comm_model_workMom <- lm(ViolentCrimesPerPop ~ PctWorkMom, data = df1new)
#R-squared is 0.02267
summary(comm_model_workMom)
plot(ViolentCrimesPerPop ~ PctWorkMom, data = df1new)
abline(comm_model_workMom)



#This variable has the second lowest p-value in the model of all variables
comm_model_numStreet <- lm(ViolentCrimesPerPop ~ NumStreet, data = df1new)
#R-squared is 0.1158
summary(comm_model_numStreet)
plot(ViolentCrimesPerPop ~ NumStreet, data = df1new)
abline(comm_model_numStreet)




############################################################


#Build a model using Percent under poverty line as the independent variable
#P-value in model of all was 0.004268
comm_model_pov <- lm(ViolentCrimesPerPop ~ PctPopUnderPov, data = df1new)
plot(ViolentCrimesPerPop ~ PctPopUnderPov, data = df1new)

abline(lm(ViolentCrimesPerPop ~ PctPopUnderPov, data = df1new))


#R-squared is 0.2724
summary(comm_model_pov)
plot(comm_model_pov)




#Trying out polynomial regression with poverty variable
polyvar_pov = poly(df1new$PctPopUnderPov, degree = 8)



#Poly r-squared value is 0.335 ; Linear is 0.2724
model_pov <- lm(df1new$ViolentCrimesPerPop ~ polyvar_pov, data = df1new)
summary(model_pov) 

#Use this model to predict values...
predictedCrime_pov <- predict(model_pov)

#Plot the predictive model

library(ggplot2)

df2 <- data.frame(x = df1new$PctPopUnderPov, y = predictedCrime_pov)
ggplot(df2, aes(x = df1new$PctPopUnderPov, y = predictedCrime_pov)) +
  geom_point() + geom_line()


#Compare the model curve to our original plot
plot(ViolentCrimesPerPop ~ PctPopUnderPov, data = df1new)




##################################################################


#Use percentage of kids in house with two parents as our X variable...
#P-value in model of all was 0.036019

comm_model_income <- lm(ViolentCrimesPerPop ~ PctKids2Par, data = df1new)
plot(ViolentCrimesPerPop ~ PctKids2Par, data = df1new)

abline(lm(ViolentCrimesPerPop ~ PctKids2Par, data = df1new))

#R-squared is 0.5453
summary(comm_model_income)
plot(comm_model_income)




#Trying out polynomial regression
polyvar_income = poly(df1new$PctKids2Par, degree = 5)

#R-squared has only improved to 0.5656
model_income <- lm(df1new$ViolentCrimesPerPop ~ polyvar_income, data = df1new)
summary(model_income) 

#Use this model to predict values...
predictedCrime_income <- predict(model_income)


#Plot the predictive model
df2 <- data.frame(x = df1new$PctKids2Par, y = predictedCrime_income)
ggplot(df2, aes(x = df1new$PctKids2Par, y = predictedCrime_income)) +
  geom_point() + geom_line()




#####################################################################



#Using a few variables and their squares as predictor
#P-value for PctWInvInc was 0.008791
comm_model_invest_plus <- lm(ViolentCrimesPerPop ~ PctWInvInc + PctKids2Par + PctPopUnderPov + 
                           I(PctWInvInc^2) + I(PctKids2Par^2)+ I(PctPopUnderPov^2), data = df1new)


#Adjusted R-squared is 0.5877
summary(comm_model_invest_plus)





#############################################################################


#Cross validation for PctPopUnderPov, this needs a training set and a validation set 
# get a random sample of half the length of our instances
n <- length(df1new$PctPopUnderPov)
indices <- sort(sample(1:n, round(0.5 * n)))

# training data
training.x <- df1new$PctPopUnderPov[indices]
training.y <- df1new$ViolentCrimesPerPop[indices]
training.df <- data.frame(X = training.x, Y = training.y)

# validation data is the other indices
validation.x <- df1new$PctPopUnderPov[-indices]
validation.y <- df1new$ViolentCrimesPerPop[-indices]
validation.df <- data.frame(X = validation.x, Y = validation.y)

# define the function for root mean square error
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# a dataframe to store performance data
performance <- data.frame()



for (d in 1:15)
{
  
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
  # add a row of three values, including
  # rmse of actual and predicted values of y (training data)  
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))
  
  # same for validation data
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Validation',
                                  RMSE = rmse(validation.y, predict(poly.fit,
                                                                    newdata = validation.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()
