# Author - Romil Gupta
# Course - Statistical Learning, BDA, Hyderabad

# Data set used here is Concrete_Data.xls and we have converted it to csv format for easier usage

# Import and understand the data. Look at the range of the various attributes
setwd ("E:/Data Science/Assignments/Statistics/25-Dec-17")
Concrete_Data <- read.csv('Concrete_Data.csv',header = TRUE)
View(Concrete_Data)
str(Concrete_Data)

# Making the name of heading shorter

names(Concrete_Data) <- c('cement_component','blast_furnace_slag','fly_ash','water','superplasticizer', 'coarse_aggregate','fine_aggregate','age','compressive_strength')

# Print out the summary statistics of all variables and arrange them in a neat tabular format.

View(summary(Concrete_Data))

# Creating a test data

set.seed(1)
test <- sample(nrow(Concrete_Data),200)

#Create Scatter Plot of Compressive Strength versus the other variables, taking each predictive variable at a time. Clearly label the graphs.

plot(Concrete_Data$compressive_strength , Concrete_Data$cement_component)
plot(Concrete_Data$compressive_strength , Concrete_Data$blast_furnace_slag)
plot(Concrete_Data$compressive_strength , Concrete_Data$fly_ash)
plot(Concrete_Data$compressive_strength , Concrete_Data$water)
plot(Concrete_Data$compressive_strength , Concrete_Data$superplasticizer)
plot(Concrete_Data$compressive_strength , Concrete_Data$coarse_aggregate)
plot(Concrete_Data$compressive_strength , Concrete_Data$fine_aggregate)
plot(Concrete_Data$compressive_strength , Concrete_Data$age)

# Produce pairwise correlation coefficient table. Comment on the values of the correlations between each predictor and response. Do you think there is any pairwise correlation between predictors which may cause worry?

cor(Concrete_Data$compressive_strength , Concrete_Data$cement_component)
cor(Concrete_Data$compressive_strength , Concrete_Data$blast_furnace_slag)
cor(Concrete_Data$compressive_strength , Concrete_Data$fly_ash)
cor(Concrete_Data$compressive_strength , Concrete_Data$water)
cor(Concrete_Data$compressive_strength , Concrete_Data$superplasticizer)
cor(Concrete_Data$compressive_strength , Concrete_Data$coarse_aggregate)
cor(Concrete_Data$compressive_strength , Concrete_Data$fine_aggregate)
cor(Concrete_Data$compressive_strength , Concrete_Data$age)

# As per our analysis,  pairwise correlation between predictors is not a  cause worry

cor(Concrete_Data)

#Build Multiple Linear Regression model of Compressive Strength on ALL the predictors. Report multiple R2 of the model.

lm.model <- lm(compressive_strength~.,data=Concrete_Data)
summary(lm.model)

#Sum of square

lm.modelsum<- sum((lm.model$residuals)^2)
lm.modelsum

# As we can see the p-values of the t-statistic of all the coefficients are close to zero except for that of coarse_aggregate and fine_aggregate
# If we look at summary data, after p-value there is dot for coarse_aggregate and fine_aggregate
# Usually it means probablity between 0.5 and 0.10 and we do not include these in mode

# This indicates that other than the coefficients of coarse_aggregate and fine_aggregate, all the other coefficients of the model are significant for the accuracy of the fit
# Removing the coarse_aggregate and fine_aggregate attributes from the model

lm.model2 <- lm(compressive_strength~.-coarse_aggregate-fine_aggregate,data=Concrete_Data)
summary(lm.model2)

#Sum of square

lm.modelsum2<- sum((lm.model2$residuals)^2)
lm.modelsum

# Removal of "coarse_aggregate" and "fine_aggregate" attributes doesn't improve the R-squared value of the model and the mean squared error of the model  data doesn't decrease
# Therefore we will go ahead with the first model, lm.model
# Residual Standard Error of lm.model

sqrt(deviance(lm.model)/lm.model$df.residual)

# Mean response of the dataset

mean(Concrete_Data$compressive_strength)

# The residual standard error of lm.model is around 10.340and the mean response is about 36.00.
# The error rate is as following.
# Error rate

10.40/36*100

28.88

# R Squared value

summary(lm.model)$r.squared

# The R-squared value of lm.model is 0.61. This indicates that 61% of the variability in the response has been explained by the model.
# In our model multicollinearity is not there, as there is no independent variable which are highly co related
# There s no definate cut off but we are considering any correlation greater than or less than 0.7 is cause for concern.
# So we will sticking to model 1

#################################################################

# Predicting The Model

predict_strength = predict(lm.model,Concrete_Data[test,])
mean((Concrete_Data[test,]$compressive_strength-predict_strength)^2)
str(Concrete_Data)

# Mean Squared Error of the lm.model2 on test data

predict_strength2 = predict(lm.model2,Concrete_Data[test,])
mean((Concrete_Data[test,]$compressive_strength-predict_strength2)^2)

#Correlation between response in the test data and the response of lm.model

cor(Concrete_Data[test,]$compressive_strength,predict(lm.model,Concrete_Data[test,]))

# Comparison of mean squared error between training data and test data
# Mean squared error of lm.model on training data

mean((Concrete_Data[-test,]$compressive_strength-predict(lm.model,Concrete_Data[-test,]))^2)

# Mean squared error of lm.model on test data

mean((Concrete_Data[test,]$compressive_strength-predict(lm.model,Concrete_Data[test,]))^2)

# Testing a model on a single new data

new_data <- data.frame(cement_component=310,
                        blast_furnace_slag=0,
                        fly_ash=0,
                        water=150,
                        superplasticizer=5,
                        coarse_aggregate=1047,
                        fine_aggregate=676,
                        age=28)

predict(lm.model,new_data,interval="confidence")

#############################################################################################
####################End of Assignment##################