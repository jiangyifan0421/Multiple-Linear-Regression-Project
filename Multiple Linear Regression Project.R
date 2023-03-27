library(GGally)
library(dplyr)

dataSet <- read.csv("insurance.csv", header = TRUE)

# Using only the columns I am interested in
dataSet <- dataSet[,-6]
head(dataSet)

# Convert the categorical variables into 0 or 1
temp <- if_else(dataSet$sex == "male", 1,0)
dataSet$sex <- temp
temp <- if_else(dataSet$smoker == "yes", 1,0)
dataSet$smoker <- temp
head(dataSet)

# Checking for NAs after converting
sum(is.na(dataSet$age))
sum(is.na(dataSet$bmi))
sum(is.na(dataSet$charges))
sum(is.na(dataSet$smoker))
sum(is.na(dataSet$children))


# Checking for summary statistic and correlation between the variables
summary(dataSet)
correlation_matrix <- cor(dataSet)
correlation_matrix
ggpairs(dataSet)

# linear models
linearModel <- lm(charges~., data = dataSet)
summary(linearModel)
anova(linearModel)

reducedModel<- lm(charges~age+bmi+children+smoker, data = dataSet)
anova(reducedModel, linearModel) 
summary(reducedModel)
# We fail to reject the null hypothesis(the reduced model) due to the p-value, hence the reduced model is the better fit

#Standardized residual plots
SR <- rstandard(reducedModel)
par(mfrow=c(2,2))
plot(dataSet$age, SR, xlab="age", ylab="Standardized Residuals")
plot(dataSet$bmi, SR, xlab="bmi", ylab="Standardized Residuals")
plot(dataSet$children, SR, xlab="children", ylab="Standardized Residuals")
plot(dataSet$smoker, SR, xlab="smoker", ylab="Standardized Residuals")
# The random nature of these plots is indicative that the model is a valid model for the data

plot(reducedModel)
# Leverage points = 2*(4+1)/1338 = 0.007473842
# Residual vs fitted ->> the relationship is not perfectly linear
# Normal Q-Q ->> The graph is heavly tiltted
# Scale-Location ->> shows the variance of the error is not perfectly constant.


#Added-Variable Plots
library(car)
par(mfrow=c(2,2))
avPlot(reducedModel, variable = "age", ask=FALSE)
avPlot(reducedModel, variable = "bmi", ask=FALSE)
avPlot(reducedModel, variable = "children", ask=FALSE)
avPlot(reducedModel, variable = "smoker", ask=FALSE)


vif(reducedModel)
#not greater than 5, which imply that the slopes for the predictors are estimated pretty accuratly, thus there are no significant sign of multicollinearity

#Trying a quadratic transformation
m2 = lm(charges~age+I(age^2)+bmi+I(bmi^2)+children+I(children^2)+smoker+I(smoker^2), data = dataSet)
summary(m2)
plot(m2)
# Didn't really helped



