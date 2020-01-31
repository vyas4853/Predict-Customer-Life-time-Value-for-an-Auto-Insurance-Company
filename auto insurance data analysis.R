library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(e1071)

setwd("G:\\R Language\\R project")

data<- read.csv("Auto insurance.csv")
str(data)
summary(data)
boxplot(data$Customer_Lifetime_Value)
quantile(data$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

data <- data[data$Customer_Lifetime_Value <15500, ]
boxplot(data$Customer_Lifetime_Value)


## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))

data <- na.omit(data)
str(data)
nrow(data)
names(data)

set.seed(123)
test<- sample(1:nrow(data), (nrow(data)/4))
test_data<- data[test, ]
training_data<-data[-test, ]


fit <- lm(Customer_Lifetime_Value ~ 	Coverage + I(EmploymentStatus == "Unemployed") +	
          	Income +	I(Marital_Status =="Single") +	Monthly_Premium_Auto +	
            Months_Since_Policy_Inception +	Number_of_Open_Complaints +	Number_of_Policies+ 
            Renew_Offer_Type+	I(Vehicle_Class=="Sports Car") + I(Vehicle_Class=="SUV"), data=training_data)
summary(fit)

vif(fit)

fit <- lm(Customer_Lifetime_Value ~ 	Coverage +	Income +	I(Marital_Status =="Single") +	Monthly_Premium_Auto +	
            Months_Since_Policy_Inception +	Number_of_Open_Complaints +	Number_of_Policies+ 
            Renew_Offer_Type+	I(Vehicle_Class=="Sports Car") + I(Vehicle_Class=="SUV"), data=training_data)
summary(fit)


str(training_data)
training_data$Customer_Lifetime_Value<- as.integer(training_data$Customer_Lifetime_Value)

library(randomForest)
set.seed(123)
regressor<- randomForest(x=training_data[c(2,4,5,6,8:24)], y=training_data$Customer_Lifetime_Value, ntree=11)

test_data$pred<- predict(regressor, test_data)
View(test_data[c(3,25)])
(sum((abs(test_data$Customer_Lifetime_Value- test_data$pred))/test_data$Customer_Lifetime_Value))/nrow(test_data)
##MAPE is approximately equal to 4% that is sign of excellent model.