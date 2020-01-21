rm(list=ls())
setwd("D:/R/Codes")

#Loading necessary packages
x = c("corrplot","fastDummies","randomForest")
install.packages(x)

#Reading the data
df = read.csv("day.csv", header = T)

#Printing top 5 rows
head(df,5)

#Analysing the data types and structure of the data
str(df)

#Removing instant variables as its just the index
df$instant = NULL
#Removing casual and registered as their sum is equal to count which is our dependent variable
df$casual = NULL
df$registered = NULL


#Missing value analysis
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))

#Outlier analysis
#Printing the boxplot of continuous variables
boxplot(df$temp)
boxplot(df$atemp)
boxplot(df$hum)
boxplot(df$cnt)
boxplot(df$windspeed)

cnames = c('hum','windspeed')

#Imputation of outliers with mean
for(i in cnames){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df[,i][df[,i] %in% val] = NA
  df[,i][is.na(df[,i])] = mean(df[,i], na.rm = T)
}


#Categorical box plots of categorical variables
boxplot(cnt~yr,data=df)
boxplot(cnt~season,data=df)
boxplot(cnt~mnth,data=df)
boxplot(cnt~holiday,data=df)
boxplot(cnt~weekday,data=df)
boxplot(cnt~workingday,data=df)
boxplot(cnt~weathersit,data=df)

#Regression plots for continuous variables
plot(df$cnt, df$hum) 
abline(lm(df$hum ~ df$cnt))

plot(df$cnt, df$temp) 
abline(lm(df$temp ~ df$cnt))

plot(df$cnt, df$atemp) 
abline(lm(df$atemp ~ df$cnt))

plot(df$cnt, df$windspeed) 
abline(lm(df$windspeed ~ df$cnt))

hist(df$cnt)
hist(df$hum)
hist(df$windspeed)
hist(df$temp)
hist(df$atemp)

#Feature Selection
#Multicollinearity Test
cont = c("hum", "windspeed","temp","atemp","cnt")
library(corrplot)
M = cor(df[cont])
corrplot(M, method = "circle")

#Anova test for categorical variables
cat = c('season','yr','mnth','holiday','weekday','workingday','weathersit')
for(i in cat){
  print(i)
  anova_test = aov(cnt ~ df[[i]], data = df)
  print(summary(anova_test))
}




#Removing dteday as it has no significance in our model as we already have variables for year, month etc.
df$dteday = NULL

#Remvoing atemp as it is collinear to temp
df$atemp = NULL

#Removing the below categorical variables as they failed the anova test
df[ ,c('holiday', 'weekday','workingday')] = list(NULL)

#Dummy coding for below categorical variables
library(fastDummies)
dummy_var = c('mnth','season','weathersit')
df = dummy_cols(df,select_columns = dummy_var)
df[ ,c('mnth_1', 'season_1','weathersit_1','mnth','weathersit','season')] = list(NULL)
library(dplyr)
df = df%>%select(-cnt,cnt)

#Removed the following variables after performing backward elimination
df[, c('hum','mnth_7','mnth_11','mnth_12','windspeed')] = list(NULL)

#Splitting into train and test
train_index = sample(1:nrow(df), 0.7 * nrow(df))
train = df[train_index,]
test = df[-train_index,]


#Linear regression
model_LR = lm(cnt ~ ., data = train)
predictions_LR = predict(model_LR, test[,1:15])

#Function to calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)) * 100
}

MAPE(test[,16], predictions_LR)
print(summary(model_LR)$r.squared)

#K cross validation for linear regression algorithm
library(caret)
train_control_lr = trainControl(method="cv", number=4)
model_lr = train(cnt~., data=df, trControl=train_control_lr, method="lm")
print(model_lr)

plot(test[,16], predictions_LR)

#Decistion Tree regression
library(rpart)

model_DT = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(model_DT, test[,1:15])
MAPE(test[,16], predictions_DT)
print(summary(model_DT))

#K cross validation for decision tree algorithm
train_control_dt = trainControl(method="cv", number=4)
model_dt = train(cnt~., data=df, trControl=train_control_dt, method="rpart")
print(model_dt)

plot(test[,16], predictions_DT)

#Random Forest Regression
library(randomForest)
model_RF = randomForest(cnt ~ ., train, ntree = 100)

predictions_RF = predict(model_RF, test[,1:15])
MAPE(test[,16], predictions_RF)
print(model_RF)

#K Cross Validation for random forest algorithm
train_control_rf = trainControl(method="cv", number=4)
model_rf = train(cnt~., data=df, trControl=train_control_rf, method="rf")
print(model_rf)

plot(test[,16], predictions_RF)
























