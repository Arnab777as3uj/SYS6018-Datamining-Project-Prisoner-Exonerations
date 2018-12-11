library(tidyverse)
#install.packages('xgboost')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(data.table)
#install.packages('mlr')
library(mlr)

df <- read.csv("cleanex.csv",header=TRUE)
df <- df[-c(1)] # Dropping rows column
summary(df)

##### Data Cleaning

# Adding the target CTE("Conviction to Exoneration) time(in years)-
df$CTE <- df$Exonerated - df$Convicted

# Dropping Exonerated column-
df <- df[!names(df) %in% c('Exonerated')]

# Dropping the Names columns as we are not looking for relation between Names and CTE
df = df[!names(df) %in% c('Last.Name', 'First.Name')]

# Converting Occurred and Convicted to categorical variables
df$Occurred <- as.factor(df$Occurred)
df$Convicted <- as.factor(df$Convicted)

##### Data Splitting

# Let's divide the data into 75:25 train and test set.
set.seed(7)
i = sample(nrow(df), nrow(df)*0.75)

train <- df[i,]
test <- df[-i,]

write.csv(train,"train.csv", row.names = FALSE )
write.csv(test,"test.csv", row.names = FALSE )

#convert data frame to data table
setDT(train)
setDT(test)

#check missing values
table(is.na(train))
# FALSE
# 54784
table(is.na(test))
# FALSE
# 18176

# Data has no missing/NA values

# Converting categorical variables using hot encoding

train_y <- train$CTE
test_y <- test$CTE

new_tr <- model.matrix(~.+0,data = train[,-c("CTE"),with=F])
new_ts <- model.matrix(~.+0,data = test[,-c("CTE"),with=F])

# we'll use xgb.DMatrix to convert data table into a matrix
dtrain <- xgb.DMatrix(data = new_tr,label = train_y)
dtest <- xgb.DMatrix(data = new_ts,label= test_y)

##### Model 1

# Tuning the parameters of xgboost model : Parameter 1
params1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.01, gamma=0.5, max_depth=4, min_child_weight=2, subsample=0.8, colsample_bytree=0.7,scale_pos_weight = 1)

# Cross validation using xgboost on training data
xgbcv <- xgb.cv( params = params1, data = dtrain, nrounds = 3000, nfold = 10, showsd = T, stratified = T, print_every_n = 50, early_stopping_rounds = 10, maximize = F)
# Best iteration:
#   [1463]	train-rmse:4.086295+0.027094	test-rmse:5.433083+0.297614

# CV-Train-MSE
4.086295^2
# [1] 16.69781

# CV-Test-MSE 
5.433083^2
# [1] 29.51839

# Initial Model training
xgb1 <- xgb.train(params = params1, data = dtrain, nrounds = 1500*(10/9), watchlist = list(val=dtest,train=dtrain), print_every_n = 50, early_stopping_rounds = 10, maximize = F , eval_metric = "rmse")

# Model prediction for train set
xgb_trainfit<-predict(xgb1,dtrain)

# Train MSE
train.MSE <- mean((xgb_trainfit - train_y)^2)
train.MSE
# [1] 16.07458

xgb_train_R2<-cor(train_y,xgb_trainfit)^2  
xgb_train_R2
# [1] 0.812392

# Model prediction for test set
xgbpred <- predict(xgb1,dtest)

# Variable Importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])
# IO , NC , DNA and Worse.Crime.Display - Murder,Occurred1971 seem best 5 variables for prediction by this XGBoost model

test.MSE <- mean((xgbpred - test_y)^2)
test.MSE
# [1] 32.52627

xgb_test_R2<-cor(test_y,xgbpred)^2  
xgb_test_R2
# [1] 0.6061408

# This model seems to be overfitting, let's try another model with slightly different parameters

##### Model 2

params2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.005, gamma=0.5, max_depth=4, min_child_weight=2, subsample=0.8, colsample_bytree=0.7,scale_pos_weight = 1)

# Cross validation using xgboost on training data
xgbcv2 <- xgb.cv( params = params2, data = dtrain, nrounds = 6000, nfold = 10, showsd = T, stratified = T, print_every_n = 50, early_stopping_rounds = 20, maximize = F)
# Best iteration:
#   [4469]	train-rmse:3.643108+0.033370	test-rmse:5.356785+0.339462

# CV-Train-MSE
3.643108^2 
# [1] 13.27224

# CV-Test-MSE 
5.356785^2 
# [1] 28.69515

# Model training, nrounds has to be increased as considering entire train set
xgb2 <- xgb.train(params = params2, data = dtrain, nrounds = 4500*(10/9), watchlist = list(val=dtest,train=dtrain), print_every_n = 50, early_stopping_rounds = 20, maximize = F , eval_metric = "rmse")

# Model prediction for train set
xgb2_trainfit<-predict(xgb2,dtrain)

# Train MSE
train.MSE <- mean((xgb2_trainfit - train_y)^2)
train.MSE 
# [1] 13.10527

xgb2_train_R2<-cor(train_y,xgb2_trainfit)^2  
xgb2_train_R2
# [1] 0.8469109

# Model prediction for test set
xgbpred2 <- predict(xgb2,dtest)

# Variable Importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])
# IO , NC , DNA and Worse.Crime.Display - Murder,Occurred1971 seem best 5 variables for prediction by this XGBoost model

test.MSE2 <- mean((xgbpred2 - test_y)^2)
test.MSE2
# [1] 31.89231

xgb2_test_R2<-cor(test_y,xgbpred2)^2  
xgb2_test_R2
# [1] 0.6137864