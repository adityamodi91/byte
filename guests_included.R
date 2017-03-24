# guests_included prediction

# Prediction of price variable
library(ranger)
library(dplyr)
library(caTools)
library(xgboost)
library(ggplot2)
library(e1071)
library(rpart)

# create Extra_people data frame equals to trainExpanded and remove price and guests_included
guest_include=trainExpanded
guest_include$extra_people=NULL
guest_include$price=NULL

#Converting guests_included to factor
guest_include$guests_included=as.factor(guest_include$guests_included)

#Dividing data into test and training set
set.seed(123)
split = sample.split(guest_include$guests_included, SplitRatio = 0.7)
guests_training_setExtended = subset(guest_include, split == TRUE)
guests_test_setExtended = subset(guest_include, split == FALSE)



#random Forest
random_forest_extra <- ranger(guests_included ~ ., data = guests_training_setExtended,
                              num.trees = 1000, num.threads = 12,
                              importance = 'impurity', classification = TRUE)
rf_predictions_guests<- predict(random_forest_extra, guests_test_setExtended)
rf_error <- error2(rf_predictions_guests$predictions, guests_test_setExtended$guests_included)
accuracy_rf <- accuracy(rf_predictions_guests$predictions, guests_test_setExtended$guests_included)


# SVM on guests_included
TragetCol <- which(colnames(guests_training_setExtended) == "guests_included")


svm_model <- svm(guests_included~ ., method="class", data = guests_training_setExtended)
guests_test_setExtended_svm=subset(guests_test_setExtended, select = -guests_included)
pred_svm <- predict(svm_model,guests_test_setExtended_svm)
error_svm <- error2(pred_svm, guests_test_setExtended$guests_included)
accuarcy_svm <- accuracy(pred_svm, guests_test_setExtended$guests_included)

#I tried SVM for parameter tuning but my machine was taking a lot of time, i waited for so long but code didn't excuted

#XGBoost
#First create model for classsification and check error and accuracy
model_class<- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                 label = guests_training_setExtended[,TragetCol],
                 params =list(
                   booster="gbtree",
                   eta=0.4,
                   max_depth=500,
                   subsample=0.7,
                   colsample_bytree=1,
                   objective="multi:softmax",
                   num_class=18
                 ), nrounds=60,
                 nthread=8)
out_class <- predict(model_class, as.matrix( guests_test_setExtended[,-TragetCol]))
error_xgboodeost_ml<- error2(out_class, guests_test_setExtended$guests_included )
accuracy <- accuracy(out_class, guests_test_setExtended$guests_included)
print(error_xgboodeost_ml)
print(accuracy)


#create model for regression and check error and accuracy
model_reg <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                 label = guests_training_setExtended[,TragetCol],
                 params =list(
                   booster="gbtree",
                   eta=0.2,
                   max_depth=200,
                   subsample=0.8,
                   colsample_bytree=0.2,
                   objective="reg:linear"
                 ), nrounds=500,
                 nthread=6)
out <- predict(model_reg, as.matrix( guests_test_setExtended[,-TragetCol]))
error_xgboodeost_ml<- error2(out, guests_test_setExtended$guests_included )
accuracy <- accuracy(out, guests_test_setExtended$guests_included)
print(error_xgboodeost_ml)
print(accuracy)

#here i checked that classification is working better than regression model so i choose to go ahead with classification


# parameter tunning , calculate error for different values of parameter and choose best
max_depth <- sapply(c(100, 200, 500, 1000, 2000), function(x)
{
  model <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                   label = guests_training_setExtended[,TragetCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=x,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="multi:softmax", 
                     num_class = 18
                   ), nrounds=50,
                   nthread=6)
  out <- predict(model, as.matrix(guests_test_setExtended[,-TragetCol]))
  error_xgboost_model<- error2(out, guests_test_setExtended$guests_included )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

eta_new <- sapply(c(0.1, 0.2, 0.4, 0.6, 0.8), function(x)
{  
  model <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                   label = guests_training_setExtended[,TragetCol],
                   params =list(
                     booster="gbtree",
                     eta=x,
                     max_depth=500,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="multi:softmax", 
                     num_class = 18
                   ), nrounds=50,
                   nthread=6)
  out <- predict(model, as.matrix(guests_test_setExtended[,-TragetCol]))
  error_xgboost_model<- error2(out, guests_test_setExtended$guests_included )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

SubSample <- sapply(c(0.2, 0.7, 0.4, 0.6, 0.8), function(x)
{  
  model <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                   label = guests_training_setExtended[,TragetCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=500,
                     subsample=x,
                     colsample_bytree=1,
                     objective="multi:softmax", 
                     num_class = 18
                   ), nrounds=50,
                   nthread=6)
  out <- predict(model, as.matrix( guests_test_setExtended[,-TragetCol]))
  error_xgboost_model<- error2(out, guests_test_setExtended$guests_included )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

Rounds <- sapply(c(500, 800, 1000, 300, 1500), function(x)
{  
  model <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                   label = guests_training_setExtended[,TragetCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=500,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="multi:softmax", 
                     num_class = 18
                   ), nrounds=x,
                   nthread=6)
  out <- predict(model, as.matrix(guests_test_setExtended[,-TragetCol]))
  error_xgboost_model<- error2(out, guests_test_setExtended$guests_included )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

ByTree <- sapply(c(0.2,0.4, 0.6, 0.8, 1, 1.2), function(x)
{  
  model <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                   label = guests_training_setExtended[,TragetCol],
                   params =list(
                     booster="gbtree",
                     eta=0.1,
                     max_depth=1000,
                     subsample=0.8,
                     colsample_bytree=x,
                     objective="multi:softmax", 
                     num_class = 18
                   ), nrounds=50,
                   nthread=6)
  out <- predict(model, as.matrix( guests_test_setExtended[,-TragetCol]))
  error_xgboost_model<- error2(out, guests_test_setExtended$guests_included )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

#Final Model for Classification after parameter Tunning
model_class <- xgboost(as.matrix(guests_training_setExtended[,-TragetCol]),
                       label = guests_training_setExtended[,TragetCol],
                       params =list(
                         booster="gbtree",
                         eta=0.1,
                         max_depth=500,
                         subsample=0.4,
                         colsample_bytree=1,
                         objective="multi:softmax", 
                         num_class = 18
                       ), nrounds=300,
                       nthread=6)
out_class <- predict(model_class, as.matrix( guests_test_setExtended[,-TragetCol]))
error_xgboodeost_ml<- error2(out_class, guests_test_setExtended$guests_included )
accuracy <- accuracy(out_class, guests_test_setExtended$guests_included)
print(error_xgboodeost_ml)
print(accuracy)

#Graphs of Model
guests_test_setExtended$Level = seq(1:nrow(guests_test_setExtended))
ggplot() +
  geom_point(aes(x = guests_test_setExtended$Level, y = guests_test_setExtended[,TragetCol]),
             colour = 'red') +
  geom_point(aes(x = guests_test_setExtended$Level, y = out_class),
             colour = 'blue') +
  ggtitle('Guests Included of Dataset actual vs Predicted all points') +
  xlab('Level') +
  ylab('Guests Included') 

ggplot() +
  geom_point(aes(x = guests_test_setExtended$Level, y = sort(guests_test_setExtended[,TragetCol])),
             colour = 'red') +
  geom_point(aes(x = guests_test_setExtended$Level, y = sort(out_class)),
             colour = 'blue') +
  ggtitle('Guests Included of DataSet actual vs predicted sorted ') +
  xlab('Level') +
  ylab('Guests Included')
