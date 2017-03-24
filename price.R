# Prediction of price variable
library(rpart)
library(ranger)
library(caTools)
library(dplyr)
library(caret)
library(xgboost)


#create 2 more variable price and price extanded
price=train
priceExpanded=trainExpanded

# Removing Guests_included and extra_people from price nad price_extanded
price$guests_included=NULL
price$extra_people=NULL
priceExpanded$guests_included=NULL
priceExpanded$extra_people=NULL

# split price data into train and test data
set.seed(123)
split = sample.split(price$price, SplitRatio = 0.7)
price_training_set = subset(price, split == TRUE)
price_test_set = subset(price, split == FALSE)


#split price extend data into train and test data
set.seed(123)
split = sample.split(priceExpanded$price, SplitRatio = 0.7)
price_training_setExtended = subset(priceExpanded, split == TRUE)
price_test_set_Extended = subset(priceExpanded, split == FALSE)


# linear model on train data without expantion
regressor = lm(formula = price~ .,data = price_training_set)
summary(regressor)
y_pred = predict(regressor, newdata = price_test_set)
error_lm= error(y_pred, price_test_set$price )


#linear model on price extanded data on extanded data
extand_linear_model= lm(formula = price~ .,data = price_training_setExtended)
summary(xtand_linear_model)
y_pred = predict(extand_linear_model, newdata = price_test_set_Extended )
error_linear_model<- error(y_pred, price_test_set_Extended$price )


# Prediction by decision tree
decision=rpart(formula=price ~., data = price_training_setExtended, method="anova")
pred_decision <- predict(decision, price_test_set_Extended)
error_decisiontree_model<- error(pred_decision, price_test_set_Extended$price )   #testing error
error_decisiontree_training <- error(decision$y, price_training_setExtended$price)  #training error

# Choosing important variable from decision tree
dt_importance <- as.matrix(sort(decision$variable.importance, decreasing = TRUE))
rows<-row.names(dt_importance)
new_data <- select(priceExpanded, one_of(rows))
new_data$price = train$price
# here we got new_data data.frame with important variable of decision tree

#again decison tree on this new data

split = sample.split(new_data$price, SplitRatio = 0.7)
new_training_setExtended = subset(new_data, split == TRUE)
new_test_setExtended = subset(new_data, split == FALSE)

decision_new=rpart(formula=price ~., data = new_training_setExtended, method="anova")
pred_decision_new <- predict(decision_new, new_test_setExtended)
error_decisiontree_model_new<- error(pred_decision_new, new_training_setExtended$price )   #testing error
error_decisiontree_training_new <- error(decision_new$y, new_test_setExtended$price)  #training error


#here We didn't got satishfactory results so now try random forest
#applying random forest using ranger package because it is less time consuming

random_forest <-  ranger(price ~ ., data = price_training_setExtended,
                        num.trees = 500, num.threads = 8, importance)
rf_predictions <- predict(random_forest, price_test_set_Extended)
error_rf_model<-  error(rf_predictions$predictions, price_test_set_Extended$price ) #test error
error_rf_model_train <- error(random_forest$predictions , price_training_setExtended$price)  #train error


#Taking important variables from random_forest method
rf_importance <- as.matrix(sort(random_forest_extra$variable.importance, decreasing = TRUE))
rows<-row.names(rf_importance)
rows=rows[1:100] #taking top 100 varibles  # i also tried for 200, 500, 1000 variables

#creating new data
new_data <- select(priceExpanded, one_of(rows) )
new_data$price = train$price

#again applying Random Forest on new data
split = sample.split(new_data$price, SplitRatio = 0.7)
new_training_setExtended = subset(new_data, split == TRUE)
new_test_setExtended = subset(new_data, split == FALSE)

random_forest_new <-  ranger(price ~ ., data = new_training_setExtended,
                         num.trees = 800, num.threads = 8, importance = 'impurity')   # here i also tried for different values of num.tree and threads
rf_predictions_new <- predict(random_forest, new_test_setExtended)
error_rf_model_new<-  error(rf_predictions_new$predictions, new_test_setExtended$price ) #test error
error_rf_model_train_new <- error( random_forest_new$predictions, new_training_setExtended$price)  #train error


# Finally applyong XGboost Model

priceCol <- which(colnames(price_training_setExtended) == "price")


# parameter tunning , calculate error for different values of parameter and choose best
max_depth <- sapply(c(100, 200, 500, 1000, 2000), function(x)
{
  model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                   label = price_training_setExtended[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=x,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="reg:linear"
                   ), nrounds=500,
                   nthread=6)
  out <- predict(model, as.matrix(price_test_set_Extended[,-priceCol]))
  error_xgboost_model<- error(out, price_test_set_Extended$price )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

eta_new <- sapply(c(0.1, 0.2, 0.4, 0.6, 0.8), function(x)
{  
  model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                   label = price_training_setExtended[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=x,
                     max_depth=500,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="reg:linear"
                   ), nrounds=500,
                   nthread=6)
  out <- predict(model, as.matrix(price_test_set_Extended[,-priceCol]))
  error_xgboost_model<- error(out, price_test_set_Extended$price )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

SubSample <- sapply(c(0.2, 0.7, 0.4, 0.6, 0.8), function(x)
{  
  model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                   label = price_training_setExtended[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=500,
                     subsample=x,
                     colsample_bytree=1,
                     objective="reg:linear"
                   ), nrounds=100,
                   nthread=6)
  out <- predict(model, as.matrix( price_test_set_Extended[,-priceCol]))
  error_xgboost_model<- error(out, price_test_set_Extended$price )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

Rounds <- sapply(c(500, 800, 1000, 300, 1500), function(x)
{  
  model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                   label = price_training_setExtended[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=0.4,
                     max_depth=500,
                     subsample=0.7,
                     colsample_bytree=1,
                     objective="reg:linear"
                   ), nrounds=x,
                   nthread=6)
  out <- predict(model, as.matrix(price_test_set_Extended[,-priceCol]))
  error_xgboost_model<- error(out, price_test_set_Extended$price )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

ByTree <- sapply(c(0.2,0.4, 0.6, 0.8, 1, 1.2), function(x)
{  
  model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                   label = price_training_setExtended[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=0.1,
                     max_depth=1000,
                     subsample=0.8,
                     colsample_bytree=x,
                     objective="reg:linear"
                   ), nrounds=1500,
                   nthread=6)
  out <- predict(model, as.matrix( price_test_set_Extended[,-priceCol]))
  error_xgboost_model<- error(out, price_test_set_Extended$price )
  print(error_xgboost_model)
  return(error_xgboost_model)
})

# Final model after  tunning of parameters ( i tried many values, choose this model as final)
model <- xgboost(as.matrix(price_training_setExtended[,-priceCol]),
                 label = price_training_setExtended[,priceCol],
                 params =list(
                   booster="gbtree",
                   eta=0.03,
                   max_depth=1000,
                   subsample=0.8,
                   colsample_bytree=0.8,
                   objective="reg:linear"
                 ), nrounds=1700,
                 nthread=6)
out <- predict(model, as.matrix(price_test_set_Extended[,-priceCol]))
error_xgboost_model<- error(out, price_test_set_Extended$price )
print(error_xgboost_model)




#Graphs for Testing Data
price_test_set_Extended$Level= seq(1:nrow(price_test_set_Extended))
ggplot() +
  geom_point(aes(x = price_test_set_Extended$Level, y = price_test_set_Extended[,priceCol]),
             colour = 'red') +
  geom_point(aes(x = price_test_set_Extended$Level, y = out),
             colour = 'blue') +
  ggtitle('Price of Dataset actual vs Predicted all points') +
  xlab('Level') +
  ylab('Price') 

ggplot() +
  geom_point(aes(x = price_test_set_Extended$Level, y = sort(price_test_set_Extended[,priceCol])),
             colour = 'red') +
  geom_point(aes(x = price_test_set_Extended$Level, y = sort(out)),
             colour = 'blue') +
  ggtitle('Price of DataSet actual vs predicted sorted ') +
  xlab('Level') +
  ylab('Price')

 price_test_set_Extended$Level=NULL



# Applying k-Fold Cross Validation on above model which give minimum error.
folds = createFolds(price_training_setExtended[,priceCol], k = 10)
cv = lapply(folds, function(x) {
  training_fold = price_training_setExtended[-x, ]
  test_fold = price_training_setExtended[x, ]
  model <- xgboost(as.matrix(training_fold[,-priceCol]),
                   label = training_fold[,priceCol],
                   params =list(
                     booster="gbtree",
                     eta=0.03,
                     max_depth=1000,
                     subsample=0.8,
                     colsample_bytree=0.8,
                     objective="reg:linear"
                   ), nrounds=1700,
                   nthread=6)
  out <- predict(model, as.matrix(test_fold[,-priceCol]))
  error_xgboost_model<- error(out, test_fold[,priceCol] )
  accuracy = mean(error_xgboost_model)
  print(error_xgboost_model)
  return(error_xgboost_model)
})
accuracy1 = mean(as.numeric(cv))

# I found that average error is just little higher so i choose this model


# Taking top features (i tried from 100, 500, 1000, 2000)
# and creating model and calculate error
names <-dimnames(data.matrix(price_training_setExtended[,-priceCol])[[2]])
importance_matrix <- xgb.importance(names, model = model)
rows <- importance_matrix$Feature[1:2000]
new_data <- select(priceExpanded, one_of(rows) )
new_data$price= train$price


set.seed(123)
split = sample.split(new_data$price, SplitRatio = 0.7)
price_training_set_new = subset(new_data, split == TRUE)
price_test_set_new = subset(new_data, split == FALSE)
priceCol <- which(colnames(price_training_set_new) == "price")

model <- xgboost(as.matrix(price_training_set_new[,-priceCol]),
                 label = price_training_set_new[,priceCol],
                 params =list(
                   booster="gbtree",
                   eta=0.2,
                   max_depth=1000,
                   subsample=0.8,
                   colsample_bytree=0.8,
                   objective="reg:linear"
                 ), nrounds=500,
                 nthread=6)
out <- predict(model, as.matrix(price_test_set_new[,-priceCol]))
error_xgboost_model<- error(out, price_test_set_new[,priceCol] )
print(error_xgboost_model)

# I found that error was not decreasing after topfeatures selected so i choose model with least error.                 
