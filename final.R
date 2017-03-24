# Final training and predicting values


#removing 2 extra duplicate columns from train data
trainExpanded[,804]=NULL
#Making column order same for prediction
predictionExpanded <- predictionExpanded[colnames(trainExpanded)]

#remove dependent variable from prediction
predictionExpanded$guests_included = NULL
predictionExpanded$extra_people= NULL  
predictionExpanded$price= NULL

# Final model for price
train_new=trainExpanded
train_new$guests_included = NULL
train_new$extra_people= NULL  
priceCol <- which(colnames(train_new) == "price")  
model <- xgboost(as.matrix(train_new[,-priceCol]),
                 label = train_new[,priceCol],
                 params =list(
                   booster="gbtree",
                   eta=0.03,
                   max_depth=1000,
                   subsample=0.8,
                   colsample_bytree=0.8,
                   objective="reg:linear"
                 ), nrounds=1700,
                 nthread=6)
out <- predict(model, as.matrix(predictionExpanded))  




#Final Model for extra_people
train_new=trainExpanded
train_new$guests_included = NULL
train_new$price= NULL  
TragetCol <- which(colnames(train_new) == "extra_people") 
model_guests <- xgboost(as.matrix(train_new[,-TragetCol]),
                        
                        label = train_new[,TragetCol],
                        params =list(
                          booster="gbtree",
                          eta=0.05,
                          max_depth=300,
                          subsample=0.8,
                          colsample_bytree=0.4,
                          objective="reg:linear"
                        ), nrounds=300,
                        nthread=6)
out_xg <- predict(model_guests, as.matrix(predictionExpanded))


#Final Model for guests included
train_new=trainExpanded
train_new$extra_people = NULL
train_new$price= NULL  
TragetCol_guests <- which(colnames(train_new) == "guests_included") 
model_class <- xgboost(as.matrix(train_new[,-TragetCol_guests]),
                       label = train_new[,TragetCol_guests],
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
out_class <- predict(model_class, as.matrix(predictionExpanded))

