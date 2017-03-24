#extra_people

# Prediction of price variable
library(rpart)
library(ranger)
library(dplyr)


Extra_people=trainExpanded


Extra_people$guests_included=NULL
Extra_people$price=NULL


library(caTools)
set.seed(123)
split = sample.split(Extra_people$extra_people, SplitRatio = 0.7)
extra_training_setExtended = subset(Extra_people, split == TRUE)
extra_test_set_Extended = subset(Extra_people, split == FALSE)


#Linear Model

extra_linear_model= lm(formula = extra_people~ .,
                        data = extra_training_setExtended)
extra_linear_pred = predict(extra_linear_model, newdata = extra_test_set_Extended )
error_linear_model<- error(extra_linear_pred, extra_test_set_Extended$extra_people )



#Decision Tree

decision_extra=rpart(formula = extra_people~ .,
                     data = extra_training_setExtended,  method="anova" )
pred_decision_extra <- predict(decision_extra, extra_test_set_Extended)

error_decisiontree_model_extra<- error(pred_decision_extra, extra_test_set_Extended$extra_people )

error_decisiontree_model_extra<- error(decision_extra$y, extra_training_setExtended$extra_people )


# DT after variable importance
decision_extra=rpart(formula = Extra_people~ .,
                     data = new_training_setExtended,  method="anova" )
pred_decision_extra <- predict(decision_extra, new_test_set_Extended)

error_decisiontree_model_extra<- error(pred_decision_extra, new_test_set_Extended$Extra_people )

error(decision_extra$y, new_training_setExtended$Extra_people )



dt_importance <- as.matrix(sort(decision_extra$variable.importance, decreasing = TRUE))
rows<-row.names(dt_importance)


# Random Forest

random_forest_extra <- ranger(extra_people ~ ., data = extra_training_setExtended,
                        num.trees = 500, num.threads = 8,
                        importance = 'impurity')

rf_predictions_extra <- predict(random_forest_extra, extra_test_set_Extended)

error_rf_model_extra<- error(rf_predictions_extra$predictions, extra_test_set_Extended$extra_people)

rf_importance <- as.matrix(sort(random_forest_extra$variable.importance, decreasing = TRUE))
rows<-row.names(rf_importance)
rows=rows[1:100]

Extra_people$streetw111thst=NULL
Extra_people$street28thstreet=NULL

new_data <- select(Extra_people, one_of(rows) )

new_data$Extra_people = train$extra_people

library(caTools)
set.seed(123)
split = sample.split(new_data$Extra_people, SplitRatio = 0.7)
new_training_setExtended = subset(new_data, split == TRUE)
new_test_set_Extended = subset(new_data, split == FALSE)

random_forest_extra <- ranger(Extra_people ~ ., data = new_training_setExtended,
                              num.trees = 100, num.threads = 12,
                              importance = 'impurity')

rf_predictions_extra <- predict(random_forest_extra, new_test_set_Extended)

error_rf_model_extra<- error(rf_predictions_extra$predictions, new_test_set_Extended$Extra_people)
error_rf_model_extra

error_rf_model_extra_training<- error(rf_predictions_extra$predictions, new_training_setExtended$Extra_people)
error_rf_model_extra_training 
  
  



#