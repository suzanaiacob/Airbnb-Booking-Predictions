library(dplyr)
library(randomForest)
library(gbm)
library(caret)
library(ROCR)

setwd(dirname(rstudioapi :: getSourceEditorContext()$path))

# Read and pre-porocess the datasets
users_train <- read.csv("users_train.csv")
users_test <- read.csv("users_test.csv")
 
#converting the signup_flow into a factor variable.
users_train$signup_flow = factor(users_train$signup_flow)
users_test$signup_flow = factor(users_test$signup_flow)

### Random Forest
set.seed(657)

users_train_model <- users_train %>% select(-c("id","date_first_booking", 
                                                    "date_account_created", "booking"))

control <- trainControl(method="repeatedcv", number=10, repeats=1, search="random")


train.rf.oob <- train(country_destination ~ .,
                      data = users_train_model,
                      method="rf",
                      tuneGrid=data.frame(mtry=1:sqrt(ncol(users_train_model))),
                      nodesize = 25, 
                      ntree = 50,
                      trControl=control)

mtry_chosen <- train.rf.oob$bestTune[[1]]
print(mtry_chosen)

model_RF = randomForest(country_destination ~ . , 
                        data=users_train_model, 
                        ntree=80, 
                        mtry=mtry_chosen, 
                        nodesize=25)
importance.rf <- data.frame(imp=importance(model_RF))
importance.rf


model_RF_LIKE_CART = randomForest(country_destination ~ . , 
                        data=users_train_model, 
                        ntree=1, 
                        mtry=16, 
                        nodesize=25)
importance.rf <- data.frame(imp=importance(model_RF_LIKE_CART))
importance.rf


### Evaluate the performance
users_test_model = users_test %>% select(-c("id","date_first_booking", 
                         "date_account_created", "booking"))

#this is needed to equalize the levels between training and testing sets so we can do predictions
levels(users_test_model$signup_flow) <- levels(users_train_model$signup_flow)
levels(users_test_model$first_browser) <- levels(users_train_model$first_browser)
levels(users_test_model$signup_flow) <- levels(users_train_model$signup_flow)

pred_RF = predict(model_RF, newdata = users_test_model)
pred_RF_train = predict(model_RF, newdata = users_train_model)

pred_RF_LIKE_CART = predict(model_RF_LIKE_CART, newdata = users_test_model)

#confusion matrix
table(pred_RF, users_test_model$country_destination)

#misclassification
#in-sample:
mean(pred_RF_train != users_train_model$country_destination)


#out-of-sample:
mean(pred_RF != users_test_model$country_destination)
mean(pred_RF_LIKE_CART != users_test_model$country_destination)



### Boosting

#simplest model
model_GBM_1 = gbm(country_destination ~ .,
                  data=users_train_model, distribution = "gaussian",
                  n.minobsinnode = 50, n.trees = 100, shrinkage = 0.001, interaction.depth = 3)
influence = summary(model_GBM_1)


#a bit more complex: more and deeper trees, but faster learning rate
model_GBM_2 = gbm(country_destination ~ .,
                  data=users_train_model, distribution = "gaussian",
                  n.minobsinnode = 25, n.trees = 500, shrinkage = 0.01, interaction.depth = 7)
influence = summary(model_GBM_2)


#the most complex model: small learning rate, lots of deep trees 
model_GBM_3 = gbm(country_destination ~ .,
                  data=users_train_model, distribution = "gaussian",
                  n.minobsinnode = 25, n.trees = 1000, shrinkage = 0.001, interaction.depth = 10)
influence = summary(model_GBM_3)




