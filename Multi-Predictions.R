library(dplyr)
library(randomForest)
library(gbm)
library(caret)
library(ROCR)
library(rlist)

# Read and pre-porocess the datasets
users_train <- read.csv("nonUS_train.csv")
users_test <- read.csv("nonUS_test.csv")

#converting the signup_flow into a factor variable.
users_train$signup_flow = factor(users_train$signup_flow)
users_test$signup_flow = factor(users_test$signup_flow)

### Random Forest
set.seed(657)

users_train_model <- users_train %>% select(-c("id","date_first_booking", "booking"))

control <- trainControl(method="repeatedcv", number=10, repeats=1, search="random")


train.rf.oob <- train(country_destination ~ .,
                      na.action = na.exclude,
                      data = users_train_model,
                      method="rf",
                     # tuneGrid=data.frame(mtry = 1:ncol(users_train_model)),
                      nodesize = 25,
                      ntree = 80,
                      trControl=control)

mtry_chosen <- train.rf.oob$bestTune[[1]]
print(mtry_chosen)
model_RF = randomForest(country_destination ~ . ,
                        na.action = na.exclude,
                        data=users_train_model, 
                        ntree=120, 
                        mtry=12, 
                        nodesize=25)
importance.rf <- data.frame(imp=importance(model_RF))
importance.rf


### Evaluate the performance
users_test_model = users_test %>% select(-c("id","date_first_booking", "booking"))

#this is needed to equalize the levels between training and testing sets so we can do predictions
levels(users_test_model$signup_flow) <- levels(users_train_model$signup_flow)
levels(users_test_model$first_browser) <- levels(users_train_model$first_browser)
levels(users_test_model$language) <- levels(users_train_model$language)
levels(users_test_model$first_device_type) <- levels(users_train_model$first_device_type)



users_test_model <- rbind(users_train_model[1, ] , users_test_model)
users_test_model <- users_test_model[-1,]


for (i in 1:19) {
  if (class(users_train_model[, i]) == "factor"){
    print(levels(users_train_model[, i]))
    print(levels(users_test_model[, i]))
  } 
}



pred_RF = predict(model_RF, newdata = users_test_model)
pred_RF_train = predict(model_RF, newdata = users_train_model)


#catch the predictions that the model is >10% sure in 
pred_RF_prob = predict(model_RF, newdata = users_test_model, type = "prob")

list_predicted_countries <- list()
countries_to_go <- levels(users_test_model$country_destination)
for (i in 1:nrow(users_test_model)) {
  predicted_countries <- sort(pred_RF_prob[i, ], decreasing = TRUE)
  top_countries_sorted <- names(predicted_countries[predicted_countries >= threshold])
  list_predicted_countries <- list.append(list_predicted_countries, top_countries_sorted)
}


#accuracy
number_correctly_identified <- 0
for (i in 1:nrow(users_test_model)) {
  correctly = as.character(users_test_model$country_destination[i]) %in% list_predicted_countries[i][[1]]
  number_correctly_identified = number_correctly_identified + as.numeric(correctly)
}

smart_accuracy <- number_correctly_identified/nrow(users_test_model)
smart_accuracy


get_multiclass_accuracy <- function (threshold) {
  #catch the predictions that the model is >10% sure in 
  pred_RF_prob = predict(model_RF, newdata = users_test_model, type = "prob")
  
  list_predicted_countries <- list()
  countries_to_go <- levels(users_test_model$country_destination)
  for (i in 1:nrow(users_test_model)) {
    predicted_countries <- sort(pred_RF_prob[i, ], decreasing = TRUE)
    top_countries_sorted <- names(predicted_countries[predicted_countries >= threshold])
    list_predicted_countries <- list.append(list_predicted_countries, top_countries_sorted)
  }

  #accuracy
  number_correctly_identified <- 0
  for (i in 1:nrow(users_test_model)) {
    correctly = as.character(users_test_model$country_destination[i]) %in% list_predicted_countries[i][[1]]
    number_correctly_identified = number_correctly_identified + as.numeric(correctly)
  }
  
  smart_accuracy <- number_correctly_identified/nrow(users_test_model)
  print(smart_accuracy)
  list_predicted_countries
}

list_predicted_countries <- get_multiclass_accuracy(0.05)

#histogram of how many country we predict
country_count <- c()
for (i in 1:nrow(users_test_model)) {
  country_count <- c(country_count, length(list_predicted_countries[i][[1]]))
}

ggplot() +
  geom_histogram(aes(x = country_count), bins = 30, binwidth = 0.7, fill = "#640706") +
  theme_minimal() +
  xlab("Number of countries predicted above the threshold") +
  ylab("Count")


#confusion matrix
table(pred_RF, users_test_model$country_destination)

# AUC
library(pROC)
M_in<-multiclass.roc(pred_RF_train, as.numeric(as.factor(users_train_model$country_destination)), levels=base::levels(as.factor(pred_RF_train)))
M_in$auc

M_out<- multiclass.roc(pred_RF, as.numeric(as.factor(users_test_model$country_destination)), levels=base::levels(as.factor(pred_RF)))
M_out$auc





### Boosting

model_GBM_1 = gbm(country_destination ~ .,
                  data=users_train_model, distribution = "multinomial",
                  n.minobsinnode = 50, n.trees = 100, shrinkage = 0.01, interaction.depth = 3)
influence = summary(model_GBM_1)

pred.boost.train <- predict.gbm(model_GBM_1, newdata = users_train_model, n.trees = 100, type = "response")
pred.boost <- predict.gbm(model_GBM_1, newdata = users_test_model, n.trees = 100, type = "response")

labels.in = colnames(pred.boost.train)[apply(pred.boost.train, 1, which.max)]
labels.out = colnames(pred.boost)[apply(pred.boost, 1, which.max)]

# AUC
M_in_boost<-multiclass.roc(as.factor(labels.in), as.numeric(as.factor(users_train_model$country_destination)), levels=base::levels(as.factor(labels.in)))
M_in_boost$auc

M_out_boost<- multiclass.roc(as.factor(labels.out), as.numeric(as.factor(users_test_model$country_destination)), levels=base::levels(as.factor(labels.out)))
M_out_boost$auc








