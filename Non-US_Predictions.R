##############################################
## 15.071 Analytics Edge: Final Project
## US/Non-US booking prediction
##############################################

# Set working directory 
setwd(dirname(rstudioapi :: getSourceEditorContext()$path))


# Load Packages
library(caret)
library(caTools)
library(e1071)
library(gbm)
library(rpart.plot)
library(rpart)
library(randomForest)
library(ROCR)
library(tidyverse)


# load data
booked_train <- read.csv("booked_train.csv")
booked_test <- read.csv("booked_test.csv")

booked_train <- select(booked_train, -c(id, date_first_booking, booking))
booked_test <- select(booked_test, -c(id, date_first_booking, booking))

# factor destination country into US/Non-US
# 0 = US
# 1 = Non-US


booked_train$country_destination <- factor((booked_train$country_destination!="US")*1)
booked_test$country_destination <- factor((booked_test$country_destination!="US")*1)


#factor millenials and signup_flow into factor

booked_train$millenials <- factor(booked_train$millenials)
booked_test$millenials <- factor(booked_test$millenials)
booked_train$signup_flow <- factor(booked_train$signup_flow)
booked_test$signup_flow <- factor(booked_test$signup_flow)
booked_train$account_created_year <- factor(booked_train$account_created_year)
booked_test$account_created_year <- factor(booked_test$account_created_year)
booked_train$account_created_month <- factor(booked_train$account_created_month)
booked_test$account_created_month <- factor(booked_test$account_created_month)

table(is.na(booked_train$country_destination))
table(is.na(booked_test$country_destination))

# test CART to see what happens
cart_test1 = rpart(country_destination ~ ., 
                    data=booked_train, 
                    minbucket = 25,
                    cp=0.0001)
prp(cart_test1,digits = 3, varlen = 0, faclen = 0, tweak = 2)

# training ROC and AUC
predict_train_vals <- predict(cart_test1, newdata=select(booked_train, -country_destination), type="prob")[,2]
predict_train <- prediction(predict_train_vals, booked_train$country_destination)
AUC_train <- performance(predict_train, "auc")@y.values[[1]]

rocrTrain <- data.frame(
  fpr=slot(performance(predict_train, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_train, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrTrain, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# test ROC and AUC
predict_test_vals <- predict(cart_test1, newdata=select(booked_test, -country_destination), type="prob")[,2]
predict_test <- prediction(predict_test_vals, booked_test$country_destination)
AUC_test <- performance(predict_test, "auc")@y.values[[1]]

rocrtest <- data.frame(
  fpr=slot(performance(predict_test, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_test, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrtest, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# --------------
# Regular Boosting with Pre-Defined Parameters
# --------------
numeric_train <- booked_train
numeric_test <- booked_test

indx <- sapply(numeric_train, is.factor)
numeric_train[indx] <- lapply(numeric_train[indx], function(x) as.numeric(factor(x)))
numeric_test[indx] <- lapply(numeric_test[indx], function(x) as.numeric(factor(x)))
numeric_train$country_destination <- as.factor(numeric_train$country_destination -1)
numeric_test$country_destination <- as.factor(numeric_test$country_destination -1)

boost_1 <- gbm(country_destination~., data=na.omit(numeric_train), 
              n.trees=500, shrinkage=0.01, interaction.depth=2)



##### Alexandru: "GBM didn't work on my computer so I used my own code

boost_1 = gbm(country_destination ~ .,
                  data=na.omit(booked_train), distribution = "multinomial",
                  n.trees = 500, shrinkage = 0.01, interaction.depth=3)
influence = summary(boost_1)

pred_GBM_1_prob <-  predict(boost_1, newdata = booked_test, n.trees = 200, type = "response")
pred_GBM_1 <- as.numeric(pred_GBM_1_prob[, 2, 1] <= pred_GBM_1_prob[, 1, 1])

get_auc <- function(predicted_probabilities, true_data) {
  rocr.pred.rf <- prediction(predicted_probabilities, true_data)
  performance(rocr.pred.rf, "auc")@y.values[[1]]
}

get_auc(pred_GBM_1_prob[, 2, ], booked_test$country_destination)
#0.5624248

#####





summary(boost_1)

plot(boost_1, i="first_browser")

# training ROC and AUC
predict_train_vals_b1 <- predict(boost_1, n.trees=50, select(numeric_train, -country_destination), type="link")
predict_train_b1 <- prediction(predict_train_vals_b1, booked_train$country_destination)
AUC_train_b1 <- performance(predict_train_b1, "auc")@y.values[[1]]

rocrTrain_b1 <- data.frame(
  fpr=slot(performance(predict_train_b1, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_train_b1, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrTrain_b1, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# test ROC and AUC
predict_test_vals_b1 <- predict(boost_1, n.trees=5000, select(numeric_test, -country_destination), type="link")
predict_test_b1 <- prediction(predict_test_vals_b1, booked_test$country_destination)
AUC_test_b1 <- performance(predict_test_b1, "auc")@y.values[[1]]

rocrtest_b1 <- data.frame(
  fpr=slot(performance(predict_test_b1, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_test_b1, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrtest_b1, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))








# --------------------
# Random Forest
# --------------------

booked_test <- rbind(booked_train[1, ] , booked_test)
booked_test <- booked_test[-1,]


rf_1 <- randomForest(country_destination~., data=na.omit(booked_train), ntree=80,mtry=5)


# training ROC and AUC
predict_train_vals_rf1 <- predict(rf_1, na.omit(booked_train), type="prob")[,2]
predict_train_rf1 <- prediction(predict_train_vals_rf1, na.omit(booked_train)$country_destination)
AUC_train_rf1 <- performance(predict_train_rf1, "auc")@y.values[[1]]

rocrTrain_rf1 <- data.frame(
  fpr=slot(performance(predict_train_rf1, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_train_rf1, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrTrain_rf1, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# test ROC and AUC
predict_test_vals_rf1 <- predict(rf_1, na.omit(booked_test), type="prob")[,2]
predict_test_rf1 <- prediction(predict_test_vals_rf1, na.omit(booked_test)$country_destination)
AUC_test_rf1 <- performance(predict_test_rf1, "auc")@y.values[[1]]
#0.5387806

rocrtest_rf1 <- data.frame(
  fpr=slot(performance(predict_test_rf1, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_test_rf1, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrtest_rf1, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))




rf_cv_1 <- train(x = na.omit(booked_train) %>% select(-country_destination),
                 y = na.omit(booked_train)$country_destination,
                 method="rf",
                 tuneGrid=data.frame(mtry=1:10),
                 ntree=500,
                 trControl=trainControl(method="cv", number=10))

best.mtry <- rf_cv_1$bestTune[[1]]

# best.mtry = 1
rf_FIN <- randomForest(country_destination~., data=na.omit(booked_train), ntree=500,mtry=best.mtry)


# training ROC and AUC
predict_train_vals_rfFIN <- predict(rf_FIN, na.omit(booked_train), type="prob")[,2]
predict_train_rfFIN <- prediction(predict_train_vals_rfFIN, na.omit(booked_train)$country_destination)
AUC_train_rfFIN <- performance(predict_train_rfFIN, "auc")@y.values[[1]]

rocrTrain_rfFIN <- data.frame(
  fpr=slot(performance(predict_train_rfFIN, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_train_rfFIN, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrTrain_rfFIN, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# test ROC and AUC
predict_test_vals_rfFIN <- predict(rf_FIN, na.omit(booked_test), type="prob")[,2]
predict_test_rfFIN <- prediction(predict_test_vals_rfFIN, na.omit(booked_test)$country_destination)
AUC_test_rfFIN <- performance(predict_test_rfFIN, "auc")@y.values[[1]]
#0.5234609

rocrtest_rfFIN <- data.frame(
  fpr=slot(performance(predict_test_rfFIN, "tpr", "fpr"), "x.values")[[1]],
  tpr=slot(performance(predict_test_rfFIN, "tpr", "fpr"), "y.values")[[1]]
)
ggplot(rocrtest_rfFIN, aes(x=fpr)) + 
  geom_line(aes(y=tpr), lwd=1) + 
  geom_segment(x=0, y=0, xend=0, yend=1, color = "blue") +
  geom_segment(x=0, y=1, xend=1, yend=1, color = "blue") +
  geom_segment(x=0, y=0, xend=1, yend=1, color = "red") + 
  labs(x="False Positive Rate", y="True Positive Rate", title ="ROC Curve") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
