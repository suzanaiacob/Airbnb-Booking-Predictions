# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(users_train$date_account_created, '-', 3))
users_train['account_created_year'] = dac[,1]
users_train['account_created_month'] = dac[,2]
users_train['account_created_day'] = dac[,3]
users_train = users_train[,-c(which(colnames(users_train) %in% c('date_account_created')))]

dac = as.data.frame(str_split_fixed(users_test$date_account_created, '-', 3))
users_test['account_created_year'] = dac[,1]
users_test['account_created_year'] = dac[,2]
users_test['account_created_year'] = dac[,3]
users_test = users_test[,-c(which(colnames(users_test) %in% c('date_account_created')))]


# split timestamp_first_active in year, month and day
users_train[,'first_active_year'] = substring(as.character(users_train[,'timestamp_first_active']), 1, 4)
users_train['first_active_month'] = substring(as.character(users_train['timestamp_first_active']), 5, 6)
users_train['first_active_day'] = substring(as.character(users_train['timestamp_first_active']), 7, 8)
users_train = users_train[,-c(which(colnames(users_train) %in% c('timestamp_first_active')))]


# split timestamp_first_active in year, month and day
users_test[,'first_active_year'] = substring(as.character(users_test[,'timestamp_first_active']), 1, 4)
users_test['first_active_month'] = substring(as.character(users_test['timestamp_first_active']), 5, 6)
users_test['first_active_day'] = substring(as.character(users_test['timestamp_first_active']), 7, 8)
users_test = users_test[,-c(which(colnames(users_test) %in% c('timestamp_first_active')))]

summary(users_train$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   28.00   34.00   49.31   43.00 2014.00

users_train[users_train$age < 14 | users_train$age > 100,'age'] <- NA
