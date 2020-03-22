### Load packages
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggvis)
library(caret)

### Read in datasets
country <- read.csv("countries.csv")
train_users <- read.csv("train_users_2.csv")
users <- read.csv("age_gender_bkts.csv")


### Data Cleaning
data <-subset(train_users, age <=100 & age >=0)
data <-na.omit(train_users) 

# Add age description columns

# age quantile
for (i in 1:nrow(data)){
  if (data$age[i] < 21)  data$age_quantil[i] = "yonger than 20"
  if (data$age[i] >= 21 & data$age[i] < 31)  data$age_quantil[i] = "20-30"
  if (data$age[i] >= 31 & data$age[i] < 41)  data$age_quantil[i] = "30-40"
  if (data$age[i] >= 41 & data$age[i] < 51)  data$age_quantil[i] = "40-50"
  if (data$age[i] >= 51 & data$age[i] < 61)  data$age_quantil[i] = "50-60"
  if (data$age[i] >= 60)  data$age_quantil[i] = "older than 60"
}

# age generation
for (i in 1:nrow(data)){
  if (data$age[i] >25 & data$age[i] < 40)  data$age_generation[i] = "Millennials"
  else data$age_generation[i] = "Non-millennials"
}

# millenials
for (i in 1:nrow(data)){
  if (data$age_generation[i] == "Millennials") data$millenials[i] = 1
  if (data$age_generation[i] == "Non-millennials") data$millenials[i] = 0}

# Add booking binary
for (i in 1:nrow(data)) {
  if(data$country_destination[i] == "NDF")  data$booking[i] = 0
  if(data$country_destination[i] != "NDF")  data$booking[i] = 1
}


#colapsing the device type into three categories: Desktop, Smartphone, Tablet or Other
data$device_type_colapsed <- "0"

data$device_type_colapsed[which(data$first_device_type %in% 
                                  c("Android Phone", "iPhone", "SmartPhone (Other)"))] <- "1"

data$device_type_colapsed[which(data$first_device_type %in% 
                                  c("iPad", "Android Tablet"))] <- "2"

data$device_type_colapsed[which(data$first_device_type %in% 
                                  c("Desktop (Other)", "Mac Desktop", "Windows Desktop"))] <- "3"

data$device_type_colapsed <- factor(data$device_type_colapsed)
levels(data$device_type_colapsed) <- list(Other = "0", Smartphone="1", Tablet="2", Desktop="3")



# Add language mismatch column----TBD



### Saving the transformed data, splitting in into training and testing sets
write.csv(data, "users_transformed_full.csv")

set.seed(657)
split = createDataPartition(data$country_destination, p = 0.70, list = FALSE)
data_train = data[split,]
data_test = data[-split,]

write.csv(data_train, "users_train.csv", row.names = FALSE)
write.csv(data_test, "users_test.csv", row.names = FALSE)



### Exploratory analysis and data visulization

# Gender
ggplot(data, aes(gender)) +
  geom_bar()

# Age
ggplot(data, aes(age)) +
  geom_histogram()

ggplot(data, aes(age_quantil))+
  geom_bar()

ggplot(data, aes(age_generation))+
  geom_bar()

lan <- table(data$language)
lab <- paste(names(lan), "\n", lan, sep="")
pie(lan, labels = lab,
    main="Pie Chart of Species\n (with sample sizes)")

# language
ggplot(data, aes(language)) +
  geom_bar()
table(data$language)

# country destination
country <- as.data.frame.matrix(country) 

lab <- paste(names(lan), "\n", lan, sep="")
pie(lan, labels = lab,
    main="Pie Chart of Species\n (with sample sizes)")

# booked countries
data %>% 
  filter(country_destination != 'NDF') %>% 
  ggplot() + 
  aes(country_destination) + 
  geom_bar()+
  coord_flip()

booked<-data %>% 
  filter(country_destination != 'NDF')
booked_country <- table(booked$country_destination)
lab <- paste(names(booked_country), "\n", booked_country, sep="")
pie(booked_country, labels = lab,
    main="Pie Chart of Species\n (with sample sizes)")

#looking at the age-device relationship 
data[which(!is.na(data$age) & !is.na(data$device_type_colapsed)),] %>%
  group_by(device_type_colapsed) %>%
  ggvis(~age) %>% layer_densities(stroke = ~device_type_colapsed, fill := "#aaaaaa", 
                                  strokeWidth := 3) %>% 
  add_axis("x", title = "Age", 
           properties = axis_props(grid = list(stroke = "white"))) %>%
  add_axis("y", title = "", value = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
           properties = axis_props(grid = list(stroke = "white")))




