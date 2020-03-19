# Airbnb Booking Predictions


This project aims at **predicting travel destinations for Airbnb’s first-time users**, with the ultimate goal of tailoring advertising strategies to different users. 

Due to highly unbalanced data, the **analysis was split into three steps** 

- whether the user booked a trip or not from their account 
- whether the booking is to the US or abroad and 
- which country is more likely to be booked

We performed a range of models from **logistic regression** to **boosted trees** with cross validation as well as an **ensemble** of these. 

Additionally, given the use case for our predictions, i.e. ads on the Airbnb homepage, we developed an impactful model that **predicts multiple countries per user**, in most cases two or three. For 87% of the new users, we are able to predict the true destination in the list of likely countries and provide the certainty around that prediction.

This analysis equips us with the following insights. Firstly, during the analysis on whether a customer will book or not, we found that reaching the Airbnb website via Facebook increases the chances of booking. This points us to investigating whether investment in Facebook advertising is more effective than elsewhere. Secondly, whether a user will book abroad is highly impacted by their language and browser setting. Italian speakers being more inclined to book abroad than Chinese speakers. Lastly, the multi-country analysis allows ranking ads on the company’s landing page. And the certainty around each prediction helps prioritize the advertising budget across users and order the ads shown to each user by their probabilities.
Overall, this project addressed the difficulty of predicting the first country of booking. While the likelihood of booking could cut some advertising costs on the users unlikely to book, the multi-class predictions are expected to bring the biggest value when deciding which ads to show.

