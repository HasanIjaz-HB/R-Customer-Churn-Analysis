# R-Customer-Churn-Analysis

##Case Problem
In this case study, you will investigate a telecommunication customer churn data set to predict a
customer’s situation (churn or not).

## Data Set
Telco Customer Churn Dataset:
https://www.kaggle.com/blastchar/telco-customer-churn
The dataset is provided by IBM which has information on Telco consumers including the
following:
• Customers who left within the last month – the column is called Churn.
• Services that each customer has signed up for – phone, multiple lines, internet, online
security, online backup, device protection, tech support, and streaming TV and movies.
• Customer account information – how long they had been a customer, contract, payment
method, paperless billing, monthly charges, and total charges.
• Demographic info about customers – gender, age range, and if they have partners and
dependents.

## Methods
Data pre-processing, visualisation, Decision Tree, Validation ROC-Curves confusion Martix

## Results
After pre-processing, visualizing and cleaning the data we applied Decision Tree algorithm from
the one’s taught in class in order to predict churn status for the customers. Our initial
observations included the correlation between TotalCharges and both monthly charges and
tenure. We observed an average accuracy rate of around 79% using our decision tree model for
the predicted outcomes. The observed AUC values are high as well indicating a good
performance model. We can try using other models and pre-processing techniques if we are
hoping to find a more accurately predicting method.
