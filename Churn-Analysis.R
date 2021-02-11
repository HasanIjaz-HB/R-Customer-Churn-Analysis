#Churn Analysis

getwd()
#read data
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

#get data summary and types by the following functions
summary(telco)
str(telco)

library(dplyr)
glimpse(telco)

#TotalCharges has 11 missing values as observed in the summary so we remove those rows entirely

telco <- telco[complete.cases(telco),]

#data visualisation

#for continous variables we look for distributions and check outliers
library(ggplot2)

ggplot(data = telco,aes(MonthlyCharges,color = Churn))+geom_freqpoly(binwidth = 5, size = 1)
#we can see that the number of customers with MonthlyCharges < 25 is very high => the distribution graph are similar between the people who churned and not

boxplot(telco$tenure)$out
boxplot(telco$MonthlyCharges)$out
boxplot(telco$TotalCharges)$out
#no outliers were found all values are inside whiskers

ggplot(data = telco, aes(TotalCharges, color = Churn))+ geom_freqpoly(binwidth = 150, size = 1)
ggplot(data = telco, aes(TotalCharges, color = Churn))+ geom_freqpoly(binwidth = 200, size = 1)
ggplot(data = telco, aes(TotalCharges, color = Churn))+ geom_freqpoly(binwidth = 250, size = 1)
#the distribution shows a high positive skew regardless of the churn status (binwidth test = 150,200,250)

ggplot(data = telco, aes(tenure, colour = Churn))+geom_freqpoly(binwidth = 5, size = 1)
#the distribution is very different for the customers who churned and who did not . The skew is high and positive for customers who churned indicating 
#that they are likely to end the subscription in early months. If customers don't churn intially we can see a huge rise in the distibution indicating their loyalty

#we look at the correlation between continous variables
telco_cor <- round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
library(corrplot)
corrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))
#the attribute totalcharges has a positive correlation with both monthly charges and tenure

#for factor variables

ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()
ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')
#we can observe that churn rate: 
#is almost same for both genders, is higher for senior citizens, is lowers for consumers who have a partner or dependent and for lines and phone we can evaluate more

ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')
#we can observe that churn rate:
#is higher for people utilizing Fiber Optics services, and another observation is that people with no online (secuirty) services have left more

#applying decision tree  on raw data
treedata<-telco
#since customerID is useless we remove it from our data
treedata$customerID <- NULL
#selecting attributes from tree data which are character and changing them to factor
treedata %>% mutate_if(is.character, as.factor) -> treedata
glimpse(treedata)
str(treedata)
#setting seed
set.seed(1)
# #partioning the dataset into training and test 70%vs30% 
# library (caret)
# trainingindex  = createDataPartition(treedata$Churn, p = 0.7, list=FALSE)
# training = treedata[trainingindex,]
# test = treedata[-trainingindex,]
# 
# #using rpart for decision tree algo
# library(rpart)
# Dtree = rpart(Churn ~., data = training, method = "class")
# summary(Dtree)
# 
# #Predicting data based on DTree
# Predicted <- predict(Dtree,data=training, type = "class")
# #comparing and checking with confusion matrix
# confusionMatrix(Predicted,test$Churn)

#above attempt failed for idk what reason below is another approach learned from internet sources

#attempt 2 with decision tree
treeslice <- sample(0:1, size= nrow(treedata), prob = c(0.75,0.25), replace = TRUE)
trainingtree <- treedata[treeslice == 0, ]
testtree <- treedata[treeslice == 1, ]
#applying the rpart algorithm on all attributes
DTree1 <- rpart(formula = Churn ~., data = trainingtree, 
                     method = "class", parms = list(split = "gini"))
#predicting data based on class and prob (inorder to produce roc curve)
predict(DTree1, data = trainingtree, type = "class") -> training_pre1
predict(DTree1, data = trainingtree, type = "prob") -> training_pro1
predict(DTree1, newdata= testtree, type = "class") -> test_pre1
predict(DTree1, newdata = testtree, type = "prob") -> test_pro1

#generating confusion Matrix and sketching roc curves
library(pROC)
confusionMatrix(data = training_pre1, reference = trainingtree$Churn)
#accuracy 0.789
traintree_actual <- ifelse(trainingtree$Churn == "Yes", 1,0)
#auc 0.793
roc <- roc(traintree_actual, training_pro1[,2], plot= TRUE, print.auc=TRUE)
#accuracy 0.794
confusionMatrix(data = test_pre1, reference = testtree$Churn)
testtree_actual <- ifelse(testtree$Churn == "Yes", 1,0)
#auc 0.801
roc <- roc(testtree_actual, test_pro1[,2], plot = TRUE, print.auc = TRUE)

#Decision Tree 2 after removing TotalCharges because it was highly correlated with 2 other attributes
treedata2 <- treedata
treedata2$TotalCharges <- NULL
treeslice2 <- sample(0:1, size= nrow(treedata2), prob = c(0.75,0.25), replace = TRUE)
trainingtree2 <- treedata2[treeslice2 == 0, ]
testtree2 <- treedata2[treeslice2 == 1, ]
#applying the rpart algorithm on all attributes
DTree2 <- rpart(formula = Churn ~., data = trainingtree2, 
                method = "class", parms = list(split = "gini"))
#predicting data based on class and prob (inorder to produce roc curve)
predict(DTree2, data = trainingtree2, type = "class") -> training_pre2
predict(DTree2, data = trainingtree2, type = "prob") -> training_pro2
predict(DTree2, newdata= testtree2, type = "class") -> test_pre2
predict(DTree2, newdata = testtree2, type = "prob") -> test_pro2

#generating confusion Matrix and sketching roc curves
library(pROC)
confusionMatrix(data = training_pre2, reference = trainingtree2$Churn)
#accuracy 0.8047
traintree_actual <- ifelse(trainingtree2$Churn == "Yes", 1,0)
#auc 0.817
roc <- roc(traintree_actual, training_pro2[,2], plot= TRUE, print.auc=TRUE)
#accuracy 0.776
confusionMatrix(data = test_pre2, reference = testtree2$Churn)
testtree_actual <- ifelse(testtree2$Churn == "Yes", 1,0)
#auc 0.798
roc <- roc(testtree_actual, test_pro2[,2], plot = TRUE, print.auc = TRUE)

