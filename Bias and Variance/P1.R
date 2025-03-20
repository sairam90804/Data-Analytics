library(data.table)
library(readr)
library(caret)
library(corrplot)


abaloneURl = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
ab_data = fread(abaloneURl, header = FALSE) 
abaloneHeader = c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Rings")
colnames(ab_data) = abaloneHeader

#Use of attribute names
attach(ab_data)

#Returns the indices when case is true
case = which(ab_data$Sex!="I")

#Rows with Sex = I are removed
ab_data2 = ab_data[case]

#Converting categorical Sex to number
ab_data2$Sex = factor(ab_data2$Sex)

#Create a 80-20 Train-Test Split, createDataPartition() returns the indices
train = createDataPartition(y = ab_data2$Sex, p = 0.8, list = FALSE)

#Training data
trainData = ab_data2[train,]

#Testing data (note the minus sign)
testData = ab_data2[-train,]    

#Predicting Sex using glm
model <- glm(Sex~.,family=binomial,data=trainData)

#Summary
summary(model)

#Coefficients
coef(model)

#Confidence Interval
confint(model)

#By setting the parameter type='response', R will output probabilities in the form of P(y=1|X)]
probs = predict(model, testData, type = "response")

#probabilities > 0.5 are Males and rest are Females 
resultSet = ifelse(probs > 0.5,"M","F")
resultSet2 = factor(resultSet)

#confusion matrix
confusionMatrix(resultSet2, testData$Sex)

#ROC Curve
library(ROCR)
roc.pred = prediction(probs,testData$Sex)
roc.perf = performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(0,1)
auc.perf = performance(roc.pred, measure = "auc")
cat("Area Under the Curve: ")
auc.perf@y.values

#correlations between the predictors
cm = cor(ab_data2[,-1])
corrplot(cm, method = "number")
