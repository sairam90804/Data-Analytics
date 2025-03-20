#Data Import
library(data.table) 
#Naive Bayes
library(e1071)     

#URL for data import
mushroomURL = "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroomData = fread(mushroomURL,header=FALSE)

#Add headers
headers = c("Class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")
colnames(mushroomData) = headers

#Class Distribution
table(mushroomData$Class)

#Converting Class Attribute to a factor
mushroomData$Class = factor(mushroomData$Class)

#Dimensions
dim(mushroomData)

#Structure 
str(mushroomData)

#Finding the number of missing values
cat("Number of missing values = ",sum(mushroomData=="?"))

#New dataset with removed missing values
mushroomData2 = mushroomData[mushroomData$`stalk-root`!="?"]

#Split the data
train_dimension = floor(0.80*nrow(mushroomData2))
train_index = sample(nrow(mushroomData2), size = train_dimension)
trainData = mushroomData2[train_index,]
testData = mushroomData2[-train_index,]

#Naive Bayes 
model = naiveBayes(trainData[,-1],trainData$Class)

#Testing data prediction
test_pred = predict(model,testData[,-1])

#Training data prediction 
train_pred = predict(model,trainData[,-1])

#Testing Model accuracy
cat("Accuracy of Testing Model: ",mean(test_pred == testData$Class)*100,"%")

#Accuracy of Training Model
cat("Accuracy of Training Model: ",mean(train_pred == trainData$Class)*100,"%")

#Confusion Matrix
table(test_pred, testData$Class)
