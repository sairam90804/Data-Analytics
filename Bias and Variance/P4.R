library(readr)
library(data.table)
library(caret)

gcd_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric"
gc_data = fread(gcd_URL, header = FALSE)

# Convert categorical variable V25 to a factor
gc_data$V25 = factor(gc_data$V25)

# Create an 80-20 training and testing split
trainIndex = createDataPartition(y = gc_data$V25, p = 0.8, list = FALSE)
trainData = gc_data[trainIndex,]
testData = gc_data[-trainIndex,] 

# Create a logistic regression model to predict V25
logisticModel1 = glm(V25 ~ ., family = binomial, data = trainData)

# Make predictions on the training data
actualVals = trainData$V25
fittedVals = ifelse(logisticModel1$fitted.values > 0.5, 2, 1)
fittedVals = factor(fittedVals)

# Generate a confusion matrix for the training data predictions
cm = confusionMatrix(fittedVals, trainData$V25)

# Print the training precision, recall, and F1 score
cat("Training Precision: ", cm$byClass[5] * 100, "%")
cat("Training Recall: ", cm$byClass[6] * 100, "%")
cat("Training F1-Score: ", cm$byClass[7] * 100, "%")

# Make predictions on the testing data
probs = predict(logisticModel1, testData, type = "response")
fittedVals_test = ifelse(probs > 0.5, 2, 1)
fittedVals_test = factor(fittedVals_test)

# Generate a confusion matrix for the testing data predictions
cm_test = confusionMatrix(fittedVals_test, testData$V25)

# Print the testing precision, recall, and F1 score
cat("Testing Precision: ", cm_test$byClass[5] * 100, "%")
cat("Testing Recall: ", cm_test$byClass[6] * 100, "%")
cat("Testing F1-Score: ", cm_test$byClass[7] * 100, "%")

# Perform 10-fold cross-validation
train.control = trainControl(method = "cv", number = 10)
logisticModel2 = train(V25 ~ ., data = trainData, method = "glm", family = "binomial", trControl = train.control)

# Make predictions on the training data with cross-validation
fittedVals_cv = ifelse(logisticModel2$finalModel$fitted.values > 0.5, 2, 1)
fittedVals_cv = factor(fittedVals_cv)

# Generate a confusion matrix for the cross-validated training data predictions
cm_cv = confusionMatrix(fittedVals_cv, trainData$V25)

# Print the training precision, recall, and F1 score with cross-validation
cat("Training Precision with 10-fold CV: ", cm_cv$byClass[5] * 100, "%")
cat("Training Recall with 10-fold CV: ", cm_cv$byClass[6] * 100, "%")
cat("Training F1-Score with 10-fold CV: ", cm_cv$byClass[7] * 100, "%")

# Make predictions on the testing data with cross-validation
probs_cv = predict(logisticModel2, testData, type = "prob")
fittedVals_cv_test = ifelse(probs > 0.5, 2, 1)
fitted
