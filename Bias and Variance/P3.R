library(readr)
library(data.table)
library(caret)

# Load data
hyd_URL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data"
hyd_data <- fread(hyd_URL, header = FALSE)
colnames(hyd_data) <- c("longitudinalPos","prismaticCoef","LDR","BDR","LBR","froudeNo","Residuary")

# Create 80-20 training testing split
set.seed(123)
training_index <- createDataPartition(y = hyd_data$Residuary , p = 0.8, list = FALSE)
training_data <- hyd_data[training_index,]
test_data <- hyd_data[-training_index,]

# Train linear model
linearModel1 <- lm(Residuary ~ ., data = training_data)

# Define functions for metrics
mse <- function(yActual, yPred) {
  mean((yActual - yPred)^2)
}
rsquared <- function(model) {
  summary(model)$r.sq
}

# Compute metrics for training set
mse1 <- mse(training_data$Residuary, linearModel1$fitted.values)
rmse1 <- sqrt(mse1)
rsq1 <- rsquared(linearModel1)
cat("Training MSE: ", mse1, "\n")
cat("Training RMSE: ", rmse1, "\n")
cat("Training R-squared: ", rsq1, "\n")

# Train linear model using bootstrap for resampling
train_control <- trainControl(method = "boot", number = 1000)
linearModel2 <- train(Residuary ~ ., data = training_data, method = "lm", trControl = train_control)

# Compute metrics for resampled training set
mse2 <- mean(linearModel2$resample$RMSE)^2
rmse2 <- mean(linearModel2$resample$RMSE)
rsq2 <- mean(linearModel2$resample$Rsquared)
cat("Training Mean MSE (Bootstrap): ", mse2, "\n")
cat("Training Mean RMSE (Bootstrap): ", rmse2, "\n")
cat("Training Mean R-squared (Bootstrap): ", rsq2, "\n")

# Predict on test data using bootstrap model
predVals_boot <- predict(linearModel2, test_data)
mse3 <- mse(test_data$Residuary, predVals_boot)

# Compute metrics for test set
rmse3 <- sqrt(mse3)
rsq3 <- 1 - (sum((test_data$Residuary - predVals_boot)^2) / sum((test_data$Residuary - mean(test_data$Residuary))^2))
cat("Testing MSE (Bootstrap): ", mse3, "\n")
cat("Testing RMSE (Bootstrap): ", rmse3, "\n")
cat("Testing Mean R-squared (Bootstrap): ", rsq3, "\n")
