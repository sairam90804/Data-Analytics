library(ggplot2)
library(lattice)
library(caret)

# Load the dataset and rename variables
data(swiss)
swiss_data <- swiss
colnames(swiss_data)[1] <- "Fertility"

# Display the dataset
head(swiss_data)

# Split data into training and test sets
set.seed(150)
train_index <- createDataPartition(swiss_data$Fertility, p=0.8, list=FALSE)
train_data <- swiss_data[train_index,]
test_data <- swiss_data[-train_index,]

# Fit a linear regression model on the training data
lm_model <- lm(Fertility ~ ., data=train_data)
summary(lm_model)

# Calculate test mean squared error for the linear model
lm_predictions <- predict(lm_model, newdata=test_data)
lm_mse <- mean((test_data$Fertility - lm_predictions)^2)
cat("Linear Model Test MSE:", lm_mse, "\n")

# Fit a LASSO regression model on the training data
library(glmnet)
x <- model.matrix(Fertility ~ ., data=train_data)[,-1]
y <- train_data$Fertility
lambda_seq <- 10^seq(5,-5,by=-.1)
lasso_cv <- cv.glmnet(x, y, alpha=1, lambda=lambda_seq)
plot(lasso_cv)
best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x, y, alpha=1, lambda=best_lambda)
summary(lasso_model)

# Calculate test mean squared error for the LASSO model
lasso_predictions <- predict(lasso_model, newx=model.matrix(Fertility ~ ., data=test_data)[,-1], type="response")
lasso_mse <- mean((test_data$Fertility - lasso_predictions)^2)
cat("LASSO Model Test MSE:", lasso_mse, "\n")

# Print the coefficients of both models
cat("\nLinear Model Coefficients:\n")
print(coef(lm_model))
cat("\nLASSO Model Coefficients:\n")
print(coef(lasso_model))
