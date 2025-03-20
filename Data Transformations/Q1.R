# Importing the dataset
car <- data.frame(mtcars)

# Displaying the dataset
head(car)

# Dimensions of dataset
nrow(car)
ncol(car)

# Check structure of dataset
str(car)

library(caret)
set.seed(200)
partition <- createDataPartition(car$am, times = 1, p = 0.8, list = F)
train <- car[partition, ]
test <- car[-partition, ]

# Fitting a linear model
model <- lm(mpg ~ ., data = train)
# MSE on test set
mean((predict(model, test) - test$mpg)^2)
summary(model)
coef(model)

# Ridge Regression
# Loading the library
library(glmnet)
# Getting the independent variable
x <- model.matrix(mpg ~ ., train)[, -1]

# Getting the dependent variable
y <- train$mpg

# Setting the range of lambda values
lambda_seq <- 10^seq(5, -5, by = -0.1)

# Using cross-validation glmnet
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
plot(ridge_cv)

# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

# Using glmnet function to build the ridge regression model
fit <- glmnet(x, y, alpha = 0, lambda = best_lambda)

# Checking the model
summary(fit)

# Coefficients
coef(ridge_cv, s = "lambda.min")

# For test dataset
xx <- model.matrix(mpg ~ ., test)[, -1]
model_predict <- predict(fit, s = best_lambda, newx = xx, type = "response")

# MSE on test data
mean((model_predict - test$mpg)^2)
