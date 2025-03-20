#free memory
rm(list=ls())
gc()

#importation des modules
#install.packages('tidyverse')
#install.packages('caret')
#install.packages('dummies')

# Load necessary libraries
library(tidyverse)
library(readxl)
library(dplyr) 
library(tidyr)
library(corrplot)
library(lmridge)
library(glmnet)
library(dummies)
library(caret)

#Seed
set.seed(1)

########
#DATA IMPORTATION
########

# Load the dataset and check its structure
data <- read_csv("Z:/Presentation/Code Files/sm.csv")
str(data)

########
#DATA PREPERATION
########

# Convert columns to appropriate data types
data$Year <- as.integer(as.factor(data$Year))
data$Area <- as.factor(data$Area)

# Convert other columns to numeric format
data[-c(1,2)] <- lapply(data[-c(1,2)], as.numeric)

# To create dummy variables if wanted
#data_dummies <- dummy.data.frame(data, names = "Area")


# Remove rows with NA in the predicted column
imputation <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      median_value <- median(df[[i]], na.rm = TRUE)
      print(median_value)
      df[[i]][is.na(df[[i]])] <- median_value
    }
  }
  return(df)
}

# Apply the imputation function to your dataset
data <- imputation(data)

# Add any necessary variable transformations or additional variables
# For example, can create interaction terms, polynomial terms, or perform log transformation if necessary

# Check the summary of the cleaned data
summary(data)

# Drop Area column from train and test sets
data <- select(data, -Area)

split_and_normalize_data <- function(data) {
  # Split the data into train and test sets
  train_index <- createDataPartition(data$Agri_Production_Value, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Normalize the train data and save the mean and standard deviation
  train_data_norm <- scale(train_data[, -c(1:2)], center = TRUE, scale = TRUE)
  train_mean <- attr(train_data_norm, "scaled:center")
  train_sd <- attr(train_data_norm, "scaled:scale")
  train_data_norm <- as.data.frame(train_data_norm)
  
  # Apply the same normalization to the test data
  test_data_norm <- scale(test_data[, -c(1:2)], center = train_mean, scale = train_sd)
  test_data_norm <- as.data.frame(test_data_norm)
  
  # Return a list of the train and test sets, and the mean and standard deviation
  return(list(train_data_norm = train_data_norm, test_data_norm = test_data_norm, train_mean = train_mean, train_sd = train_sd))
}



########
#FIRST MODEL: Linear Regression
########

#We apply the pre-processing step
result <- split_and_normalize_data(data)
train_data_norm <- result$train_data_norm
test_data_norm <- result$test_data_norm
train_mean <- result$train_mean
train_sd <- result$train_sd

linear_regression <- function(train_data_norm, test_data_norm, train_mean, train_sd) {
  # Perform linear regression on the updated train set
  lm_model <- lm(Agri_Production_Value ~ ., data = train_data_norm)
  
  # Predict on the updated test set using normalized coefficients
  predictions <- predict(lm_model, newdata = test_data_norm)
  
  # De-normalize the predictions for reporting
  predictions_unormalized <- (predictions * train_sd[length(train_sd)] + train_mean[length(train_mean)])
  
  # Model Summary
  sm = summary(lm_model)
  print(sm)
  
  # Calculate R-squared of the prediction on test data
  mean_Y_test_norm <- mean(test_data_norm$Agri_Production_Value)
  SSE_test <- sum((predictions - test_data_norm$Agri_Production_Value)^2)
  SST_test <- sum((test_data_norm$Agri_Production_Value - mean_Y_test_norm)^2)
  r2_test <- 1 - SSE_test / SST_test
  cat("Multiple R-squared (test data): ", r2_test, "\n")
  
  # Calculate mean prediction error on test data normalized
  mean_error <- mean((predictions - test_data_norm$Agri_Production_Value)^2)
  cat("Mean error (test data): ", mean_error, "\n")
  
  # Residual Plot
  e <- lm_model$residuals
  plot(train_data_norm[,length(train_sd)], e, ylab="Residuals", xlab=names(train_data_norm)[length(train_sd)])
  abline(h=0, col='red')
  
  # Return the linear regression model object and predictions
  return(list(lm_model = lm_model, predictions = predictions_unormalized))
}

result <- linear_regression(train_data_norm, test_data_norm, train_mean, train_sd)
lm_model <- result$lm_model
predictions_unnormalized <- result$predictions


###
#MODEL SELECTION 1 - AIC
###

# This function returns the data frame without the variables eliminated by the backward AIC algorithm
aic_backward <- function(df) {
  model = lm(Agri_Production_Value~., data=df)
  model_minimize_aic = step(model, data=df, direction="backward")
  df <- model_minimize_aic$model
  return(df)
}

# We call the aic_backward function and store the new data frame
AIC_agri_data <- aic_backward(data)

# Concatenate Agri_Production_Value variable with reduced data frame
AIC_agri_data <- cbind(AIC_agri_data, Agri_Production_Value = data$Agri_Production_Value)

# We perform a new linear regression this time without the outliers and without the variables that increase AIC
# We process the data frame and apply the linear modeling function
result_AIC <- split_and_normalize_data(AIC_agri_data)
train_data_norm_AIC <- result_AIC$train_data_norm
test_data_norm_AIC <- result_AIC$test_data_norm
train_mean_AIC <- result_AIC$train_mean
train_sd_AIC <- result_AIC$train_sd

result_AIC <- linear_regression(train_data_norm_AIC, test_data_norm_AIC, train_mean_AIC, train_sd_AIC)
lm_model_AIC <- result_AIC$lm_model
predictions_unnormalized_AIC <- result_AIC$predictions


###
#MODEL SELECTION 2 - STUDENT
###

# Removes non-significant variables (according to the Student test) in backward
student <- function(df, alpha) {
  # We repeat until we return the data frame
  while (TRUE) {
    # we perform the model
    result <- split_and_normalize_data(df)
    train_data_norm <- result$train_data_norm
    test_data_norm <- result$test_data_norm
    train_mean <- result$train_mean
    train_sd <- result$train_sd
    model <- lm(Agri_Production_Value~., data=train_data_norm)
    sm <- summary(model)
    
    # we extract the list of coefficients associated with each variable of the model
    coeff_p = sm$coefficients[,4][-1] #-1 to remove the coefficient of the constant from the list
    # We retrieve the maximum of these coefficients
    max_p = max(coeff_p)
    if (max_p <= alpha) { # If the max is lower than the fixed alpha threshold then we don't remove any variable
      return(df)
    }
    # Otherwise we retrieve the column name and delete it then start again
    column_to_delete = names(which.max(coeff_p))
    df[column_to_delete] <- NULL
  }
}

# We apply on the previous data frame, with an alpha = 0.05 to retrieve the data frame without the variables
# that have a t-value greater than the fixed alpha threshold
Student_AIC_agri_data = student(AIC_agri_data, 0.05)

# We perform a new linear regression
# We process the data frame and apply the linear modeling function
result_AIC_Student <- split_and_normalize_data(Student_AIC_agri_data)
train_data_norm_AIC_Student <- result_AIC_Student$train_data_norm
test_data_norm_AIC_Student <- result_AIC_Student$test_data_norm
train_mean_AIC_Student <- result_AIC_Student$train_mean
train_sd_AIC_Student <- result_AIC_Student$train_sd

result_AIC_Student <- linear_regression(train_data_norm_AIC_Student, test_data_norm_AIC_Student, train_mean_AIC_Student, train_sd_AIC_Student)
lm_model_AIC_Student <- result_AIC_Student$lm_model
predictions_unnormalized_AIC <- result_AIC_Student$predictions

###
#MODEL SELECTION 3 - COLINEARITY/VIF
###

# Function that displays the correlation matrix of the variables as well as the VIF values of the variables
correlation <- function(df) {
  df[,1] = NULL # We remove the Y variable
  # Calculation and display of the correlation matrix
  corr_df = cor(df)
  corrplot(corr_df**2, tl.col = "black", tl.srt=45, method = "number", type="upper", diag=FALSE)
  
  # Calculation and display of the VIF
  inv_corr_df = solve(corr_df)
  vif = diag(inv_corr_df)
  vif[vif<4] <- NA
  print(vif)
}

# we apply the correlation function on the previous data set
Student_AIC_agri_data <- Student_AIC_agri_data[, -1]
correlation(Student_AIC_agri_data)

# After VIF, we decided to remove Humid_tropical_forest_in_ha from Student_AIC_agri_data          
# Drop Area column from train and test sets
data_VIF <- select(Student_AIC_agri_data, -Humid_tropical_forest_in_ha)
correlation(data_VIF)
# Then we decided to remove Emissions_N2O_Forest_fires
data_VIF <- select(data_VIF, -Emissions_N2O_Fires_in_humid_tropical_forests)
correlation(data_VIF)
# Then we decided to remove Closed_shrubland_in_ha
data_VIF <- select(data_VIF, -Closed_shrubland_in_ha)
correlation(data_VIF)
# Then we decided to remove Emissions_N2O_Savanna_fires
data_VIF <- select(data_VIF, -Emissions_N2O_Savanna_fires)
correlation(data_VIF)
# Then we decided to remove Emissions_N2O_Forest_fires
data_VIF <- select(data_VIF, -Emissions_N2O_Forest_fires)
correlation(data_VIF)
# Then we decided to remove Grassland_in_ha
data_VIF <- select(data_VIF, -Grassland_in_ha)
correlation(data_VIF)

# We process the data frame and apply the linear modeling function
result_VIF <- split_and_normalize_data(data_VIF)
train_data_norm_VIF <- result_VIF$train_data_norm
test_data_norm_VIF <- result_VIF$test_data_norm
train_mean_VIF <- result_VIF$train_mean
train_sd_VIF <- result_VIF$train_sd

result_VIF <- linear_regression(train_data_norm_VIF, test_data_norm_VIF, train_mean_VIF, train_sd_VIF)
lm_model_VIF <- result_VIF$lm_model
predictions_unnormalized_VIF <- result_VIF$predictions

########
#OUTLIERS: Linear Regression WITHOUT outliers
########

# Define a function to calculate the leverage of each observation
leverage <- function(df) {
  df <- data.frame(df)
  df[,1] = NULL # Remove the target variable
  df <- data.matrix(df) # Convert data frame to matrix
  H <- (df %*% solve(t(df) %*% df)) %*% t(df) # Calculate the hat matrix
  H_diag <- diag(H) # Keep only the values on the diagonal
  
  # Calculate the threshold not to exceed
  threshold <- 2 * (dim(df)[2] + 1) / dim(df)[1]
  print(paste0("Threshold for leverage: ", threshold))
  
  # Plot the leverages against the observation number
  plot(H_diag, ylab = "Leverage", xlab = "Observation Number")
  # Add a horizontal line representing the threshold not to exceed
  abline(h = threshold, col = "red")
  
  # Return the values on the diagonal of H and the threshold
  return(list(H_diag, threshold))
}

# Get the diagonal values of the hat matrix and the threshold for your data frame
leverage_list <- leverage(train_data_norm_VIF)
H_diag <- data.frame(leverage_list[1])
threshold <- leverage_list[2]

# Keep only the observations with a leverage lower than or equal to the threshold
outliers_leverage <- (H_diag > threshold) # If TRUE, the observation has a leverage above the maximum authorized threshold

# Function to detect outliers using standardized and studentized residuals
standard_student <- function(df, alpha=0.1) {
  # Fit a linear model to the data frame
  model <- lm(Agri_Production_Value ~ ., data = df)
  
  # Calculate dimensions of the dataset
  n <- nrow(df)
  p <- ncol(df) - 1
  
  # Calculate standardized and studentized residuals and respective thresholds
  std_resid <- rstandard(model)
  stud_resid <- rstudent(model)
  std_thresh <- qt(1 - alpha / 2, n - p - 1) # Threshold for standardized residuals
  stud_thresh <- qt(1 - alpha / 2, n - p - 2) # Threshold for studentized residuals
  
  # Identify outliers based on standardized residuals
  std_outliers <- (std_resid < -std_thresh | std_resid > std_thresh)
  
  # Plot standardized residuals against the response variable and draw a red line at the threshold
  plot(df$Agri_Production_Value, std_resid, cex = 0.65, xlab = "Agri_Production_Value", ylab = "Standardized Residuals")
  abline(h = -std_thresh, col = 'red')
  abline(h = std_thresh, col = 'red')
  abline(h = 0)
  # Mark outliers in red
  text(df$Agri_Production_Value[std_outliers], std_resid[std_outliers], "O", col = 'red')
  
  # Identify outliers based on studentized residuals
  stud_outliers <- (stud_resid < -stud_thresh | stud_resid > stud_thresh)
  
  # Plot studentized residuals against the response variable and draw a red line at the threshold
  plot(df$Agri_Production_Value, stud_resid, cex = 0.65, xlab = "Agri_Production_Value", ylab = "Studentized Residuals")
  abline(h = -stud_thresh, col = 'red')
  abline(h = stud_thresh, col = 'red')
  abline(h = 0)
  # Mark outliers in red
  text(df$Agri_Production_Value[stud_outliers], stud_resid[stud_outliers], "O", col = 'red')
  
  # Return outliers identified by either method
  return(std_outliers | stud_outliers)
}

outliers.stand.OR.stud = standard_student(train_data_norm_VIF) # True if atypical according to standardized or studentized
outliers = (outliers_leverage | outliers.stand.OR.stud) # here all the atypical observations of the 3 methods

non.outliers = !outliers # If True then this observation is not classified as atypical by any of the above methods
# Remove all outliers
train_data_norm_VIF <- train_data_norm_VIF[non.outliers,]

# Training model without outliers
result_VIF_out <- linear_regression(train_data_norm_VIF, test_data_norm_VIF, train_mean_VIF, train_sd_VIF)
lm_model_VIF_out <- result_VIF_out$lm_model
predictions_unnormalized_VIF_out <- result_VIF_out$predictions


#------------------
#RIDGE REGRESSION
#------------------

# Function that returns the optimal lambda for the Ridge regression
ridge <- function(df, nb.group=3){ # default is 3-fold cross-validation
  df <- data.frame(df) # make sure the input data is a data frame
  n = nrow(df) # number of observations
  taille = n%/%nb.group # number of observations in each group
  # divide the data frame into nb.group
  rang = rank(runif(n))
  groupeF = (rang - 1)%/%taille + 1
  groupeF = as.factor(groupeF)
  
  lambda_to_try = seq(0.0001, 1, 0.01) # generate a sequence of numbers from 0.0001 to 1 with a step of 0.01
  m = length(lambda_to_try) # equals to 100
  
  MFOLD = matrix(lambda_to_try, m, 2) # create a matrix with m rows and 2 columns
  # 1st column for lambda to try
  # 2nd column for mean error values for 100 lambdas
  
  Err_prevF = matrix(1:nb.group, nb.group, 2) # initialize the matrix that will contain the prediction errors for each group
  
  erreurF = rep(0, nb.group) # initialize the error vector
  
  for (j in 1:m){ # loop over the different lambdas to try
    for (i in 1:nb.group){ # loop over the different created groups
      
      rid = lmridge(Agri_Production_Value ~ ., df[groupeF != i,], K = lambda_to_try[j]) # Ridge lin mod on the groups different from i
      pred = predict(rid, newdata = df[groupeF == i,]) # Predict the observations of the i-th group using the obtained model
      # mean prev err of group i
      erreurF[i] = mean((pred - df[groupeF == i,][,1])^2)
      Err_prevF[i,2] = erreurF[i]
      
    }
    # calculate the mean prediction error of each group following the k-fold procedure
    MFOLD[j,2] = mean(Err_prevF[,2])
  }
  
  # display the mean prediction error for different lambda values
  plot(MFOLD, type = "l", col = 2, xlab = "Values of lambda to try", ylab = "Mean for different lambda values", col.sub = "blue")
  
  Lambda_optF = MFOLD[which.min(MFOLD[,2]),1] # choose the lambda value that minimizes the mean error
  
  # return the optimal lambda value
  return(Lambda_optF)
}

# Determine the optimal lambda for the Ridge regression (choose cv with a 10-fold)
Lambda_optimal = ridge(train_data_norm_VIF, 10)

# Compute the Ridge regression with the optimal lambda
rid_optimal = lmridge(Agri_Production_Value ~ ., train_data_norm_VIF, K = Lambda_optimal)
print.lmridge(rid_optimal)

# Compute the mean prediction error on the Ridge test set
predtest = predict(rid_optimal, train_data_norm_VIF)
print(paste0("The mean prediction error on the test set following the Ridge regression: ", mean((predtest - train_data_norm_VIF[,1])^2)))

rstats1(rid_optimal) # Allows to return statistics related to the regression peak
  

#----------------
#Regression LASSO
#----------------
