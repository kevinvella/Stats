# Load the dataset into R
data <- read.csv("concrete_data.csv")

# Perform correlation analysis
correlation_matrix <- cor(data)

# Identify the response variable and explanatory variables
response_variable <- names(correlation_matrix)[ncol(correlation_matrix)]
explanatory_variables <- names(correlation_matrix)[1:(ncol(correlation_matrix) - 1)]

# Print the response variable and explanatory variables
print(response_variable)
print(explanatory_variables)

###########
y <- concrete_data$`Concrete compressive strength (MPa)`
X <- concrete_data[, -ncol(concrete_data)]

install.packages("caret")
library(glmnet)
library(caret)

test = data$Concrete.compressive.strength.MPa..megapascals..

# Step 3: Split the dataset into training and validation sets
set.seed(123)  # Set a seed for reproducibility
trainIndex <- createDataPartition(data$Concrete.compressive.strength.MPa..megapascals.., p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]  # Training set
validData <- data[-trainIndex, ]  # Validation set

# Step 4: Fit Ridge regression model
ridgeModel <- glmnet(as.matrix(trainData[, -c("ResponseVariable")]), trainData$Concrete.compressive.strength.MPa..megapascals.., alpha = 0)

# Step 5: Fit LASSO regression model
lassoModel <- glmnet(as.matrix(trainData[, -c("ResponseVariable")]), trainData$Concrete.compressive.strength.MPa..megapascals.., alpha = 1)

# Step 6: Evaluate the models
ridgePred <- predict(ridgeModel, as.matrix(validData[, -c("ResponseVariable")]))
lassoPred <- predict(lassoModel, as.matrix(validData[, -c("ResponseVariable")]))
ridgeMSE <- mean((ridgePred - validData$Concrete.compressive.strength.MPa..megapascals..)^2)
lassoMSE <- mean((lassoPred - validData$Concrete.compressive.strength.MPa..megapascals..)^2)

# Step 7: Compare the models
if (ridgeMSE < lassoMSE) {
  bestModel <- "Ridge Regression"
} else {
  bestModel <- "LASSO Regression"
}

# Step 8: Check the bias in parameter estimates
ridgeCoeff <- coef(ridgeModel)
lassoCoeff <- coef(lassoModel)

ridgeBias <- sum(ridgeCoeff != 0)
lassoBias <- sum(lassoCoeff != 0)

# Step 9: Print the results
print(paste("Best Model:", bestModel))
print(paste("Ridge Bias:", ridgeBias))
print(paste("LASSO Bias:", lassoBias))