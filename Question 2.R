data <- data.frame(
  Cement = c(540.0, 540.0, 332.5, 332.5, 198.6, 266.0, 380.0, 380.0, 266.0),
  BlastFurnaceSlag = c(0.0, 0.0, 142.5, 142.5, 132.4, 114.0, 95.0, 95.0, 114.0),
  Water = c(162.0, 162.0, 228.0, 228.0, 192.0, 228.0, 228.0, 228.0, 228.0),
  Superplasticizer = c(2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  CoarseAggregate = c(1040.0, 1055.0, 932.0, 932.0, 978.4, 932.0, 932.0, 932.0, 932.0),
  FineAggregate = c(676.0, 676.0, 594.0, 594.0, 825.5, 670.0, 594.0, 594.0, 670.0),
  Age = c(28, 28, 270, 365, 360, 90, 365, 28, 28),
  ConcreteCompressiveStrength = c(79.99, 61.89, 40.27, 41.05, 44.30, 47.03, 43.70, 36.45, 45.85)
)

# Step 2: Calculate correlation coefficients
correlation <- cor(data)

# Identify response variable
response_variable <- names(correlation)[which.max(abs(correlation[,"ConcreteCompressiveStrength"]))]

# Identify explanatory variables
explanatory_variables <- names(correlation)[abs(correlation[, "ConcreteCompressiveStrength"]) > 0.2 & names(correlation) != response_variable]

# Print the response variable and explanatory variables
print(response_variable)
print(explanatory_variables)
