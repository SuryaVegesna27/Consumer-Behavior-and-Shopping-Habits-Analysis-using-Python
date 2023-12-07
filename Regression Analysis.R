library(tidyverse)

# Load the dataset
data <- read.csv("D:\\Fall_2023 MSDA\\Statistical Foundations\\MiniProject\\Bias_correction_ucl.csv")

head(data)

# Check for missing values
summary(data)

# To check the total number of missing values in the dataset
sum(is.na(data))

colSums(is.na(data))

# Removes rows with any missing values
data <- na.omit(data)  

# Recheck for Null Values 
colSums(is.na(data))

# Checking for duplicate rows
sum(duplicated(data))

# Checking data types of each column
sapply(data, class)


#The "station" column likely represents different locations or observation points and is 
#encoded as a number. However, these numbers don't have a mathematical meaning.

#Therefore, We have treated as a categorical variable (factor in R), not a numerical one. 
#In R, We are converting it to a factor:

data$station <- as.factor(data$station)

sapply(data, class)

#We are transforming the "Date" column into more useful numerical features, 
#like extracting the year, month, and day.

# Convert the "Date" column to R's Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

data <- data[order(data$Date), ]
# Drop the original "Date" column
data <- subset(data, select = -Date)


#data$year <- as.factor(data$year)
#data$month <- as.factor(data$month)
#data$day <- as.factor(data$day)
head(data)

sapply(data, class)


# Calculating indices for splitting the data
num_rows <- nrow(data)
num_rows
train_end <- round(num_rows * 0.6)
train_end
valid_end <- round(num_rows * 0.8)
valid_end
# Splitting the data
train_data <- data[1:train_end, ]
validation_data <- data[(train_end + 1):valid_end, ]
test_data <- data[(valid_end + 1):num_rows, ]

# Displaying the number of rows in each set
cat("Training Set Rows:", nrow(train_data), "\n")
cat("Validation Set Rows:", nrow(validation_data), "\n")
cat("Testing Set Rows:", nrow(test_data), "\n")

# STEP 1
# Fit the model
model1 <- lm(Next_Tmax ~ ., data = train_data)
model1
# Make predictions on the validation set
predictions1 <- predict(model1, newdata = validation_data)

# Calculate RMSE
rmse1 <- sqrt(mean((validation_data$Next_Tmax - predictions1)^2))

# Print RMSE
print(paste("RMSE on Validation Set for Initial Model:", rmse1))
summary(model1)

plot_coeffs <- function(mlr_model) {
  coeffs <- coefficients(mlr_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Regression Coefficients")
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}
plot_coeffs(model1)

# Predictions using the initial model
predictions_initial <- predict(model1, newdata = test_data)

# Calculate RMSE for the initial model
rmse_test_initial <- sqrt(mean((test_data$Next_Tmax - predictions_initial)^2))

# Print the RMSEs
print(paste("RMSE on Test Set for Initial Model:", rmse_test_initial))

residuals = summary(model1$residuals)
#Checking Normality
qqnorm(model1$residuals)
qqline(model1$residuals, col = "steelblue")



# STEP 2
# Feature Importance
library(glmnet)

# Assuming Next_Tmax is your target variable and all other columns are predictors
x <- as.matrix(data[, -which(names(data) == "Next_Tmax")])
y <- data$Next_Tmax

# Fit the model
cv_model <- cv.glmnet(x, y, alpha = 1)  # alpha = 1 for lasso regression

# Get coefficients at the best lambda
best_lambda <- cv_model$lambda.min
coefficients_matrix <- as.matrix(coef(cv_model, s = best_lambda))

# View coefficients
print(coefficients_matrix)

coef_df <- data.frame(Feature = rownames(coefficients_matrix)[-1], 
                      Coefficient = coefficients_matrix[-1, 1])

library(ggplot2)
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +  # Flip coordinates for horizontal plot
  labs(title = "Feature Importance", x = "Features", y = "Coefficient Magnitude")



train_data1 <- train_data[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Next_Tmax")]
validation_data1 <- validation_data[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Next_Tmax")]
test_data1 <- test_data[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Next_Tmax")]

 
# Fit the model with most important Features
model2 <- lm(Next_Tmax ~ ., data = train_data1)
model2

# Make predictions on the validation set
predictions2 <- predict(model2, newdata = validation_data1)

# Calculate RMSE
rmse2 <- sqrt(mean((validation_data1$Next_Tmax - predictions2)^2))

# Print RMSE
print(rmse2)
summary(model2)



# Load necessary library
library(ggplot2)

# Create a scatter plot with a regression line
ggplot(train_data, aes(x = Present_Tmax, y = Next_Tmax)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  theme_minimal() +
  labs(title = "Regression Line with Present_Tmax as Predictor",
       x = "Present_Tmax",
       y = "Next_Tmax")




# Predictions using the improved model
predictions_improved <- predict(model2, newdata = test_data1)

# Calculate RMSE for the improved model
rmse_test_improved <- sqrt(mean((test_data1$Next_Tmax - predictions_improved)^2))

print(paste("RMSE on Test Set for Improved Model:", rmse_test_improved))
summary(model2)
plot_coeffs(model2)

#calculating the highly corelated pairs
colnames(data)
# Select the desired columns and create the dataframe
cor_df <- data[, c("Present_Tmax", "Present_Tmin", "LDAPS_RHmin", "LDAPS_RHmax", "LDAPS_Tmax_lapse", "LDAPS_Tmin_lapse", "LDAPS_WS", "LDAPS_LH", "LDAPS_CC1", "LDAPS_CC2", "LDAPS_CC3", "LDAPS_CC4", "LDAPS_PPT1", "LDAPS_PPT2", "LDAPS_PPT3", "LDAPS_PPT4", "lat", "lon", "DEM", "Slope", "Solar.radiation", "Next_Tmax", "Next_Tmin")]

# Display the first few rows of the dataframe
head(cor_df)

# Create a correlation plot
library(corrplot)
# Create a correlation matrix
correlation_matrix <- cor(cor_df)
# Display the correlation matrix
print(correlation_matrix)
corrplot(correlation_matrix, method = 'color', order = 'alphabet', tl.cex = 0.5)

# Find pairs of highly correlated features including positive or negative correlations, excluding correlations of 1
highly_correlated_pairs <- which(upper.tri(correlation_matrix, diag = TRUE) & abs(correlation_matrix) > 0.7 & abs(correlation_matrix) < 1, arr.ind = TRUE)
highly_correlated_pairs_names <- rownames(correlation_matrix)[highly_correlated_pairs[, 1]]
highly_correlated_pairs_names <- cbind(highly_correlated_pairs_names, colnames(correlation_matrix)[highly_correlated_pairs[, 2]])

# Display pairs of highly correlated features
highly_correlated_pairs_names

###Step 3

# Calculate the average cloud cover (Average_CC)
data$Average_CC <- rowMeans(data[, c("LDAPS_CC1", "LDAPS_CC2", "LDAPS_CC3", "LDAPS_CC4")], na.rm = TRUE)

# Calculate the average precipitation (Average_PPT)
#data$Average_PPT <- rowMeans(data[, c("LDAPS_PPT1", "LDAPS_PPT2", "LDAPS_PPT3", "LDAPS_PPT4")], na.rm = TRUE)

# Now let's check the structure of the updated dataframe
str(data)


# Calculating indices for splitting the data
num_rows <- nrow(data)
num_rows
train_end <- round(num_rows * 0.6)
train_end
valid_end <- round(num_rows * 0.8)
valid_end
# Splitting the data
train_data3 <- data[1:train_end, ]
validation_data3 <- data[(train_end + 1):valid_end, ]
test_data3 <- data[(valid_end + 1):num_rows, ]

train_data3 <- train_data3[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Average_CC", "Next_Tmax")]
validation_data3 <- validation_data3[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Average_CC", "Next_Tmax")]
test_data3 <- test_data3[, c("LDAPS_Tmax_lapse", "Next_Tmin", "LDAPS_PPT2", "Present_Tmax", "Slope", "LDAPS_PPT4", "LDAPS_RHmin", "station", "LDAPS_LH", "Average_CC", "Next_Tmax")]


# Fit the model with most important Features
model3 <- lm(Next_Tmax ~ ., data = train_data3)
model3

# Make predictions on the validation set
predictions3 <- predict(model3, newdata = validation_data3)

# Calculate RMSE
rmse3 <- sqrt(mean((validation_data3$Next_Tmax - predictions3)^2))

# Print RMSE
print(rmse3)
summary(model3)
# Predictions using the improved model
predictions_improved <- predict(model3, newdata = test_data3)

# Calculate RMSE for the improved model
rmse_test_improved <- sqrt(mean((test_data3$Next_Tmax - predictions_improved)^2))

print(paste("RMSE on Test Set for Improved Model:", rmse_test_improved))
residuals = summary(model3$residuals)
#Checking Normality
qqnorm(model3$residuals)
qqline(model3$residuals, col = "steelblue")
residuals = summary(model2$residuals)
#Checking Normality
qqnorm(model2$residuals)
qqline(model2$residuals, col = "steelblue")

# Assuming model2 is your linear regression model
plot(model3$fitted.values, model3$residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Adding a horizontal line at y = 0







