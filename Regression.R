# Load the dataset
#dataset link: https://www.kaggle.com/datasets/fedesoriano/air-quality-data-set
aq_dataset <- read.csv("C:/Intelligent Systems/dataset/AirQuality.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)

# Remove unnecessary columns and the dependent variable CO(GT)
aq_dataset <- aq_dataset[, !(names(aq_dataset) %in% c("Date", "Time", "X", "X.1"))]

# Transform numeric columns that have a comma to a dot
for (col in colnames(aq_dataset)) {
  if (is.character(aq_dataset[[col]])) {
    # Replace the comma with the dot
    aq_dataset[[col]] <- gsub(",", ".", aq_dataset[[col]])
    # Convert the column to numeric
    aq_dataset[[col]] <- as.numeric(aq_dataset[[col]])
  }
}

# Replace -200 values with NA throughout the dataset
aq_dataset[aq_dataset == -200] <- NA

# Remove rows with NA values
aq_dataset <- na.omit(aq_dataset)

# Function to detect outliers using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25) # First quartile
  Q3 <- quantile(x, 0.75) # Third quartile
  IQR <- Q3 - Q1 # Interquartile range
  
  # Limits to define outliers
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Returns a boolean vector indicating if the value is an outlier
  return(x < lower_limit | x > upper_limit)
}

# Set the plot grid (3 rows and 4 columns)
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))  # Set margins to avoid overlap

# Iterate over the numeric variables to detect outliers and create box plots
for (var in colnames(aq_dataset)) {
  if (is.numeric(aq_dataset[[var]]) && var != "CO.GT.") {  # Exclude CO.GT.
    # Detect outliers for the variable
    outliers <- detect_outliers(aq_dataset[[var]])
    
    # Display the box plot for the variable
    boxplot(aq_dataset[[var]], main = var, col = "lightblue", 
            ylab = "Values", xlab = "Variable", horizontal = TRUE)
    
    # Count the number of outliers
    outlier_count <- sum(outliers)
    
    # Print the count of outliers and the anomalous values
    cat(paste("For variable", var, "there are", outlier_count, "anomalous values.\n"))
    cat("Anomalous values:\n")
    print(aq_dataset[outliers, var, drop = FALSE])
    cat("\n\n")
  }
}

# Reset the plot to default view
par(mfrow = c(1, 1))

# Create a 3x4 grid for the plots
par(mfrow = c(3, 4))

# Iterate over the numeric columns excluding 'CO.GT.'
for (col in colnames(aq_dataset)) {
  if (is.numeric(aq_dataset[[col]]) && col != "CO.GT.") {
    # Remove NA values from the current column
    cleaned_data <- na.omit(aq_dataset[[col]])
    
    # Check if there is valid data for the test
    if(length(cleaned_data) > 1) {
      # Perform the Shapiro-Wilk test
      shapiro_test <- shapiro.test(cleaned_data)
      p_value <- format(shapiro_test$p.value, digits = 2)
      print(paste("S-W Test", col, ": p-value =", p_value))
      
      # Create the QQ plot with blue points
      qqnorm(cleaned_data, main = paste("QQ Plot for", col), col = "blue")
      qqline(cleaned_data, col = "red")
    } else {
      print(paste("Column", col, "has only NA values after removal. Skipped."))
    }
  } else {
    print(paste("Column", col, "is not numeric or is 'CO.GT.'. Skipped."))
  }
}

# Reset the plot grid (set to 1x1 view)
par(mfrow = c(1, 1))

# Perform the bar chart (histogram) for 'CO.GT.'
if("CO.GT." %in% colnames(aq_dataset)) {
  # Retrieve the CO.GT. variable
  co_gt_data <- aq_dataset$CO.GT.
  
  # Remove NA values from the CO.GT. variable
  co_gt_data <- na.omit(co_gt_data)
  
  # Check if there is enough valid data for the plot
  if(length(co_gt_data) > 1) {
    # Create the histogram for CO(GT)
    hist(co_gt_data, 
         main = "Bar Distribution for CO(GT)", 
         xlab = "CO.GT. Values", 
         col = "lightblue", 
         border = "black",
         breaks = 30)  # You can change the number of bins with the 'breaks' parameter
  } else {
    print("The 'CO.GT.' variable contains only NA values or too few valid data for the plot.")
  }
} else {
  print("The 'CO.GT.' column is not present in the dataset.")
}

# Reset the plot grid (set to 1x1 view)
par(mfrow = c(1, 1))

# Run correlation matrix excluding "CO.GT."
# Select only numeric columns
numeric_data <- aq_dataset[, sapply(aq_dataset, is.numeric)]

# Calculate the correlation matrix (without "CO.GT.")
cor_matrix <- cor(numeric_data[, !(names(numeric_data) %in% "CO.GT.")], use = "complete.obs")

print("Correlation matrix (excluding CO.GT.):")
print(cor_matrix)


library(corrplot)

# View correlation matrix with customization
corrplot(cor_matrix, 
         method = "color",      
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "upper",        
         order = "hclust",      
         tl.col = "black",      
         tl.srt = 45,           
         addCoef.col = "black", 
         number.cex = 0.7,      
         diag = FALSE)   

# Check if 'CO.GT.' is present in the dataset
if("CO.GT." %in% colnames(aq_dataset)) {
  # Extract the CO.GT. variable
  co_gt_data <- aq_dataset$CO.GT.
  
  # Bin the data into intervals between 0 and 10 with an interval of 1
  co_gt_binned <- cut(co_gt_data, breaks = seq(0, 10, by = 1), right = FALSE, include.lowest = TRUE)
  
  # Calculate the frequency for each interval
  co_gt_freq <- table(co_gt_binned)
  
  # Print the results
  print(co_gt_freq)
} else {
  print("The 'CO.GT.' column is not present in the dataset.")
}

# Function to replace outliers with bounds
replace_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  # Limits for outliers
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Replace outliers with bounds
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  
  return(x)
}

par(mfrow = c(3, 4), mar = c(4, 4, 2, 1)) 

# Iterate over the numeric variables to detect outliers, replace them and create box plots
for (var in colnames(aq_dataset)) {
  if (is.numeric(aq_dataset[[var]]) && var != "CO.GT.") { 
    # Detect outliers for the variable
    outliers <- detect_outliers(aq_dataset[[var]])
    
    # Replace outliers with bounds
    aq_dataset[[var]] <- replace_outliers(aq_dataset[[var]])
    
    # Create the updated box plot for the variable (after replacing the outliers)
    boxplot(aq_dataset[[var]], main = paste(var, "- Without Outliers"), col = "lightgreen", 
            ylab = "Values", xlab = "Variable", horizontal = TRUE)
    
    # Print the number of replaced outliers
    outlier_count <- sum(outliers)
    cat(paste("For variable", var, "there were", outlier_count, "outliers replaced.\n"))
    cat("\n\n")
  }
}

par(mfrow = c(1, 1))

# Load necessary libraries
library(randomForest)
library(caret)
library(ggplot2)

# Random Forest model
rf_model <- randomForest(CO.GT. ~ ., data = aq_dataset, importance = TRUE)

# Variable importance
feature_importance <- importance(rf_model)
feature_importance <- feature_importance[order(-feature_importance[, 1]), ]

# Number of selected variables
num_features <- 5
selected_features <- rownames(feature_importance)[1:num_features]

# New dataset with selected variables and 'CO.GT.'
selected_dataset <- aq_dataset[, c(selected_features, "CO.GT.")]

# Bar plot of variable importance
barplot(feature_importance[, 1], 
        names.arg = rownames(feature_importance), 
        col = "lightblue", 
        main = "Feature Importance in Random Forest Model", 
        ylab = "Importance", 
        las = 2,  
        cex.names = 0.7,  
        border = NA,  
        ylim = c(0, 25))  

# Set seed for reproducibility
set.seed(123)

# Dataset splitting (80% training, 20% testing)
index <- createDataPartition(y = aq_dataset$CO.GT., p = 0.8, list = FALSE)
train_data <- aq_dataset[index, ]
test_data <- aq_dataset[-index, ]

# Preprocessing (centering and scaling)
preproc <- preProcess(train_data[, -which(names(train_data) == "CO.GT.")], method = c("center", "scale"))
train_norm <- predict(preproc, train_data[, -which(names(train_data) == "CO.GT.")])
test_norm <- predict(preproc, test_data[, -which(names(test_data) == "CO.GT.")])

# Add 'CO.GT.' back to normalized datasets
train_norm$CO.GT. <- train_data$CO.GT.
test_norm$CO.GT. <- test_data$CO.GT.

# Baseline model
mean_training_target <- mean(train_norm$CO.GT.)
baseline_predictions <- rep(mean_training_target, nrow(test_norm))

# Error metrics for baseline model
mae_baseline <- mean(abs(test_norm$CO.GT. - baseline_predictions))
mse_baseline <- mean((test_norm$CO.GT. - baseline_predictions)^2)
rmse_baseline <- sqrt(mse_baseline)
ss_total <- sum((test_norm$CO.GT. - mean(test_norm$CO.GT.))^2)
ss_res <- sum((test_norm$CO.GT. - baseline_predictions)^2)
r_squared_baseline <- 1 - (ss_res / ss_total)

cat("Baseline MAE:", mae_baseline, "\n")
cat("Baseline MSE:", mse_baseline, "\n")
cat("Baseline RMSE:", rmse_baseline, "\n")
cat("Baseline R^2:", r_squared_baseline, "\n")

# KNN regression with hyperparameter tuning
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(123)
knn_model <- train(CO.GT. ~ ., data = train_norm, method = 'knn', tuneLength = 10, trControl = train_control)
print(knn_model)

# RMSE vs k plot
knn_rmse_plot <- ggplot(knn_model$results, aes(x = k, y = RMSE)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  ggtitle("KNN Regression") +
  xlab("k (Number of Neighbors)") +
  ylab("RMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(knn_rmse_plot)

# Predictions with KNN
knn_predictions <- predict(knn_model, test_norm)
knn_results <- postResample(pred = knn_predictions, obs = test_norm$CO.GT.)
print(knn_results)

# Actual vs Predicted plot for KNN model
knn_actual_vs_predicted_plot <- ggplot(data.frame(pred = knn_predictions, actual = test_data$CO.GT.), 
                                       aes(x = pred, y = actual)) +
  geom_point(color = "blue", alpha = 0.6) +
  ggtitle("KNN Regression: Actual vs Predicted") +
  xlab("Predicted CO.GT.") +
  ylab("Actual CO.GT.") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(knn_actual_vs_predicted_plot)

# Ridge regression with hyperparameter tuning
ridge_model <- train(CO.GT. ~ ., data = train_norm, method = 'ridge', tuneLength = 10, trControl = train_control)
print(ridge_model)

# RMSE vs lambda plot
ridge_rmse_plot <- ggplot(ridge_model$results, aes(x = lambda, y = RMSE)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Ridge Regression") +
  xlab("lambda") +
  ylab("RMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(ridge_rmse_plot)

# Predictions with Ridge
ridge_predictions <- predict(ridge_model, test_norm)
ridge_results <- postResample(pred = ridge_predictions, obs = test_norm$CO.GT.)
print(ridge_results)

# Actual vs Predicted plot for Ridge model
ridge_actual_vs_predicted_plot <- ggplot(data.frame(pred = ridge_predictions, actual = test_data$CO.GT.), 
                                         aes(x = pred, y = actual)) +
  geom_point(color = "blue", alpha = 0.6) +
  ggtitle("Ridge Regression: Actual vs Predicted") +
  xlab("Predicted CO.GT.") +
  ylab("Actual CO.GT.") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(ridge_actual_vs_predicted_plot)
