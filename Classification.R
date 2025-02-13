# Set the working directory
setwd("C:/Intelligent Systems/dataset")

# Load the dataset
#dataset link: https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic
Breast_Cancer_dataset <- read.csv("wdbc.data", header = FALSE)

# Assign column names to the dataset
colnames(Breast_Cancer_dataset) <- c("ID", "Diagnosis", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", 
                                     "smoothness_mean", "compactness_mean", "concavity_mean", "concave_points_mean", 
                                     "symmetry_mean", "fractal_dimension_mean", "radius_se", "texture_se", 
                                     "perimeter_se", "area_se", "smoothness_se", "compactness_se", "concavity_se", 
                                     "concave_points_se", "symmetry_se", "fractal_dimension_se", "radius_worst", 
                                     "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst", 
                                     "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", 
                                     "fractal_dimension_worst")

# Select only the significant variables (excluding 'Diagnosis')
significant_vars <- Breast_Cancer_dataset[, c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", 
                                              "smoothness_mean", "compactness_mean", "concavity_mean", 
                                              "concave_points_mean", "symmetry_mean", "fractal_dimension_mean")]
head(significant_vars)


# Function to detect outliers using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25) # First quartile
  Q3 <- quantile(x, 0.75) # Third quartile
  IQR <- Q3 - Q1 # Interquartile range
  
  # Limits for defining outliers
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Returns a boolean vector indicating if the value is an outlier
  return(x < lower_limit | x > upper_limit)
}

# Set up the plot grid (3 rows and 4 columns)
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))  # Set margins to avoid overlap

# Apply the IQR function to detect outliers and create box plots
for (var in colnames(significant_vars)) {
  # Detect outliers for each variable
  outliers <- detect_outliers(significant_vars[[var]])
  
  # Display the box plot of the variable
  boxplot(significant_vars[[var]], main = var, col = "lightblue", 
          ylab = "Values", xlab = "Variable", horizontal = TRUE)
  
  # Count the number of outliers
  outlier_count <- sum(outliers)
  
  # Print the count of outliers and the anomalous values
  cat(paste("For variable", var, "there are", outlier_count, "anomalous values.\n"))
  cat("Anomalous values:\n")
  print(significant_vars[outliers, var, drop = FALSE])
  cat("\n\n")
}

# Reset plot to default view
par(mfrow = c(1, 1))

# Create a 3x4 grid for the plots
par(mfrow = c(3, 4))

# Perform Shapiro-Wilk test and create QQ plots for each variable (except 'Diagnosis')
for (col in colnames(significant_vars)) {
  # Shapiro-Wilk Test
  shapiro_test <- shapiro.test(significant_vars[[col]])
  p_value <- format(shapiro_test$p.value, digits = 2)
  print(paste("S-W Test", col, ": p-value =", p_value))
  
  # QQ plot with blue points
  qqnorm(significant_vars[[col]], main = paste("QQ Plot for", col), col = "blue")
  qqline(significant_vars[[col]], col = "red")
}

# Reset the plot grid to 1x1
par(mfrow = c(1, 1))

# Create a bar plot for the 'Diagnosis' variable
# Check the distribution of the 'Diagnosis' variable
diagnosis_distribution <- table(Breast_Cancer_dataset$Diagnosis)

# Print the count for each diagnosis
print(diagnosis_distribution)

# Create the bar plot
barplot_heights <- barplot(diagnosis_distribution, 
                           main = "Distribution of the 'Diagnosis' Variable", 
                           xlab = "Diagnosis", ylab = "Count", 
                           col = c("lightgreen", "lightcoral"), 
                           ylim = c(0, max(diagnosis_distribution) + 10))

# Add labels to the center of the bars
text(barplot_heights, diagnosis_distribution / 2, labels = diagnosis_distribution, 
     col = "black", font = 2, cex = 1.2)

# Function to detect outliers using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25) # First quartile
  Q3 <- quantile(x, 0.75) # Third quartile
  IQR <- Q3 - Q1 # Interquartile range
  
  # Limits for defining outliers
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Returns a boolean vector indicating if the value is an outlier
  return(x < lower_limit | x > upper_limit)
}

# Function to replace outliers with limits
replace_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  # Limits for the outliers
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  # Replace anomalous values with limits
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  
  return(x)
}

# Set up the plot grid (3 rows and 4 columns)
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))  # Set margins to avoid overlap

# Apply the IQR function to detect outliers, replace them, and create updated box plots
for (var in colnames(significant_vars)) {
  # Detect outliers for each variable
  outliers <- detect_outliers(significant_vars[[var]])
  
  # Replace outliers with limits
  significant_vars[[var]] <- replace_outliers(significant_vars[[var]])
  
  # Create the updated box plot for the variable
  boxplot(significant_vars[[var]], main = paste(var, "- Without Outliers"), col = "lightgreen", 
          ylab = "Values", xlab = "Variable", horizontal = TRUE)
  
  # Print the number of replaced outliers
  outlier_count <- sum(outliers)
  cat(paste("For variable", var, outlier_count, "anomalous values have been replaced.\n"))
  cat("\n\n")
}

# Reset plot to default view
par(mfrow = c(1, 1))

# Perform the correlation matrix excluding "CO.GT."
# Select only numeric columns
numeric_data <- significant_vars[, sapply(significant_vars, is.numeric)]

# Calculate the correlation matrix (excluding "CO.GT.")
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Print the correlation matrix to screen
print("Correlation matrix (excluding CO.GT.):")
print(cor_matrix)

# Install and load the 'corrplot' package to visualize the correlation matrix
library(corrplot)

# Visualize the correlation matrix with customization
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




# Load the necessary library
library(caret)

# Extract the predictor variables (X) excluding 'Diagnosis'
X <- significant_vars

# Extract the response variable (y)
y <- Breast_Cancer_dataset$Diagnosis  

# Convert y to a factor (since it is a categorical variable)
y <- as.factor(y)

# Set the seed for reproducibility
set.seed(123)

# Split the dataset into Training (75%) and Test (25%)
trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Print the dimensions of the datasets
cat("Training Set Dimensions (X_train):", dim(X_train), "\n")
cat("Test Set Dimensions (X_test):", dim(X_test), "\n")

cat("Dimensions of y_train:", length(y_train), "\n")
cat("Dimensions of y_test:", length(y_test), "\n")

# Check that there are no overlaps between train and test sets
cat("\nCommon elements between Train and Test:", length(intersect(rownames(X_train), rownames(X_test))), "\n")

# Normalization with Min-Max Scaling
scaling_model <- preProcess(X_train, method = "range")

# Apply the transformation to the datasets
scaled_X_train <- predict(scaling_model, X_train)
scaled_X_test <- predict(scaling_model, X_test)

# Verify the correctness of scaling
cat("\nFirst few rows of X_train after scaling:\n")
print(head(scaled_X_train))

cat("\nFirst few rows of X_test after scaling:\n")
print(head(scaled_X_test))

# Check the range of scaled data (should be between 0 and 1)
cat("\nRange of scaled data (Min-Max) on X_train:\n")
print(apply(scaled_X_train, 2, range))

cat("\nRange of scaled data (Min-Max) on X_test:\n")
print(apply(scaled_X_test, 2, range))

# Random Forest

# Load the necessary libraries
library(randomForest)
library(caret)
library(pROC)

# Create the Random Forest model
rf_model <- randomForest(
  x = scaled_X_train,
  y = factor(y_train),  
  classwt = c(1, 2),  
  ntree = 100, 
  mtry = sqrt(ncol(scaled_X_train)), 
  nodesize = 15, 
  maxnodes = 15   
)

# Predictions on the training and test sets
train_preds <- predict(rf_model, newdata = scaled_X_train)
test_preds <- predict(rf_model, newdata = scaled_X_test)

# Create the classification report for training and test sets
train_conf_matrix <- confusionMatrix(factor(train_preds), factor(y_train))
test_conf_matrix <- confusionMatrix(factor(test_preds), factor(y_test))

# Print the classification reports
cat("Random Forest Classification Report - Training Set:\n")
print(train_conf_matrix)
cat("Random Forest Classification Report - Test Set:\n")
print(test_conf_matrix)

# Extract the scores
train_accuracy <- train_conf_matrix$overall["Accuracy"]
train_precision <- train_conf_matrix$byClass["Precision"]
train_recall <- train_conf_matrix$byClass["Recall"]
train_f1 <- train_conf_matrix$byClass["F1"]

test_accuracy <- test_conf_matrix$overall["Accuracy"]
test_precision <- test_conf_matrix$byClass["Precision"]
test_recall <- test_conf_matrix$byClass["Recall"]
test_f1 <- test_conf_matrix$byClass["F1"]

# Print precision, recall, and F1 score
cat("Metrics Random Forest - Training Set:\n")
cat(sprintf("Accuracy Score - Training Set: %.4f\n", train_accuracy))
cat(sprintf("Precision: %.4f\n", train_precision))
cat(sprintf("Recall: %.4f\n", train_recall))
cat(sprintf("F1 Score: %.4f\n", train_f1))

cat("Metrics Random Forest - Test Set:\n")
cat(sprintf("Accuracy Score - Test Set: %.4f\n", test_accuracy))
cat(sprintf("Precision: %.4f\n", test_precision))
cat(sprintf("Recall: %.4f\n", test_recall))
cat(sprintf("F1 Score: %.4f\n", test_f1))

# ROC for Random Forest
test_prob_rf <- predict(rf_model, newdata = scaled_X_test, type = "prob")[,2]
roc_rf <- roc(factor(y_test), test_prob_rf)
plot(roc_rf, main = "Random Forest ROC Curve", col = "blue")
auc(roc_rf)

# SVM

# Load the necessary library
library(e1071)

# Create the SVM model
svm_model <- svm(factor(y_train) ~ ., data = scaled_X_train, kernel = "radial", cost = 1, probability = TRUE, class.weights = c("B" = 1, "M" = 2))

# Print model details
print(svm_model)

# Predictions on training and test sets
train_preds_svm <- predict(svm_model, newdata = scaled_X_train)
test_preds_svm <- predict(svm_model, newdata = scaled_X_test)

# Classification report for training and test sets
train_conf_matrix_svm <- confusionMatrix(factor(train_preds_svm), factor(y_train))
test_conf_matrix_svm <- confusionMatrix(factor(test_preds_svm), factor(y_test))

# Print the classification reports
cat("SVM Classification Report - Training Set:\n")
print(train_conf_matrix_svm)
cat("SVM Classification Report - Test Set:\n")
print(test_conf_matrix_svm)

# Extract the scores
train_accuracy_svm <- train_conf_matrix_svm$overall["Accuracy"]
train_precision_svm <- train_conf_matrix_svm$byClass["Precision"]
train_recall_svm <- train_conf_matrix_svm$byClass["Recall"]
train_f1_svm <- train_conf_matrix_svm$byClass["F1"]

test_accuracy_svm <- test_conf_matrix_svm$overall["Accuracy"]
test_precision_svm <- test_conf_matrix_svm$byClass["Precision"]
test_recall_svm <- test_conf_matrix_svm$byClass["Recall"]
test_f1_svm <- test_conf_matrix_svm$byClass["F1"]

# Print precision, recall, and F1 score for SVM
cat("Metrics SVM - Training Set:\n")
cat(sprintf("Accuracy Score - Training Set: %.4f\n", train_accuracy_svm))
cat(sprintf("Precision: %.4f\n", train_precision_svm))
cat(sprintf("Recall: %.4f\n", train_recall_svm))
cat(sprintf("F1 Score: %.4f\n", train_f1_svm))

cat("Metrics SVM - Test Set:\n")
cat(sprintf("Accuracy Score - Test Set: %.4f\n", test_accuracy_svm))
cat(sprintf("Precision: %.4f\n", test_precision_svm))
cat(sprintf("Recall: %.4f\n", test_recall_svm))
cat(sprintf("F1 Score: %.4f\n", test_f1_svm))

# ROC curve for SVM
test_prob_svm <- predict(svm_model, newdata = scaled_X_test, probability = TRUE)
test_prob_svm <- attr(test_prob_svm, "probabilities")[,2]
roc_svm <- roc(factor(y_test), test_prob_svm)
plot(roc_svm, main = "SVM ROC Curve", col = "blue")
auc(roc_svm)

# Combine ROC curves
# ROC Curve for Random Forest
test_prob_rf_comb <- predict(rf_model, newdata = scaled_X_test, type = "prob")[,2]
roc_rf_comb <- roc(factor(y_test), test_prob_rf_comb)

# ROC Curve for SVM
test_prob_svm_comb <- predict(svm_model, newdata = scaled_X_test, probability = TRUE)
test_prob_svm_comb <- attr(test_prob_svm_comb, "probabilities")[,2]
roc_svm_comb <- roc(factor(y_test), test_prob_svm_comb)


# Create ROC curve objects for both models
roc_obj_RF <- roc(factor(y_test), test_prob_rf_comb)
roc_obj_SVM <- roc(factor(y_test), test_prob_svm_comb)

# Plot the two ROC curves together
plot(roc_obj_RF, main="ROC Curves: Random Forest vs SVM", col="red", lty=2, lwd=2) # Red dashed line for Random Forest
plot(roc_obj_SVM, col="blue", lty=1, lwd=2, add=TRUE) # Blue solid line for SVM

# Add legend to differentiate the curves
legend("bottomright", legend=c("Random Forest", "SVM"), col=c("red", "blue"), lty=c(2, 1), lwd=2)

# Display AUC values for both models with 4 decimal places
cat("AUC Random Forest:", format(auc(roc_obj_RF), digits = 4), "\n")
cat("AUC SVM:", format(auc(roc_obj_SVM), digits = 4), "\n")


        



