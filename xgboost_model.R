# XGBoost Model for HIV Screening Prediction (adapted from your RF script)

# Load libraries
library(readxl)
library(caret)
library(pROC)
library(smotefamily)
library(ggplot2)
library(xgboost)
library(dplyr)
library(Ckmeans.1d.dp)
library(writexl)

# Load data
Pooled_female_model <- read_excel("C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Pooled_Sex_PrevalenceV2.xlsx")

# Prepare dataset
Mydata <- Pooled_female_model
Mydata$hivstatusfinal <- as.factor(Mydata$hivstatusfinal)

# Train-test split (80/20)
set.seed(42)
trainIndex <- createDataPartition(Mydata$hivstatusfinal, p = 0.8, list = FALSE)
train <- Mydata[trainIndex, ]
test <- Mydata[-trainIndex, ]

# Check class balance in training
table(train$hivstatusfinal)

# Convert target to numeric for SMOTE
train$hivstatusfinal_numeric <- as.numeric(as.character(train$hivstatusfinal))

# SMOTE settings
n_min <- sum(train$hivstatusfinal_numeric == 1)
n_maj <- sum(train$hivstatusfinal_numeric == 0)
target_ratio <- 0.4
dup_size <- ceiling((target_ratio * n_maj / (1 - target_ratio) - n_min) / n_min)

# Apply SMOTE
smote_result <- SMOTE(
  X = train[, !(names(train) %in% c("hivstatusfinal", "hivstatusfinal_numeric"))],
  target = train$hivstatusfinal_numeric,
  K = 5,
  dup_size = dup_size
)

# Balanced training data
train_smote <- smote_result$data
train_smote$hivstatusfinal <- as.factor(ifelse(train_smote$class == 1, "1", "0"))
train_smote$class <- NULL
table(train_smote$hivstatusfinal)

# Model matrix for xgboost
train_matrix <- model.matrix(hivstatusfinal ~ . - 1, data = train_smote)
train_label <- as.numeric(as.character(train_smote$hivstatusfinal))

test_matrix <- model.matrix(hivstatusfinal ~ . - 1, data = test)
test_label <- as.numeric(as.character(test$hivstatusfinal))

# DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  seed = 42
)

# Train xgboost
set.seed(42)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 20,
  print_every_n = 20,
  maximize = TRUE
)

# Predict probabilities
pred_prob <- predict(xgb_model, dtest)

# Default threshold (0.5)
pred_class <- factor(ifelse(pred_prob > 0.5, "1", "0"), levels = c("0", "1"))

# Confusion matrix at 0.5
conf_matrix <- confusionMatrix(pred_class, factor(test$hivstatusfinal, levels = c("0", "1")), positive = "1")
print(conf_matrix)
cat("XGBoost Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")

# ROC and AUC
roc_obj <- roc(as.numeric(as.character(test$hivstatusfinal)), pred_prob)
auc_val <- auc(roc_obj)
plot(roc_obj, main = "ROC Curve - XGBoost Model", col = "darkgreen", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)
cat("AUC:", round(auc_val, 4), "\n")

# Optimal threshold
opt_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(opt_coords)
optimal_threshold <- as.numeric(opt_coords$threshold)

# Adjusted classification
pred_class_adj <- factor(ifelse(pred_prob > optimal_threshold, "1", "0"), levels = c("0", "1"))
test_labels <- factor(test$hivstatusfinal, levels = c("0", "1"))

# Confusion matrix with adjusted threshold
conf_matrix_adj <- confusionMatrix(pred_class_adj, test_labels, positive = "1")
print(conf_matrix_adj)


# Get importance
importance_matrix <- xgb.importance(model = xgb_model)

# Take top 26 features
top_features <- importance_matrix[1:26, ]

# 📊 Feature importance table with variable names and scores from XGBoost
importance_matrix <- xgb.importance(model = xgb_model)

# Keep only Feature and Gain columns for clarity
importance_table <- importance_matrix[, c("Feature", "Gain")]

# Sort from highest to lowest importance
importance_table <- importance_table[order(importance_table$Gain, decreasing = TRUE), ]

# Print table
print(importance_table)

# Export to Excel
library(writexl)
output_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Female_XGB_Imp_Features.xlsx"
write_xlsx(importance_table, path = output_path)


# Create ggplot bar chart
ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "#1f78b4", width = 0.7) +  # Professional blue
  coord_flip() +  # Horizontal bars
  labs(
    title = "Pooled Male Prevalence Important Features  – XGBoost Model",
    x = "Feature",
    y = "Gain (Importance)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", color = "black", size = 11), 
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),  # Clean background
    panel.grid.minor = element_blank()
  )


# Export SMOTE training data
output_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/female_prev_train_smote.xlsx"
write_xlsx(train_smote, path = output_path)
