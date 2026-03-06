#Random Forest Model to predict variables that are likely to predict targets for HIV screening

#load the dataset
library(readxl)
Pooled_male_model <- read_excel("C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Pooled_Sex_PrevalenceV2.xlsx")
View(Pooled_male_model)

# 📦 Load necessary libraries

library(ranger)
library(caret)
library(pROC)
library(smotefamily)
library(ggplot2)
library(stringr)

# 🧾 Prepare the dataset
#Create a copy of the original dataset and call it Mydata so that we can work without altering the original
Mydata <- Pooled_male_model

#Ensure the dependent variable is a factor- this line tells R to treat those values as categories, not numbers.
Mydata$hivstatusfinal <- as.factor(Mydata$hivstatusfinal)


# 🧪 Train-test split (80/20)
set.seed(42)
trainIndex <- createDataPartition(Mydata$hivstatusfinal, p = 0.8, list = FALSE)
train <- Mydata[trainIndex, ]
test <- Mydata[-trainIndex, ]

# 🚀 Fit Random Forest model using ranger
rf_model <- ranger(
  formula = hivstatusfinal ~ .,
  data = train,
  probability = TRUE,    	# Required for ROC/AUC
  num.trees = 500,
  importance = 'impurity',
  seed = 42
)

# 🔮 Predict probabilities on test set
rf_pred <- predict(rf_model, data = test)
pred_prob <- rf_pred$predictions[, "1"]
pred_class <- ifelse(pred_prob > 0.5, "1", "0")
pred_class <- as.factor(pred_class)

# 🧾 Confusion Matrix and Accuracy
conf_matrix <- confusionMatrix(pred_class, test$hivstatusfinal, positive = "1")
print(conf_matrix)
cat("Random Forest Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")

# 📈 ROC Curve and AUC
if (length(unique(test$hivstatusfinal)) == 2) {
  roc_obj <- roc(as.numeric(as.character(test$hivstatusfinal)), pred_prob)
  auc_val <- auc(roc_obj)
  plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve - Random Forest")
  abline(a = 0, b = 1, col = "gray", lty = 2)
  cat("AUC:", round(auc_val, 4), "\n")
} else {
  cat("ROC curve and AUC not computed: only one class present in test set.\n")
}

# ✅ Check class balance in training data
table(train$hivstatusfinal)

# ⚠️ Convert target to numeric for SMOTE
train$hivstatusfinal_numeric <- as.numeric(as.character(train$hivstatusfinal))

# Tanzania class counts
n_min <- sum(train$hivstatusfinal_numeric == 1)
n_maj <- sum(train$hivstatusfinal_numeric == 0)
target_ratio <- 0.4

# Calculate dup_size
dup_size <- ceiling ((target_ratio * n_maj / (1 - target_ratio) - n_min) / n_min)

# Apply SMOTE with calculated dup_size
smote_result <- SMOTE(
  X = train[, !(names(train) %in% c("hivstatusfinal", "hivstatusfinal_numeric"))],
  target = train$hivstatusfinal_numeric,
  K = 5,
  dup_size = dup_size
)

# ✅ Rebuild training set with SMOTE output
train_smote <- smote_result$data
train_smote$hivstatusfinal <- as.factor(ifelse(train_smote$class == 1, "1", "0"))
train_smote$class <- NULL

# ✅ Check class balance after SMOTE
table(train_smote$hivstatusfinal)

# 🧠 Fit ranger model on SMOTE data
rf_model_smote <- ranger(
  formula = hivstatusfinal~ .,
  data = train_smote,
  probability = TRUE,
  num.trees = 500,
  importance = "impurity",  # <-- Add this line
  seed = 42
)

# 🔮 Predict on original test set
rf_pred <- predict(rf_model_smote, data = test)
pred_prob <- rf_pred$predictions[, "1"]
pred_class <- as.factor(ifelse(pred_prob > 0.5, "1", "0"))

# 📊 Confusion matrix and accuracy
conf_matrix <- confusionMatrix(pred_class, test$hivstatusfinal, positive = "1")
print(conf_matrix)
cat("SMOTE Random Forest Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")

#Plot the ROC Curve & Choose Optimal Threshold
library(pROC)
roc_obj <- roc(as.numeric(as.character(test$hivstatusfinal)), pred_prob)
plot(roc_obj, main = "ROC Curve - SMOTE Model", col = "blue")
coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

#Apply This New Threshold:
pred_class_adj <- as.factor(ifelse(pred_prob > 0.074, "1", "0"))
confusionMatrix(pred_class_adj, test$hivstatusfinal, positive = "1")


# 📈 ROC curve and AUC
if (length(unique(test$hivstatusfinal)) == 2) {
  roc_obj <- roc(as.numeric(as.character(test$hivstatusfinal)), pred_prob)
  auc_val <- auc(roc_obj)
  plot(roc_obj, col = "orange", lwd = 2, main = "ROC Curve - SMOTE + Random Forest")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  cat("AUC:", round(auc_val, 4), "\n")
} else {
  cat("ROC curve and AUC not computed: only one class present in test set.\n")
}

# Feature importance specifically for predicting "Y"
# 🧮 Get feature importance from the SMOTE-based Random Forest model
feature_importance <- as.data.frame(rf_model_smote$variable.importance)
colnames(feature_importance) <- c("Importance")
feature_importance$Feature <- rownames(feature_importance)

# 📊 Sort features by importance
feature_importance <- feature_importance[order(feature_importance$Importance, decreasing = TRUE), ]

# 📊 Feature importance table with variable names and scores
feature_importance <- as.data.frame(rf_model_smote$variable.importance)
colnames(feature_importance) <- c("Importance")
feature_importance$Feature <- rownames(feature_importance)

# Sort from highest to lowest importance
feature_importance <- feature_importance[order(feature_importance$Importance, decreasing = TRUE), ]

# View table of variable names and scores
feature_importance

# If you only want Feature + Score columns in a clean format
importance_table <- feature_importance[, c("Feature", "Importance")]
print(importance_table)

#Export feature_importance to excel
# Load library
library(writexl)

# Define path
output_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Male_RF_Imp_Features.xlsx"

# Export to Excel
write_xlsx(feature_importance, path = output_path)


# 🎨 Plot the top 26 important features
top_n <- 26
ggplot(head(feature_importance, top_n), aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#1f78b4", width = 0.7) +  # Professional blue
  coord_flip() +  # Horizontal bars
  labs(
    title = "Pooled Female Prevalence Important Features - Random Forest",
    x = "Feature",
    y = "Importance (Impurity Decrease)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", color = "black", size = 11),  # <- Updated line
    axis.text.x = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#Export train_smote for a logistic regression
# Load library
library(writexl)

# Define path
output_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/female_prev_regression_data.xlsx"

# Export to Excel
write_xlsx(train_smote, path = output_path)

