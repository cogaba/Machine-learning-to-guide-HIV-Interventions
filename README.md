# Machine-learning-to-guide-HIV-Interventions
R Code for predicting HIV status using random forest and XG Boost on data from four countries in Sub-Saharan Africa
# Machine Learning to Guide HIV Interventions

This repository contains the analysis code used in the study:

**“Leveraging Modern Machine Learning Techniques to Guide Targeted HIV Interventions and Prevention Strategies in Sub-Saharan Africa .”**

The scripts implement statistical and machine learning methods to identify predictors of HIV positivity and explore cross-country patterns using population-based HIV survey data. The analysis supports evidence-based targeting of HIV prevention and treatment interventions.

---

# Repository Structure

The repository includes the following R scripts:

### 1. SMOTE Random Forest Model

**File:**
smote_random_forest_model.R

Purpose:

* Applies **SMOTE (Synthetic Minority Oversampling Technique)** to address class imbalance in HIV status.
* Trains a **Random Forest model** to predict HIV positivity.
* Extracts variable importance to identify key predictors.

---

### 2. XGBoost Model

**File:**
xgboost_model.R

Purpose:

* Implements an **Extreme Gradient Boosting (XGBoost)** model.
* Provides an alternative machine learning approach for predicting HIV status.
* Allows comparison of model performance with Random Forest.

---

### 3. Logistic Regression Analysis

**File:**
logistic_regression_analysis.R

Purpose:

* Fits a **logistic regression model** to estimate associations between predictors and HIV status.
* Generates plots to aid interpretation and comparison with machine learning models.

---

### 4. Chi-Square Analysis

**File:**
chi_square_analysis.R

Purpose:

* Performs **chi-square tests** to examine bivariate associations between categorical predictors and HIV status.

---

### 5. Rank Correlation and Heatmap

**File:**
kendall_spearman_heatmap.R

Purpose:

* Calculates **Spearman and Kendall rank correlations**.
* Produces **heatmaps** to compare predictor importance across countries and pooled datasets.

---

### 6. HIV Incidence Visualization

**File:**
Stacked bar of incidence cases R code for Machine learning to guide HIV Interventions.R

Purpose:

* Generates **stacked bar charts** illustrating HIV incidence or case distribution across countries.

---

### 7. Country-Level New Infections

**File:**
HIV New Infections by Country R code for Machine learning to guide HIV Interventions.R

Purpose:

* Produces country-level summaries and visualizations of **new HIV infections**.

---

# Data Availability Statement

The data that support the findings of this study are available from the **PHIA Project at Columbia University**.

According to our university-sponsored data use agreement, the data cannot be publicly shared in this repository. However, researchers can create an account and obtain login credentials through the PHIA Project website to access country- and year-specific datasets.

The data repository is available at:

https://phia-data.icap.columbia.edu/datasets

---

# Software Requirements

The analysis was conducted using **R**.

Key packages used include:

* tidyverse
* ranger
* caret
* smotefamily
* xgboost
* ggplot2
* pheatmap

Users should install the required packages before running the scripts.

---

# Reproducibility

Each script corresponds to a specific component of the analysis. Running the scripts sequentially will reproduce the statistical analyses and visualizations presented in the study, provided that the user has obtained access to the PHIA datasets.

---

# Author

Collins Gaba

---

# License

This repository is shared for academic and research purposes to support **open science and reproducible research**.
