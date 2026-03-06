# Load required libraries
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(writexl)  # For exporting
library(forcats) # For factor ordering

# Step 1: Import Excel file
data <- read_excel("C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/female_prev_regression_data.xlsx")

# Step 2: Force numeric variables to 0/1 if applicable
data[] <- lapply(data, function(x) {
  if (is.numeric(x)) round(x) else x
})

# Step 3: Make hivstatusfinal a factor with 0 as reference
data$hivstatusfinal <- factor(data$hivstatusfinal, levels = c(0, 1))

# Step 4: Run logistic regression
indep_vars <- setdiff(names(data), "hivstatusfinal")
formula <- as.formula(paste("hivstatusfinal ~", paste(indep_vars, collapse = " + ")))
model <- glm(formula, data = data, family = "binomial")

# Step 5: Create results table with odds ratios and CI
results <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = term,
    Odds_Ratio = round(estimate, 2),
    CI_Lower = round(conf.low, 2),
    CI_Upper = round(conf.high, 2),
    P_Value = signif(p.value, 3),
    Significant = P_Value < 0.05,
    Direction = ifelse(Odds_Ratio > 1, "Positive", "Negative")
  ) %>%
  select(Variable, Odds_Ratio, CI_Lower, CI_Upper, P_Value, Direction, Significant)

# Step 6: Export results table to Excel
write_xlsx(results, "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/female_prev_results.xlsx")

# Step 7: Plot odds ratios on log scale
ggplot(results, aes(x = fct_reorder(Variable, Odds_Ratio), y = log(Odds_Ratio), fill = Direction, alpha = Significant)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#1b9e77", "Negative" = "#d95f02")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4)) +
  labs(
    title = "Pooled Female Prevalence Odds Ratios",
    x = "Variable",
    y = "Log Odds",
    fill = "Direction",
    alpha = "Significant (p < 0.05)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(color = "black", face = "bold"),  # Darker variable labels
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

