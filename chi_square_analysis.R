# Load necessary libraries
library(readxl)
library(dplyr)

# Step 1: Import the Excel dataset
data <- read_excel("C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/ChiSquare_Incidence_data.xlsx")

# Step 2: Check structure of the relevant variables
table(data$hivstatusfinal, data$High_alcohol_freq)

# Step 3: Create contingency table
table_chi <- table(data$hivstatusfinal, data$High_alcohol_freq)

# Step 4: Perform Pearson Chi-square test
chi_result <- chisq.test(table_chi)

# Step 5: Print results
print(table_chi)
print(chi_result)

