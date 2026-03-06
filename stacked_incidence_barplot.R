library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1. Load your Excel file (update the path accordingly)
file_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Descriptive for Incidence.xlsx"
raw <- read_excel(file_path, col_names = FALSE)

# 2. Extract the first 18 rows and first 3 columns
binary_raw <- raw[1:18, 1:3]
colnames(binary_raw) <- c("Variable", "High", "Low")

# 3. Clean % signs and convert to numeric
binary_data <- binary_raw %>%
  mutate(
    High = as.numeric(gsub("%", "", High)),
    Low = as.numeric(gsub("%", "", Low))
  )

# 4. Fix decimals
binary_data <- binary_data %>%
  mutate(
    High = ifelse(High <= 1, High * 100, High),
    Low = ifelse(Low <= 1, Low * 100, Low)
  )

# 5. Pivot to long format
binary_long <- binary_data %>%
  pivot_longer(cols = c("High", "Low"), names_to = "Category", values_to = "Percent") %>%
  mutate(
    Variable = factor(Variable, levels = rev(binary_data$Variable)),
    Category = factor(Category, levels = c("Low", "High")),
    Label = paste0(round(Percent), "%")
  )

# 6. Custom right-side labels (provided by user)
right_labels <- c(
  "Urban", "Male", "Condomlastsex12months", "Lownum_lifetimesexpartners",
  "Work_12months", "High_alcohol_freq", "Never-married", "Firstsexage_18_or_above",
  "No_sex_12_months", "Never_had_sex", "lownum_memsleep_here", "hh_head_gendermale",
  "hh_electricity", "hh_radio", "hh_TV", "no_hh_phone", "avoidpreg", "High_education"
)
right_labels_rev <- rev(right_labels)

# 7. Extract Low category (blue) positions and add custom right labels
right_label_df <- binary_long %>%
  filter(Category == "Low") %>%
  mutate(Right_Label = right_labels_rev)


# 8. Plot
binary_plot <- ggplot(binary_long, aes(x = Variable, y = Percent, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    size = 3.8,
    fontface = "bold",
    color = "black"
  ) +
  
  # Add right-side labels (ensure correct order)
  geom_text(
    data = binary_data %>%
      mutate(
        Total = High + Low,
        Right_Label = right_labels,  # Use original order!
        Variable = factor(Variable, levels = rev(unique(Variable)))  # Match plot order
      ),
    aes(x = Variable, y = Total + 5, label = Right_Label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.3,
    color = "black"
  ) +
  
  scale_fill_manual(values = c("High" = "#F8766D", "Low" = "#00BFC4")) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = expansion(mult = c(0, 0.25))) +
  labs(y = "Percentage", x = NULL, title = "Binary Variables Distribution") +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )


# 9. Show the plot
print(binary_plot)
