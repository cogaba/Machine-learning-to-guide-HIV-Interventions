# Load required libraries
library(readxl)
library(ggplot2)
library(scales)
library(showtext)
library(dplyr)

# Add clean Google font
font_add_google("Lato", "lato")
showtext_auto()

# Read in your Excel data
data <- read_excel("C:/Users/cogaba/OneDrive - Boston University/Desktop/Random Forest Project/Policy Brief.xlsx")

# Custom blue color
custom_blue <- "#36C5C8"

# Set custom order in reverse so they display top-to-bottom in coord_flip
country_order <- rev(c(
  "Botswana", "Eswatini", "Lesotho", "Malawi", 
  "Mozambique", "Tanzania", "Uganda", 
  "Zambia", "Zimbabwe", "Pooled"
))

# Apply factor levels
data$Country <- factor(data$Country, levels = country_order)

# Redraw plot
ggplot(data, aes(x = Country, y = Frequency)) +
  geom_bar(stat = "identity", fill = custom_blue, width = 0.7) +
  geom_text(aes(label = Frequency), hjust = -0.1, color = "black", size = 5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "HIV New Infections by Country",
    x = NULL,
    y = "Number of New Infections"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),  # Bigger country names
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(y = max(data$Frequency) * 1.1)
