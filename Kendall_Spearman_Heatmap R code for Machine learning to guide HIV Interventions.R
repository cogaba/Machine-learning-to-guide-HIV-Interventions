############################################################
#  Cross-Country + Pooled Rank Agreement Analysis (Males)
#  Kendall’s W + Spearman Correlation + Heatmaps
############################################################

# ===============================
# 1. Install & Load Packages
# ===============================

packages <- c("readxl", "dplyr", "irr", "ggplot2", 
              "reshape2", "corrplot")

installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])

library(readxl)
library(dplyr)
library(irr)
library(ggplot2)
library(reshape2)
library(corrplot)

# ===============================
# 2. Import Excel File
# ===============================

file_path <- "C:/Users/cogaba/OneDrive - Boston University/Desktop/CISHW/Random Forest Project/Reviewer Feedback/All-Country Variable rank-male.xlsx"

data <- read_excel(file_path)

colnames(data)

# ===============================
# 3. Rank Variables
#    Highest importance = Rank 1
# ===============================

ranked_data <- data %>%
  mutate(
    Malawi      = rank(-Malawi, ties.method = "average"),
    Mozambique  = rank(-Mozambique, ties.method = "average"),
    Tanzania    = rank(-Tanzania, ties.method = "average"),
    Uganda      = rank(-Uganda, ties.method = "average"),
    Pooled      = rank(-Pooled, ties.method = "average")
  )

# ===============================
# 4. Kendall’s W Across ALL 5 Models
# ===============================

rank_matrix_all <- ranked_data %>%
  select(Malawi, Mozambique, Tanzania, Uganda, Pooled)

kendall_result_all <- kendall(rank_matrix_all)

cat("\n==============================\n")
cat("Kendall's Coefficient of Concordance (W) - All 5 Models\n")
cat("==============================\n")
print(kendall_result_all)

# ===============================
# 5. Pairwise Spearman Correlations
# ===============================

cor_matrix_all <- cor(rank_matrix_all, method = "spearman")

cat("\n==============================\n")
cat("Spearman Rank Correlation Matrix (Countries + Pooled)\n")
cat("==============================\n")
print(cor_matrix_all)

# ===============================
# 6. Country vs Pooled Correlations
# ===============================

cat("\n==============================\n")
cat("Country vs Pooled Spearman Correlations\n")
cat("==============================\n")

print(cor_matrix_all["Pooled", 
                     c("Malawi", 
                       "Mozambique", 
                       "Tanzania", 
                       "Uganda")])

# ===============================
# 7. Correlation Heatmap
# ===============================

corrplot(cor_matrix_all,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Spearman Rank Correlation (Countries + Pooled)",
         mar = c(0,0,1,0))

# ===============================
# 8. Rank Heatmap (Improved)
# ===============================

rank_long <- ranked_data %>%
  select(Variable, Malawi, Mozambique, 
         Tanzania, Uganda, Pooled) %>%
  melt(id.vars = "Variable")

# Force model order
rank_long$variable <- factor(rank_long$variable,
                             levels = c("Malawi","Mozambique","Tanzania","Uganda","Pooled"))

ggplot(rank_long, aes(x = variable,
                      y = reorder(Variable, -value),
                      fill = value)) +
  geom_tile(color = "white", size = 0.4) +
  
  scale_fill_gradient(
    low = "#8B0000",     # dark red
    high = "#F5F5F5",    # light grey (better than white)
    guide = guide_colorbar(reverse = TRUE,
                           barheight = 10,
                           barwidth = 1)
  ) +
  
  theme_minimal(base_size = 12) +
  
  labs(title = "Predictor Rank Across Countries and Pooled Model (Males)",
       x = "Model",
       y = "Predictor",
       fill = "Rank") +
  
  theme(
    axis.text.y = element_text(element_text(face = "bold", color = "black", size = 11),
    axis.text.x = element_text(element_text(face = "bold", color = "black", size = 11),
    axis.title  = element_text(size = 13, face = "bold"),
    plot.title  = element_text(size = 15, face = "bold", hjust = 0.5),
    
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )



axis.text.y = element_text(face = "bold", color = "black", size = 11








