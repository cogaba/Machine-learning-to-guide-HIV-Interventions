################################################################################
# generate_figures.R
#
# Draws the main and supplementary figures from the results files; no model
# is refit here.
#
#   Fig 1  male XGBoost feature importance     <- DOWNSTREAM_RESULTS.xlsx (feature_importance)
#   Fig 2  female XGBoost feature importance   <- DOWNSTREAM_RESULTS.xlsx (feature_importance)
#   Fig 3  male weighted logistic regression   <- WEIGHTED_LR_RESULTS.xlsx (weighted_LR_male)
#   Fig 4  female weighted logistic regression <- WEIGHTED_LR_RESULTS.xlsx (weighted_LR_female)
#   S1 Fig male predictor-rank heatmap         <- COUNTRY_CONCORDANCE_RESULTS.xlsx (heatmap_rank_data)
#   S2 Fig female predictor-rank heatmap       <- COUNTRY_CONCORDANCE_RESULTS.xlsx (heatmap_rank_data)
#   S4 Fig calibration plots, raw (A) and Platt-recalibrated (B)
#                                               <- DOWNSTREAM_RESULTS.xlsx (calibration_plot_data)
#
# Predictor labels describe the coded variable (the category coded 1).
# Significance stars: * p<0.05, ** p<0.01, *** p<0.001.
#
# Output: PNG and TIFF (300 dpi) for each figure, in artifacts/figures.
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

source("00_config.R")
fig_dir <- file.path(artifact_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

downstream_path  <- artifact_path("DOWNSTREAM_RESULTS.xlsx")
lr_path          <- artifact_path("WEIGHTED_LR_RESULTS.xlsx")
concordance_path <- artifact_path("COUNTRY_CONCORDANCE_RESULTS.xlsx")

for (p in c(downstream_path, lr_path, concordance_path)) {
  if (!file.exists(p)) stop("Required results file not found: ", p)
}

BAR_COLOR <- "#156082"

save_fig <- function(plot, name, width, height) {
  png_path <- file.path(fig_dir, paste0(name, ".png"))
  tif_path <- file.path(fig_dir, paste0(name, ".tif"))
  ggsave(png_path, plot, width = width, height = height, dpi = 300, bg = "white")
  ggsave(tif_path, plot, width = width, height = height, dpi = 300, bg = "white",
         compression = "lzw")
  message("Wrote: ", png_path, " and .tif")
}

stars_from_p <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

# Predictor labels (the category coded 1).
FEATURE_LABEL <- c(
  Urban_rural            = "Rural residence",
  condomlastsex12months  = "No condom use at last sex, past 12 months",
  Num_sexpartners_life   = "3 or more lifetime sex partners",
  work_12months          = "Not employed in past 12 months",
  alcoholic_freq         = "Alcohol use 2 or more times per week",
  ever_married           = "Never married",
  first_sex_at_age       = "Sexual debut before age 18",
  sex_12_months          = "Sexually active in past 12 months",
  Members_sleep_here     = "Household size greater than 4",
  HH_head_gender         = "Female household head",
  HH_Electricity         = "No household electricity",
  HH_radio               = "No household radio",
  HH_TV                  = "No household television",
  HH_phone               = "No household telephone",
  avoidpreg              = "Uses contraception to avoid pregnancy",
  education              = "No or primary education only",
  agecat_15_24           = "Age 15-24",
  agecat_25_34           = "Age 25-34",
  agecat_35_44           = "Age 35-44",
  agecat_45_54           = "Age 45-54",
  agecat_55_64           = "Age 55-64",
  agecat_65plus          = "Age 65 and above",
  low_wealth             = "Low household wealth",
  mid_wealth             = "Middle household wealth",
  high_wealth            = "High household wealth",
  Male_Circumcision      = "Uncircumcised",
  live_births            = "At least one live birth"
)

theme_manuscript <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title = element_text(size = 12),
      plot.title = element_blank(),
      plot.caption = element_blank()
    )
}

# ==============================================================================
# Fig 1 / Fig 2: XGBoost feature importance (pooled model, no resampling)
# ==============================================================================
importance_all <- read_excel(downstream_path, sheet = "feature_importance") %>%
  filter(model == "XGB", resampling == "none")

make_importance_fig <- function(sex_label, out_name) {
  df <- importance_all %>%
    filter(sex == sex_label) %>%
    mutate(label = FEATURE_LABEL[feature]) %>%
    arrange(gain)
  df$label <- factor(df$label, levels = df$label)

  p <- ggplot(df, aes(x = label, y = gain)) +
    geom_col(fill = BAR_COLOR, width = 0.7) +
    coord_flip() +
    labs(x = NULL, y = "Gain (Importance)") +
    theme_manuscript()

  save_fig(p, out_name, width = 9, height = 0.32 * nrow(df) + 1.5)
}

make_importance_fig("male",   "Fig1_male_importance")
make_importance_fig("female", "Fig2_female_importance")

# ==============================================================================
# Fig 3 / Fig 4: weighted logistic regression, odds ratios
# ==============================================================================
make_lr_fig <- function(sheet_name, out_name, xmax) {
  df <- read_excel(lr_path, sheet = sheet_name) %>%
    mutate(label = FEATURE_LABEL[term],
           star  = stars_from_p(p_value)) %>%
    arrange(desc(OR))
  df$label <- factor(df$label, levels = rev(df$label))

  p <- ggplot(df, aes(x = label, y = OR)) +
    geom_col(fill = BAR_COLOR, width = 0.7) +
    geom_text(aes(label = paste0(sub("0+$", "", sub("\\.$", "", sprintf("%.2f", OR))), star),
                  y = OR + xmax * 0.012),
              hjust = 0, size = 3.6) +
    coord_flip(clip = "off") +
    scale_y_continuous(limits = c(0, xmax), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Odds Ratio") +
    theme_manuscript()

  save_fig(p, out_name, width = 9, height = 0.42 * nrow(df) + 1.5)
}

lr_male   <- read_excel(lr_path, sheet = "weighted_LR_male")
lr_female <- read_excel(lr_path, sheet = "weighted_LR_female")
make_lr_fig("weighted_LR_male",   "Fig3_male_LR",   xmax = max(lr_male$OR)   * 1.15)
make_lr_fig("weighted_LR_female", "Fig4_female_LR", xmax = max(lr_female$OR) * 1.15)

# ==============================================================================
# S1 / S2 Fig: predictor-rank heatmap (country-specific vs. pooled models)
# ==============================================================================
heatmap_all <- read_excel(concordance_path, sheet = "heatmap_rank_data")

make_heatmap_fig <- function(sex_label, out_name) {
  df <- heatmap_all %>% filter(sex == sex_label) %>%
    mutate(label = FEATURE_LABEL[feature])

  pooled_order <- df %>% filter(model == "Pooled") %>% arrange(rank) %>% pull(label)
  df$label <- factor(df$label, levels = rev(pooled_order))

  model_order <- c(sort(setdiff(unique(df$model), "Pooled")), "Pooled")
  df$model <- factor(df$model, levels = model_order)

  p <- ggplot(df, aes(x = model, y = label, fill = rank)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient(low = "#67000d", high = "#fff5f0", name = "Rank") +
    labs(x = "Model", y = "Predictor") +
    theme_manuscript() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 0))

  save_fig(p, out_name, width = 8, height = 0.34 * length(pooled_order) + 1.5)
}

make_heatmap_fig("male",   "S1_Fig_male_heatmap")
make_heatmap_fig("female", "S2_Fig_female_heatmap")

# ==============================================================================
# Calibration plots (primary model: XGBoost, no resampling), raw and
# Platt-recalibrated, both sexes
# ==============================================================================
calib_data <- read_excel(downstream_path, sheet = "calibration_plot_data") %>%
  filter(model == "XGB", resampling == "none")

make_calibration_fig <- function(prob_type, out_name) {
  df <- calib_data %>% filter(probability_type == prob_type) %>%
    mutate(sex_label = tools::toTitleCase(sex))
  max_val <- max(df$mean_predicted, df$mean_observed) * 1.05

  p <- ggplot(df, aes(x = mean_predicted, y = mean_observed)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = BAR_COLOR) +
    geom_point(color = BAR_COLOR, size = 2) +
    coord_cartesian(xlim = c(0, max_val), ylim = c(0, max_val)) +
    facet_wrap(~ sex_label) +
    labs(x = "Mean Predicted Probability", y = "Mean Observed Proportion") +
    theme_manuscript() +
    theme(panel.grid.major.y = element_line(color = "grey90"),
          strip.background = element_blank(), strip.text = element_text(size = 12))

  save_fig(p, out_name, width = 8, height = 4.2)
}

make_calibration_fig("raw",   "S4_Fig_A_calibration_raw")
make_calibration_fig("platt", "S4_Fig_B_calibration_platt")

message("\nAll figures written to: ", fig_dir)
