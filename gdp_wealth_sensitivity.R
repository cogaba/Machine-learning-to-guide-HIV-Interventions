################################################################################
# gdp_wealth_sensitivity.R
#
# Sensitivity analysis for household wealth coding. The primary model uses
# each respondent's original within-country wealth quintile (low/mid/high), this
# script constructs an alternative GDP-adjusted wealth quintile and refits
# the primary model with it in place of the within-country measure, to
# assess whether the choice of wealth measure materially affects primary
# model performance.
#
# The GDP-adjusted quintile multiplies each respondent's continuous,
# PCA-derived wealth score (wealthscorecont) by a country-level
# macroeconomic adjuster (that country's GDP per capita, PPP, relative to
# the four-country median), then re-divides the adjusted scores into five
# equally sized quintiles pooled across all four countries. 

# Both the within-country (primary) and GDP-adjusted configurations are
# fit using the primary model's tuned hyperparameters (FINAL_HP[[sex]]$XGB
# $none), no resampling, on the same primary train/test split
# (build_primary_split), with the classification threshold re-derived by the
# same nested, PSU-grouped procedure used throughout the primary analysis.
#
# Requires downstream_setup.R and 00_config.R in the working directory, and
# wealth_scores_by_country.xlsx (personid, country, wealthscorecont,
# wealthquintile from each country's household file; one sheet per country:
# Mal, Moz, TZ, Ug) in the data folder.
#
# Output: GDP_WEALTH_SENSITIVITY_RESULTS.xlsx
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(xgboost); library(pROC); library(writexl)
})

source("downstream_setup.R")

out_path <- artifact_path("GDP_WEALTH_SENSITIVITY_RESULTS.xlsx")

# ==========================================================================
# 1. Build the GDP-adjusted wealth quintile.
# ==========================================================================
wealth_file <- data_path("wealth_scores_by_country.xlsx")
country_sheets <- c("Moz", "Mal", "TZ", "Ug")
wealth_raw <- bind_rows(lapply(country_sheets, function(s) {
  read_excel(wealth_file, sheet = s) %>%
    select(personid, country, wealthscorecont, wealthquintile) %>%
    mutate(personid = as.character(personid))
}))

# GDP per capita, PPP (current international $), 2021, World Development Indicators
gdp_pc <- tibble(
  country = c("Malawi", "Mozambique", "Tanzania", "Uganda"),
  gdp_2021 = c(1687.593582, 1457.235409, 3493.083008, 2684.920359)
)
median_gdp <- median(gdp_pc$gdp_2021)
gdp_pc <- gdp_pc %>% mutate(macro_adjuster = gdp_2021 / median_gdp)

wealth_adj <- wealth_raw %>%
  mutate(country = case_when(
    country %in% c("Mozambique", "MZ", "Moz") ~ "Mozambique",
    country %in% c("Malawi", "MW", "Mal")      ~ "Malawi",
    country %in% c("Tanzania", "TZ")           ~ "Tanzania",
    country %in% c("Uganda", "UG", "Ug")       ~ "Uganda",
    TRUE ~ country
  )) %>%
  left_join(gdp_pc %>% select(country, macro_adjuster), by = "country") %>%
  mutate(adjusted_score = wealthscorecont * macro_adjuster,
         wealthquintile_gdp = ntile(adjusted_score, 5))

dat <- dat %>%
  mutate(personid = as.character(personid)) %>%
  left_join(wealth_adj %>% select(personid, wealthquintile_gdp), by = "personid") %>%
  mutate(low_wealth_gdp  = as.integer(wealthquintile_gdp %in% c(1, 2)),
         mid_wealth_gdp  = as.integer(wealthquintile_gdp %in% c(3, 4)),
         high_wealth_gdp = as.integer(wealthquintile_gdp == 5))

n_matched <- sum(!is.na(dat$wealthquintile_gdp))
message(sprintf("Join check: %d / %d analytic-sample rows (%.1f%%) matched to a GDP-adjusted wealth score.",
                 n_matched, nrow(dat), 100 * n_matched / nrow(dat)))
if (n_matched / nrow(dat) < 0.95) {
  stop("Fewer than 95% of rows matched on personid -- check personid format/consistency ",
       "between analytic_final.xlsx and the wealth-score file before proceeding.")
}

# ==========================================================================
# 2. Fit and evaluate the primary model with each wealth measure.
# ==========================================================================
fit_xgb <- function(ts, nd, feats, hp) {
  dtr <- xgb.DMatrix(as.matrix(ts[, feats]),
                      label = as.numeric(as.character(ts$hivstatusfinal)))
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  predict(m, as.matrix(nd[, feats]))
}

nested_threshold_feats <- function(train, feats, hp, k) {
  folds <- grouped_folds(train$svy_psu, k = k)
  oof_prob <- rep(NA_real_, nrow(train))
  for (tr_idx in folds) {
    te_idx <- setdiff(seq_len(nrow(train)), tr_idx)
    oof_prob[te_idx] <- fit_xgb(train[tr_idx, ], train[te_idx, ], feats, hp)
  }
  r <- roc(as.numeric(as.character(train$hivstatusfinal)), oof_prob, quiet = TRUE)
  as.numeric(coords(r, "best", ret = "threshold", best.method = "youden", transpose = TRUE))[1]
}

run_wealth_config <- function(sex_label, sx, base_feats) {
  hp <- FINAL_HP[[sex_label]]$XGB$none
  sp <- build_primary_split(dat, sx, base_feats)   # same split for both wealth measures
  train <- sp$train; test <- sp$test

  feats_within <- c(setdiff(base_feats, c("low_wealth", "mid_wealth", "high_wealth")),
                     "low_wealth", "mid_wealth", "high_wealth")
  feats_gdp    <- c(setdiff(base_feats, c("low_wealth", "mid_wealth", "high_wealth")),
                     "low_wealth_gdp", "mid_wealth_gdp", "high_wealth_gdp")

  rows <- list()
  for (cfg in list(list(n = "within_country_primary", f = feats_within),
                    list(n = "gdp_adjusted",           f = feats_gdp))) {
    thr  <- nested_threshold_feats(train, cfg$f, hp, k = THRESH_K)
    prob <- fit_xgb(train, test, cfg$f, hp)
    auc_val <- as.numeric(auc(roc(as.numeric(as.character(test$hivstatusfinal)), prob, quiet = TRUE)))
    met <- classification_metrics(prob, test$hivstatusfinal, thr)
    rows[[cfg$n]] <- bind_cols(
      tibble(sex = sex_label, wealth_measure = cfg$n, threshold = round(thr, 4), auc = round(auc_val, 4)),
      met)
  }
  out <- bind_rows(rows)
  message(sprintf("\n[%s] within-country AUC=%.3f, GDP-adjusted AUC=%.3f (diff=%+.3f)",
                   sex_label, out$auc[1], out$auc[2], out$auc[2] - out$auc[1]))
  out
}

male_out   <- run_wealth_config("male",   0, MALE_FEATS)
female_out <- run_wealth_config("female", 1, FEMALE_FEATS)

write_xlsx(list(gdp_wealth_sensitivity = bind_rows(male_out, female_out)), out_path)
message("\nWritten to: ", out_path)
