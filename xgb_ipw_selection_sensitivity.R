################################################################################
# xgb_ipw_selection_sensitivity.R
#
# Sensitivity analysis for complete-case selection (primary model). Uses the
# same stabilised inverse-probability-of-inclusion weight and country-
# rescaled design weight as ipw_selection_sensitivity.R, applied as the
# training observation weight of the primary XGBoost model (no resampling,
# tuned hyperparameters). The classification threshold is re-derived by the
# nested PSU-grouped procedure within each branch (unweighted and weighted),
# and single-split test performance is compared.
#
# Requires downstream_setup.R and 00_config.R in the working directory, and
# master_merged_all.xlsx in the data folder.
#
# Output: XGB_IPW_SELECTION_SENSITIVITY_RESULTS.xlsx
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(xgboost); library(pROC); library(writexl)
})

source("downstream_setup.R")

out_path <- artifact_path("XGB_IPW_SELECTION_SENSITIVITY_RESULTS.xlsx")

# 1. Stabilised inverse-probability-of-inclusion weight (as in
#    ipw_selection_sensitivity.R).
master <- read_excel(data_path("master_merged_all.xlsx"), guess_max = 200000) %>%
  mutate(personid = as.character(personid))
fin_ids <- as.character(dat$personid)

elig <- master %>%
  filter(!is.na(hivstatusfinal), age >= 15) %>%
  mutate(
    included      = as.integer(personid %in% fin_ids),
    low_education = as.integer(education %in% c(1, 2)),
    wealthQ12     = as.integer(wealthquintile %in% c(1, 2)),
    hivpos        = as.integer(hivstatusfinal == 1),
    female        = as.integer(gender == 2)
  )

sel_model <- glm(
  included ~ age + female + factor(country) + low_education + wealthQ12 + hivpos,
  data = elig, family = binomial()
)

incl_rate     <- mean(elig$included)
elig$p_incl   <- predict(sel_model, type = "response")
elig$ipw_stab <- incl_rate / elig$p_incl

# Training weight: country-rescaled design weight x inclusion weight.
dat <- dat %>%
  mutate(personid = as.character(personid)) %>%
  left_join(elig %>% select(personid, ipw_stab), by = "personid") %>%
  group_by(sex, country) %>%
  mutate(design_weight   = svy_weight * (dplyr::n() / sum(svy_weight)),
         combined_weight = design_weight * ipw_stab) %>%
  ungroup()

# 2. XGBoost fit/predict with optional observation weights; hyperparameters
#    are the primary model's tuned values.
fit_xgb <- function(ts, nd, feats, hp, w = NULL) {
  dtr <- xgb.DMatrix(as.matrix(ts[, feats]),
                      label  = as.numeric(as.character(ts$hivstatusfinal)),
                      weight = w)
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  predict(m, as.matrix(nd[, feats]))
}

# Nested PSU-grouped threshold selection (as nested_threshold() in
# downstream_setup.R), carrying the training weight into each inner fold.
nested_threshold_w <- function(train, feats, hp, k, weight_col = NULL) {
  folds <- grouped_folds(train$svy_psu, k = k)
  oof_prob <- rep(NA_real_, nrow(train))
  for (tr_idx in folds) {
    te_idx <- setdiff(seq_len(nrow(train)), tr_idx)
    w_tr <- if (is.null(weight_col)) NULL else train[[weight_col]][tr_idx]
    oof_prob[te_idx] <- fit_xgb(train[tr_idx, ], train[te_idx, ], feats, hp, w = w_tr)
  }
  r <- roc(as.numeric(as.character(train$hivstatusfinal)), oof_prob, quiet = TRUE)
  as.numeric(coords(r, "best", ret = "threshold", best.method = "youden", transpose = TRUE))[1]
}

# 3. Unweighted vs. IPW-weighted primary model on the primary split.
run_sensitivity <- function(sex_label, sx, feats) {
  hp <- FINAL_HP[[sex_label]]$XGB$none
  sp <- build_primary_split(dat, sx, feats)
  train <- sp$train; test <- sp$test

  thr_base <- nested_threshold_w(train, feats, hp, k = THRESH_K, weight_col = NULL)
  prob_base <- fit_xgb(train, test, feats, hp, w = NULL)
  auc_base <- as.numeric(auc(roc(as.numeric(as.character(test$hivstatusfinal)), prob_base, quiet = TRUE)))
  met_base <- classification_metrics(prob_base, test$hivstatusfinal, thr_base)

  thr_ipw <- nested_threshold_w(train, feats, hp, k = THRESH_K, weight_col = "combined_weight")
  prob_ipw <- fit_xgb(train, test, feats, hp, w = train$combined_weight)
  auc_ipw <- as.numeric(auc(roc(as.numeric(as.character(test$hivstatusfinal)), prob_ipw, quiet = TRUE)))
  met_ipw <- classification_metrics(prob_ipw, test$hivstatusfinal, thr_ipw)

  out <- tibble(
    sex = sex_label,
    metric = c("threshold", "auc", "sensitivity", "specificity", "ppv", "balanced_acc"),
    baseline = c(thr_base, auc_base, met_base$sensitivity, met_base$specificity,
                 met_base$ppv, met_base$balanced_acc),
    ipw_weighted = c(thr_ipw, auc_ipw, met_ipw$sensitivity, met_ipw$specificity,
                      met_ipw$ppv, met_ipw$balanced_acc)
  )
  out$abs_diff <- round(out$ipw_weighted - out$baseline, 4)
  out$baseline <- round(out$baseline, 4)
  out$ipw_weighted <- round(out$ipw_weighted, 4)

  message(sprintf("\n[%s] AUC baseline=%.3f, IPW-weighted=%.3f (diff=%+.3f)",
                   sex_label, auc_base, auc_ipw, auc_ipw - auc_base))
  out
}

male_out   <- run_sensitivity("male",   0, MALE_FEATS)
female_out <- run_sensitivity("female", 1, FEMALE_FEATS)

write_xlsx(list(xgb_ipw_sensitivity = bind_rows(male_out, female_out)), out_path)
message("\nWritten to: ", out_path)
