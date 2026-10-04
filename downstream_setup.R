################################################################################
# downstream_setup.R
#
# Shared setup for the downstream evaluation: data, feature lists, tuned
# hyperparameters, and helper functions. Sourced by run_stage_A.R,
# run_stage_B.R, run_stage_C.R and the sensitivity-analysis scripts; not run
# directly.
#
# Eight configurations are evaluated: male/female x Random Forest/XGBoost x
# SMOTE (target minority:majority ratio 0.4)/no resampling. All splits,
# cross-validation folds, nested threshold-selection folds, and bootstrap
# replicates are grouped by primary sampling unit (svy_psu). SMOTE, where
# used, is applied inside each training fold only.
#
#   run_stage_A.R  single-split test performance, PSU-grouped 5-fold CV,
#                  leave-one-country-out
#   run_stage_B.R  cluster (PSU) bootstrap: each replicate resamples PSUs,
#                  re-selects hyperparameters among the top-3 tuning-grid
#                  candidates, re-derives the threshold, refits, and
#                  evaluates on the test set
#   run_stage_C.R  Platt recalibration, calibration-plot data, feature
#                  importance
#
# Each stage writes its own sheets to DOWNSTREAM_RESULTS.xlsx.
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(ranger); library(xgboost); library(smotefamily)
  library(caret); library(pROC); library(writexl)
})

source("00_config.R")

COUNTRY_VAR     <- "country"
out_path        <- artifact_path("DOWNSTREAM_RESULTS.xlsx")
checkpoint_path <- artifact_path("bootstrap_checkpoint.rds")

dat <- read_excel(data_path("analytic_final.xlsx"), guess_max = 200000)

COMMON_FEATS <- c("Urban_rural","condomlastsex12months","Num_sexpartners_life","work_12months",
                   "alcoholic_freq","ever_married","first_sex_at_age","sex_12_months",
                   "Members_sleep_here","HH_head_gender","HH_Electricity","HH_radio","HH_TV",
                   "HH_phone","avoidpreg","education",
                   "agecat_15_24","agecat_25_34","agecat_35_44","agecat_45_54","agecat_55_64",
                   "agecat_65plus","low_wealth","mid_wealth","high_wealth")
MALE_FEATS   <- c(COMMON_FEATS, "Male_Circumcision")
FEMALE_FEATS <- c(COMMON_FEATS, "live_births")

TARGET_RATIO <- 0.4

# Tuned hyperparameters per configuration, selected by mean cross-validated
# AUC under PSU-grouped 5-fold CV on the training set
# (primary_model_selection.R; PRIMARY_MODEL_SELECTION.xlsx, "comparison" sheet).
# Grid: RF mtry 1-4, min_node_size 10-35; XGBoost max_depth 2-4, eta 0.01-0.05.
# Trees / boosting rounds fixed at 500.
FINAL_HP <- list(
  male = list(
    RF  = list(SMOTE_0.4 = list(mtry = 3, min_node_size = 35), none = list(mtry = 2, min_node_size = 35)),
    XGB = list(SMOTE_0.4 = list(max_depth = 4, eta = 0.01),    none = list(max_depth = 3, eta = 0.02))
  ),
  female = list(
    RF  = list(SMOTE_0.4 = list(mtry = 2, min_node_size = 35), none = list(mtry = 2, min_node_size = 20)),
    XGB = list(SMOTE_0.4 = list(max_depth = 4, eta = 0.05),    none = list(max_depth = 3, eta = 0.03))
  )
)

# Top-3 tuning-grid candidates per configuration (by mean cross-validated
# AUC; PRIMARY_MODEL_SELECTION.xlsx, "tuning_grid" sheet), re-selected among
# within each Stage B bootstrap replicate.
CANDIDATE_HP <- list(
  male = list(
    RF = list(
      SMOTE_0.4 = list(list(mtry = 3, min_node_size = 35), list(mtry = 3, min_node_size = 30), list(mtry = 2, min_node_size = 35)),
      none      = list(list(mtry = 2, min_node_size = 35), list(mtry = 2, min_node_size = 30), list(mtry = 2, min_node_size = 15))
    ),
    XGB = list(
      SMOTE_0.4 = list(list(max_depth = 4, eta = 0.01), list(max_depth = 4, eta = 0.05), list(max_depth = 3, eta = 0.05)),
      none      = list(list(max_depth = 3, eta = 0.02), list(max_depth = 2, eta = 0.03), list(max_depth = 4, eta = 0.01))
    )
  ),
  female = list(
    RF = list(
      SMOTE_0.4 = list(list(mtry = 2, min_node_size = 35), list(mtry = 2, min_node_size = 30), list(mtry = 2, min_node_size = 25)),
      none      = list(list(mtry = 2, min_node_size = 20), list(mtry = 2, min_node_size = 25), list(mtry = 2, min_node_size = 35))
    ),
    XGB = list(
      SMOTE_0.4 = list(list(max_depth = 4, eta = 0.05), list(max_depth = 3, eta = 0.05), list(max_depth = 4, eta = 0.01)),
      none      = list(list(max_depth = 3, eta = 0.03), list(max_depth = 3, eta = 0.02), list(max_depth = 4, eta = 0.02))
    )
  )
)

CV_K            <- 5    # Stage A cross-validation folds
THRESH_K        <- 5    # nested threshold-selection folds for Stage A/C's shared threshold
BOOTSTRAP_REPS  <- 150  # Stage B replicates per configuration
BOOT_TUNE_K     <- 2    # inner folds for re-selecting among CANDIDATE_HP within each replicate
BOOT_THRESH_K   <- 2    # inner folds for re-deriving the threshold within each replicate

INCLUDE_SMOTE_IN_BOOTSTRAP <- TRUE   # FALSE restricts Stage B to the no-resampling configurations

# SMOTE applied to the training rows passed in; NA target_ratio = no resampling.
smote_or_none <- function(train, feats, target_ratio, k = 5) {
  if (is.na(target_ratio)) return(train)
  y <- as.numeric(as.character(train$hivstatusfinal))
  n_min <- sum(y == 1); n_maj <- sum(y == 0)
  dup_size <- max(1, ceiling(target_ratio * n_maj / n_min - 1))
  sm <- SMOTE(X = train[, feats], target = y, K = k, dup_size = dup_size)
  out <- sm$data
  out$hivstatusfinal <- factor(ifelse(out$class == 1, "1", "0"), levels = c("0", "1"))
  out$class <- NULL
  out
}

fit_rf_prob <- function(ts, nd, feats, hp, importance = FALSE) {
  m <- ranger(hivstatusfinal ~ ., data = ts[, c("hivstatusfinal", feats)],
              probability = TRUE, num.trees = 500, mtry = hp$mtry,
              min.node.size = hp$min_node_size,
              importance = if (importance) "impurity" else "none")
  list(prob = predict(m, data = nd[, feats])$predictions[, "1"], model = m)
}
fit_xgb_prob <- function(ts, nd, feats, hp) {
  dtr <- xgb.DMatrix(as.matrix(ts[, feats]), label = as.numeric(as.character(ts$hivstatusfinal)))
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  list(prob = predict(m, as.matrix(nd[, feats])), model = m)
}
# Returns function(train_raw, test, feats): resample the training rows (if
# applicable), fit, and predict. Used for CV folds, LOCO, nested threshold
# folds, and bootstrap replicates alike.
make_resampling_fit_fn <- function(model_name, hp, ratio) {
  function(train_raw, test, feats) {
    ts <- smote_or_none(train_raw, feats, ratio)
    if (model_name == "RF") fit_rf_prob(ts, test, feats, hp)$prob
    else fit_xgb_prob(ts, test, feats, hp)$prob
  }
}
classification_metrics <- function(prob, truth, threshold) {
  pred    <- factor(ifelse(prob >= threshold, "1", "0"), levels = c("0", "1"))
  truth_f <- factor(truth, levels = c("0", "1"))
  cm  <- table(pred, truth_f)
  TP <- cm["1","1"]; FP <- cm["1","0"]; TN <- cm["0","0"]; FN <- cm["0","1"]
  sens <- TP / (TP + FN); spec <- TN / (TN + FP)
  ppv  <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  tibble(sensitivity = sens, specificity = spec, ppv = ppv, balanced_acc = (sens + spec) / 2)
}
brier_score <- function(prob, truth) mean((prob - as.numeric(as.character(truth)))^2)

# Cox calibration intercept and slope; NA if the fit fails on a small
# held-out group.
calibration_stats <- function(prob, truth) {
  eps <- 1e-6
  p <- pmin(pmax(prob, eps), 1 - eps)
  y <- as.numeric(as.character(truth))
  slope <- tryCatch(coef(glm(y ~ logit_p, data = data.frame(y = y, logit_p = qlogis(p)),
                              family = binomial()))[2], error = function(e) NA_real_)
  intercept <- tryCatch(coef(glm(y ~ 1, offset = qlogis(p), data = data.frame(y = y),
                                  family = binomial()))[1], error = function(e) NA_real_)
  tibble(calib_intercept = as.numeric(intercept), calib_slope = as.numeric(slope))
}

grouped_folds <- function(psu_vec, k) groupKFold(psu_vec, k = k)  # training indices per fold

nested_threshold <- function(train, feats, fit_fn, k) {
  folds <- grouped_folds(train$svy_psu, k = k)
  oof_prob <- rep(NA_real_, nrow(train))
  for (tr_idx in folds) {
    te_idx <- setdiff(seq_len(nrow(train)), tr_idx)
    oof_prob[te_idx] <- fit_fn(train[tr_idx, ], train[te_idx, ], feats)
  }
  r <- roc(as.numeric(as.character(train$hivstatusfinal)), oof_prob, quiet = TRUE)
  as.numeric(coords(r, "best", ret = "threshold", best.method = "youden", transpose = TRUE))[1]
}

# Primary PSU-grouped 80/20 train/test split (seed 42; fold 1 of 5 grouped
# folds is the training set), identical to primary_model_selection.R.
build_primary_split <- function(dat, sx, feats) {
  set.seed(42)
  d <- dat %>% filter(sex == sx) %>%
    mutate(hivstatusfinal = factor(as.integer(hivstatusfinal), levels = c("0", "1")))
  split_folds <- grouped_folds(d$svy_psu, k = 5)
  train_idx <- split_folds[[1]]
  test_idx  <- setdiff(seq_len(nrow(d)), train_idx)
  list(full = d, train = d[train_idx, ], test = d[test_idx, ], feats = feats)
}

append_sheets <- function(path, new_sheets) {
  prior <- list()
  if (file.exists(path)) {
    sheet_names <- readxl::excel_sheets(path)
    prior <- lapply(sheet_names, function(s) read_excel(path, sheet = s))
    names(prior) <- sheet_names
  }
  for (nm in names(new_sheets)) prior[[nm]] <- new_sheets[[nm]]
  write_xlsx(prior, path)
}

CONFIGS <- list(
  list(sex = "male",   sx = 0, model = "RF",  feats = MALE_FEATS,   resampling = "SMOTE_0.4", ratio = TARGET_RATIO),
  list(sex = "male",   sx = 0, model = "RF",  feats = MALE_FEATS,   resampling = "none",      ratio = NA_real_),
  list(sex = "male",   sx = 0, model = "XGB", feats = MALE_FEATS,   resampling = "SMOTE_0.4", ratio = TARGET_RATIO),
  list(sex = "male",   sx = 0, model = "XGB", feats = MALE_FEATS,   resampling = "none",      ratio = NA_real_),
  list(sex = "female", sx = 1, model = "RF",  feats = FEMALE_FEATS, resampling = "SMOTE_0.4", ratio = TARGET_RATIO),
  list(sex = "female", sx = 1, model = "RF",  feats = FEMALE_FEATS, resampling = "none",      ratio = NA_real_),
  list(sex = "female", sx = 1, model = "XGB", feats = FEMALE_FEATS, resampling = "SMOTE_0.4", ratio = TARGET_RATIO),
  list(sex = "female", sx = 1, model = "XGB", feats = FEMALE_FEATS, resampling = "none",      ratio = NA_real_)
)

