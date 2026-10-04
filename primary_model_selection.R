################################################################################
# primary_model_selection.R
#
# Tunes and compares the four candidate configurations per sex -- Random
# Forest and XGBoost, each with SMOTE (minority:majority ratio 0.4) and
# without resampling -- on the primary PSU-grouped 80/20 train/test split.
# Hyperparameters are tuned by grid search with PSU-grouped 5-fold
# cross-validation on the training set (selection by mean AUC); the
# classification threshold is selected by Youden's J on out-of-fold training
# predictions; each configuration is evaluated once on the test set.
#
# The tuned hyperparameters are carried into downstream_setup.R (FINAL_HP and
# CANDIDATE_HP).
#
# Requires 00_config.R in the working directory.
#
# Output: PRIMARY_MODEL_SELECTION.xlsx (tuning_grid, comparison)
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(ranger); library(xgboost); library(smotefamily)
  library(caret); library(pROC); library(writexl)
})
set.seed(42)
RUN_START <- Sys.time()

source("00_config.R")
out_path <- artifact_path("PRIMARY_MODEL_SELECTION.xlsx")

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
TUNE_K       <- 5
INNER_K      <- 5
RF_GRID  <- expand.grid(mtry = c(1, 2, 3, 4), min_node_size = c(10, 15, 20, 25, 30, 35))
XGB_GRID <- expand.grid(max_depth = c(2, 3, 4), eta = c(0.01, 0.02, 0.03, 0.05))

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

fit_rf_prob <- function(ts, nd, feats, hp) {
  m <- ranger(hivstatusfinal ~ ., data = ts[, c("hivstatusfinal", feats)],
              probability = TRUE, num.trees = 500, mtry = hp$mtry, min.node.size = hp$min_node_size)
  predict(m, data = nd[, feats])$predictions[, "1"]
}
fit_xgb_prob <- function(ts, nd, feats, hp) {
  dtr <- xgb.DMatrix(as.matrix(ts[, feats]), label = as.numeric(as.character(ts$hivstatusfinal)))
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  predict(m, as.matrix(nd[, feats]))
}
make_fit_fn <- function(model_name, hp) {
  if (model_name == "RF") function(ts, nd, feats) fit_rf_prob(ts, nd, feats, hp)
  else function(ts, nd, feats) fit_xgb_prob(ts, nd, feats, hp)
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

grouped_folds <- function(psu_vec, k = 5) groupKFold(psu_vec, k = k)  # training indices per fold

tune_hyperparams <- function(train, feats, model_name, target_ratio, grid) {
  best_auc <- -Inf; best_hp <- NULL; grid_rows <- list()
  folds <- grouped_folds(train$svy_psu, k = TUNE_K)
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    hp <- if (model_name == "RF") list(mtry = g$mtry, min_node_size = g$min_node_size)
          else list(max_depth = g$max_depth, eta = g$eta)
    fit_fn <- make_fit_fn(model_name, hp)
    aucs <- sapply(folds, function(tr_idx) {
      te_idx <- setdiff(seq_len(nrow(train)), tr_idx)
      ts <- smote_or_none(train[tr_idx, ], feats, target_ratio)
      prob <- fit_fn(ts, train[te_idx, ], feats)
      as.numeric(auc(roc(as.numeric(as.character(train$hivstatusfinal[te_idx])), prob, quiet = TRUE)))
    })
    mean_auc <- mean(aucs)
    grid_rows[[i]] <- c(as.list(g), mean_cv_auc = mean_auc)
    if (mean_auc > best_auc) { best_auc <- mean_auc; best_hp <- hp }
  }
  list(best_hp = best_hp, best_auc = best_auc, grid = bind_rows(lapply(grid_rows, as_tibble)))
}

nested_threshold <- function(train, feats, fit_fn, target_ratio) {
  folds <- grouped_folds(train$svy_psu, k = INNER_K)
  oof_prob <- rep(NA_real_, nrow(train))
  for (tr_idx in folds) {
    te_idx <- setdiff(seq_len(nrow(train)), tr_idx)
    ts <- smote_or_none(train[tr_idx, ], feats, target_ratio)
    oof_prob[te_idx] <- fit_fn(ts, train[te_idx, ], feats)
  }
  r <- roc(as.numeric(as.character(train$hivstatusfinal)), oof_prob, quiet = TRUE)
  as.numeric(coords(r, "best", ret = "threshold", best.method = "youden", transpose = TRUE))[1]
}

# Primary split per sex, then tune and evaluate the four configurations.
all_grid_rows <- list()
comparison_rows <- list()

for (sx in c(0, 1)) {
  sex_label <- if (sx == 0) "male" else "female"
  feats <- if (sx == 0) MALE_FEATS else FEMALE_FEATS

  set.seed(42)
  d <- dat %>% filter(sex == sx) %>%
    mutate(hivstatusfinal = factor(as.integer(hivstatusfinal), levels = c("0", "1")))

  # PSU-grouped 80/20 split (seed 42; fold 1 of 5 grouped folds is the training set)
  split_folds <- grouped_folds(d$svy_psu, k = 5)
  train_idx <- split_folds[[1]]
  test_idx  <- setdiff(seq_len(nrow(d)), train_idx)
  train <- d[train_idx, ]; test <- d[test_idx, ]

  message(sprintf(
    "\n[%s] PSU-grouped split: train n=%d (%.1f%% positive), test n=%d (%.1f%% positive)",
    sex_label, nrow(train), 100 * mean(train$hivstatusfinal == "1"),
    nrow(test), 100 * mean(test$hivstatusfinal == "1")))

  for (model_name in c("RF", "XGB")) {
    grid <- if (model_name == "RF") RF_GRID else XGB_GRID
    for (resample_label_ratio in list(list(label = "SMOTE_0.4", ratio = TARGET_RATIO),
                                       list(label = "none",      ratio = NA_real_))) {
      label <- resample_label_ratio$label; ratio <- resample_label_ratio$ratio

      message(sprintf("[%s / %s / %s] tuning...", sex_label, model_name, label))
      tuned <- tune_hyperparams(train, feats, model_name, ratio, grid)
      all_grid_rows[[paste(sex_label, model_name, label)]] <-
        tuned$grid %>% mutate(sex = sex_label, model = model_name, resampling = label, .before = 1)

      fit_fn <- make_fit_fn(model_name, tuned$best_hp)
      thr <- nested_threshold(train, feats, fit_fn, ratio)

      ts_final <- smote_or_none(train, feats, ratio)
      set.seed(42)
      prob <- fit_fn(ts_final, test, feats)
      auc_val <- as.numeric(auc(roc(as.numeric(as.character(test$hivstatusfinal)), prob, quiet = TRUE)))
      met <- classification_metrics(prob, test$hivstatusfinal, thr)

      hp_str <- paste(names(tuned$best_hp), unlist(tuned$best_hp), sep = "=", collapse = ", ")
      comparison_rows[[paste(sex_label, model_name, label)]] <- bind_cols(
        tibble(sex = sex_label, model = model_name, resampling = label,
               tuned_hyperparameters = hp_str, threshold = thr, auc = auc_val),
        met)
      message(sprintf("[%s / %s / %s] balAcc=%.3f  AUC=%.3f  (hp: %s)",
                       sex_label, model_name, label, met$balanced_acc, auc_val, hp_str))
    }
  }
}

tuning_grid_full <- bind_rows(all_grid_rows)
comparison <- bind_rows(comparison_rows)

write_xlsx(list(tuning_grid = tuning_grid_full, comparison = comparison), out_path)

message(sprintf("\n===== COMPLETE. Elapsed: %.1f min =====",
                 as.numeric(difftime(Sys.time(), RUN_START, units = "mins"))))
message("Written to: ", out_path)
message("\n--- Comparison ---")
print(as.data.frame(comparison), digits = 4)
