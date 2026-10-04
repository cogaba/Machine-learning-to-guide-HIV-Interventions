################################################################################
# undiagnosed_population_sensitivity.R
#
# Sensitivity analysis restricting the sample to HIV-negative respondents
# plus HIV-positive respondents unaware of their status (aware == 2), by sex.
#
#   Part 1  Predictor-ranking concordance between the primary model and the
#           restricted model, each fit to its full sex-specific sample (no
#           held-out evaluation is involved), summarised by Spearman's rho
#           and Kendall's W.
#   Part 2  Held-out discrimination in the restricted population, using the
#           same PSU-grouped train/test split and nested threshold-selection
#           procedure as the primary analysis. The primary model's held-out
#           performance for comparison is in DOWNSTREAM_RESULTS.xlsx
#           (single_split sheet).
#
# Hyperparameters are retuned for the restricted sample because the
# restriction reduces the positive class from 1,904 to 382 (males) and from
# 4,385 to 628 (females).
#
# Requires downstream_setup.R and 00_config.R in the working directory.
#
# Output: UNDIAGNOSED_POPULATION_SENSITIVITY_RESULTS.xlsx
################################################################################

source("downstream_setup.R")

out_path_restrict <- artifact_path("UNDIAGNOSED_POPULATION_SENSITIVITY_RESULTS.xlsx")

XGB_GRID  <- expand.grid(max_depth = c(2, 3, 4), eta = c(0.01, 0.02, 0.03, 0.05))
RETUNE_K  <- 5

make_samples <- function(sex_code, feats) {
  d <- dat %>% filter(sex == sex_code) %>%
    mutate(hivstatusfinal = as.integer(hivstatusfinal))
  main <- d %>% select(hivstatusfinal, all_of(feats), svy_psu)
  restricted <- d %>%
    filter(hivstatusfinal == 0 | (hivstatusfinal == 1 & aware == 2)) %>%
    select(hivstatusfinal, all_of(feats), svy_psu)
  list(main = main, restricted = restricted)
}

# ==============================================================================
# Part 1: predictor-ranking concordance (main vs. restricted), full-sample fits
# ==============================================================================

# PSU-grouped cross-validated grid search on the sample passed in.
retune <- function(df, feats, grid) {
  df$hivstatusfinal <- factor(df$hivstatusfinal, levels = c("0", "1"))
  folds <- grouped_folds(df$svy_psu, k = RETUNE_K)
  best_auc <- -Inf; best_hp <- NULL
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    hp <- list(max_depth = g$max_depth, eta = g$eta)
    aucs <- sapply(folds, function(tr_idx) {
      te_idx <- setdiff(seq_len(nrow(df)), tr_idx)
      dtr <- xgb.DMatrix(as.matrix(df[tr_idx, feats]),
                          label = as.numeric(as.character(df$hivstatusfinal[tr_idx])))
      m <- xgb.train(list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                           eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8),
                      dtr, nrounds = 500, verbose = 0)
      prob <- predict(m, as.matrix(df[te_idx, feats]))
      truth <- df$hivstatusfinal[te_idx]
      as.numeric(auc(roc(as.numeric(as.character(truth)), prob, quiet = TRUE)))
    })
    m_auc <- mean(aucs)
    if (m_auc > best_auc) { best_auc <- m_auc; best_hp <- hp }
  }
  best_hp
}

xgb_importance <- function(df, feats, hp) {
  df$hivstatusfinal <- factor(df$hivstatusfinal, levels = c("0", "1"))
  dtr <- xgb.DMatrix(as.matrix(df[, feats]), label = as.numeric(as.character(df$hivstatusfinal)))
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  imp <- xgb.importance(feature_names = feats, model = m)
  setNames(imp$Gain, imp$Feature)
}

rank_concordance <- function(a, b) {
  common <- intersect(names(a), names(b))
  ra <- rank(-a[common]); rb <- rank(-b[common])
  sp <- suppressWarnings(cor(ra, rb, method = "spearman"))
  R <- cbind(ra, rb); n <- nrow(R)
  Rsum <- rowSums(R); S <- sum((Rsum - mean(Rsum))^2)
  W <- 12 * S / (2^2 * (n^3 - n))
  list(spearman = round(sp, 3), kendall_W = round(W, 3), n_features = n)
}
topn <- function(v, n = 8) paste(names(sort(v, decreasing = TRUE))[1:n], collapse = " > ")

ranking_results <- list()

# ==============================================================================
# Part 2: held-out predictive performance on the restricted population
# ==============================================================================

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

performance_results <- list()

for (sx in c(0, 1)) {
  lab   <- if (sx == 0) "male" else "female"
  feats <- if (sx == 0) MALE_FEATS else FEMALE_FEATS
  s <- make_samples(sx, feats)
  message(sprintf("\n==== %s ====", lab))
  message(sprintf("  main positives: %d | restricted positives (unaware): %d | negatives: %d",
                   sum(s$main$hivstatusfinal == 1), sum(s$restricted$hivstatusfinal == 1),
                   sum(s$main$hivstatusfinal == 0)))

  # ---- Part 1: ranking concordance, full-sample fits ----
  hp_xgb_main <- FINAL_HP[[lab]][["XGB"]][["none"]]
  message("  retuning XGB for the restricted sample (ranking analysis)...")
  hp_xgb_rest <- retune(s$restricted, feats, XGB_GRID)
  message(sprintf("  restricted XGB (ranking): max_depth=%d eta=%.2f", hp_xgb_rest$max_depth, hp_xgb_rest$eta))

  xg_main <- xgb_importance(s$main, feats, hp_xgb_main)
  xg_rest <- xgb_importance(s$restricted, feats, hp_xgb_rest)
  xg_c <- rank_concordance(xg_main, xg_rest)
  message(sprintf("  XGB ranking concordance (main vs restricted): Spearman %.3f | Kendall W %.3f", xg_c$spearman, xg_c$kendall_W))

  ranking_results[[lab]] <- tibble(
    sex = lab,
    model = "XGBoost",
    spearman = xg_c$spearman,
    kendall_W = xg_c$kendall_W,
    restricted_hp = paste0("max_depth=", hp_xgb_rest$max_depth, ", eta=", hp_xgb_rest$eta),
    main_top8 = topn(xg_main),
    restricted_top8 = topn(xg_rest)
  )

  # ---- Part 2: held-out performance in the restricted population ----
  dat_restricted <- dat %>%
    mutate(hivstatusfinal_int = as.integer(hivstatusfinal)) %>%
    filter(hivstatusfinal_int == 0 | (hivstatusfinal_int == 1 & aware == 2)) %>%
    select(-hivstatusfinal_int)

  sp <- build_primary_split(dat_restricted, sx, feats)
  train <- sp$train; test <- sp$test
  message(sprintf("  restricted split: %d train (%d positive) / %d test (%d positive)",
                   nrow(train), sum(train$hivstatusfinal == "1"),
                   nrow(test),  sum(test$hivstatusfinal == "1")))

  message("  retuning XGB for the restricted sample (held-out performance)...")
  hp_perf <- retune(train %>% mutate(hivstatusfinal = as.integer(as.character(hivstatusfinal))),
                     feats, XGB_GRID)
  message(sprintf("  restricted XGB (performance): max_depth=%d eta=%.2f", hp_perf$max_depth, hp_perf$eta))

  thr  <- nested_threshold_feats(train, feats, hp_perf, k = THRESH_K)
  prob_test <- fit_xgb(train, test, feats, hp_perf)
  truth <- test$hivstatusfinal
  auc_val <- as.numeric(auc(roc(as.numeric(as.character(truth)), prob_test, quiet = TRUE)))
  met <- classification_metrics(prob_test, truth, thr)
  brier <- brier_score(prob_test, truth)

  performance_results[[lab]] <- bind_cols(
    tibble(sex = lab, population = "undiagnosed_restricted",
           n_train = nrow(train), n_train_positive = sum(train$hivstatusfinal == "1"),
           n_test = nrow(test), n_test_positive = sum(test$hivstatusfinal == "1"),
           hyperparameters = paste0("max_depth=", hp_perf$max_depth, ", eta=", hp_perf$eta),
           threshold = round(thr, 4), auc = round(auc_val, 4), brier = round(brier, 4)),
    met)
  message(sprintf("  [%s restricted] AUC=%.3f balAcc=%.3f", lab, auc_val, met$balanced_acc))
}

ranking_out <- bind_rows(ranking_results)
performance_out <- bind_rows(performance_results)

write_xlsx(list(undiagnosed_ranking = ranking_out,
                 undiagnosed_performance = performance_out),
           out_path_restrict)
message("\nWritten to: ", out_path_restrict)
print(as.data.frame(ranking_out[, c("sex", "model", "spearman", "kendall_W", "restricted_hp")]))
print(as.data.frame(performance_out[, c("sex", "n_test", "n_test_positive", "hyperparameters",
                                         "auc", "sensitivity", "specificity", "balanced_acc", "brier")]))
