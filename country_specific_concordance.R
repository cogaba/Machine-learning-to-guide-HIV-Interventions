################################################################################
# country_specific_concordance.R
#
# Country-specific versus pooled predictor rankings (S1 Fig, S2 Fig, and the
# reported Kendall's W). For each sex, one XGBoost model (no resampling) is
# fit to each country's portion of the primary training split, with
# hyperparameters retuned by the same PSU-grouped cross-validation grid
# search; predictors are ranked by gain. The pooled ranking is read from
# DOWNSTREAM_RESULTS.xlsx (feature_importance sheet, XGBoost / none) so that
# it is the ranking shown in Figs 1-2.
#
# Concordance: Kendall's W across the five rankings (four countries + pooled)
# over the 26 predictors (chi-square with 25 df), and Spearman's rho between
# each country's ranking and the pooled ranking.
#
# Requires downstream_setup.R and 00_config.R in the working directory, and
# DOWNSTREAM_RESULTS.xlsx from run_stage_C.R.
#
# Output: COUNTRY_CONCORDANCE_RESULTS.xlsx
################################################################################

source("downstream_setup.R")

out_path_country <- artifact_path("COUNTRY_CONCORDANCE_RESULTS.xlsx")

XGB_GRID      <- expand.grid(max_depth = c(2, 3, 4), eta = c(0.01, 0.02, 0.03, 0.05))
RETUNE_K      <- 5
MIN_COUNTRY_N <- 30   

if (!file.exists(out_path)) {
  stop("DOWNSTREAM_RESULTS.xlsx not found at ", out_path,
       " -- run run_stage_C.R first so the pooled feature_importance sheet exists.")
}
pooled_importance_all <- read_excel(out_path, sheet = "feature_importance") %>%
  filter(model == "XGB", resampling == "none")

retune <- function(df, feats, grid) {
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
  dtr <- xgb.DMatrix(as.matrix(df[, feats]), label = as.numeric(as.character(df$hivstatusfinal)))
  prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
              eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
  m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
  imp <- xgb.importance(feature_names = feats, model = m)
  v <- setNames(imp$Gain, imp$Feature)
  full <- setNames(rep(0, length(feats)), feats)  
  full[names(v)] <- v
  full
}

# rank_matrix: predictors (rows) x rankers (columns), each column already a
# rank 1..n. m = number of rankers, n = number of predictors.
kendalls_w <- function(rank_matrix) {
  m <- ncol(rank_matrix); n <- nrow(rank_matrix)
  Rsum <- rowSums(rank_matrix)
  S <- sum((Rsum - mean(Rsum))^2)
  W <- 12 * S / (m^2 * (n^3 - n))
  chi_sq <- m * (n - 1) * W
  df <- n - 1
  list(W = W, chi_sq = chi_sq, df = df, p_value = pchisq(chi_sq, df = df, lower.tail = FALSE))
}

ranking_rows     <- list()
concordance_rows <- list()
heat_male <- NULL; heat_female <- NULL

for (sx in c(0, 1)) {
  lab   <- if (sx == 0) "male" else "female"
  feats <- if (sx == 0) MALE_FEATS else FEMALE_FEATS
  sp <- build_primary_split(dat, sx, feats)

  pooled_imp_sex <- pooled_importance_all %>% filter(sex == lab)
  pooled_vec <- setNames(rep(0, length(feats)), feats)
  pooled_vec[pooled_imp_sex$feature] <- pooled_imp_sex$gain

  countries <- sort(unique(sp$train[[COUNTRY_VAR]]))
  country_imp <- list()

  message(sprintf("\n==== %s ====", lab))
  for (ctry in countries) {
    d_c <- sp$train %>% filter(.data[[COUNTRY_VAR]] == ctry)
    if (nrow(d_c) < MIN_COUNTRY_N || length(unique(d_c$hivstatusfinal)) < 2) {
      message(sprintf("  [%s] skipped: n=%d, distinct classes=%d",
                       ctry, nrow(d_c), length(unique(d_c$hivstatusfinal))))
      next
    }
    message(sprintf("  [%s] retuning (n=%d, positives=%d)...",
                     ctry, nrow(d_c), sum(d_c$hivstatusfinal == "1")))
    hp_c <- retune(d_c, feats, XGB_GRID)
    imp_c <- xgb_importance(d_c, feats, hp_c)
    country_imp[[ctry]] <- imp_c

    ranking_rows[[paste(lab, ctry)]] <- tibble(
      sex = lab, country = ctry,
      hyperparameters = paste0("max_depth=", hp_c$max_depth, ", eta=", hp_c$eta),
      n = nrow(d_c), n_positive = sum(d_c$hivstatusfinal == "1"),
      top8 = paste(names(sort(imp_c, decreasing = TRUE))[1:8], collapse = " > ")
    )
    message(sprintf("  [%s] done: max_depth=%d eta=%.2f", ctry, hp_c$max_depth, hp_c$eta))
  }

  ranking_rows[[paste(lab, "pooled")]] <- tibble(
    sex = lab, country = "Pooled", hyperparameters = NA_character_,
    n = nrow(sp$train), n_positive = sum(sp$train$hivstatusfinal == "1"),
    top8 = paste(names(sort(pooled_vec, decreasing = TRUE))[1:8], collapse = " > ")
  )

  if (length(country_imp) < 2) {
    stop(sprintf("[%s] fewer than 2 countries had a usable fit -- cannot compute concordance.", lab))
  }

  all_rankers <- c(country_imp, list(Pooled = pooled_vec))
  rank_mat <- sapply(all_rankers, function(v) rank(-v[feats]))
  rownames(rank_mat) <- feats

  kw <- kendalls_w(rank_mat)
  message(sprintf("[%s] Kendall's W = %.3f, chi2(%d) = %.1f, p %s (m=%d rankers, n=%d predictors)",
                   lab, kw$W, kw$df, kw$chi_sq,
                   if (kw$p_value < 0.001) "< 0.001" else sprintf("= %.3f", kw$p_value),
                   ncol(rank_mat), nrow(rank_mat)))

  pooled_rank <- rank(-pooled_vec[feats])
  sp_rhos <- sapply(names(country_imp), function(ctry) {
    suppressWarnings(cor(rank(-country_imp[[ctry]][feats]), pooled_rank, method = "spearman"))
  })
  message(sprintf("[%s] Spearman rho vs. pooled, by country: %s",
                   lab, paste(sprintf("%s=%.3f", names(sp_rhos), sp_rhos), collapse = ", ")))

  concordance_rows[[lab]] <- tibble(
    sex = lab, m_rankers = ncol(rank_mat), n_predictors = nrow(rank_mat),
    kendall_W = round(kw$W, 3), chi_sq = round(kw$chi_sq, 1), df = kw$df,
    p_value = if (kw$p_value < 0.001) "<0.001" else as.character(round(kw$p_value, 3)),
    spearman_min = round(min(sp_rhos), 2), spearman_max = round(max(sp_rhos), 2),
    spearman_by_country = paste(sprintf("%s=%.3f", names(sp_rhos), sp_rhos), collapse = "; ")
  )

  heat_df <- as.data.frame(rank_mat) %>%
    tibble::rownames_to_column("feature") %>%
    tidyr::pivot_longer(-feature, names_to = "model", values_to = "rank") %>%
    mutate(sex = lab, .before = 1)
  if (lab == "male") heat_male <- heat_df else heat_female <- heat_df
}

ranking_out  <- bind_rows(ranking_rows)
concordance_out <- bind_rows(concordance_rows)
heatmap_out  <- bind_rows(heat_male, heat_female)

write_xlsx(list(
  country_model_summary = ranking_out,
  concordance_summary   = concordance_out,
  heatmap_rank_data     = heatmap_out
), out_path_country)

message("\nWritten to: ", out_path_country)
print(as.data.frame(concordance_out))
