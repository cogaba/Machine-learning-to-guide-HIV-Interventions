################################################################################
# run_stage_A.R
#
# Stage A: single-split test performance, PSU-grouped 5-fold cross-validation,
# and leave-one-country-out validation, for all eight configurations.
#
# Requires downstream_setup.R and 00_config.R in the working directory.
################################################################################

source("downstream_setup.R")

################################################################################
# STAGE A -- single split, PSU-grouped 5-fold CV, leave-one-country-out
################################################################################
{
  if (!COUNTRY_VAR %in% names(dat)) {
    stop(sprintf(
      "COUNTRY_VAR = '%s' not found in analytic_final.xlsx. Columns containing 'ountry': %s",
      COUNTRY_VAR, paste(grep("ountry", names(dat), value = TRUE, ignore.case = TRUE), collapse = ", ")))
  }

  STAGE_A_START <- Sys.time()
  single_rows <- list(); cv_rows <- list(); loco_rows <- list(); by_country_rows <- list()

  for (cfg in CONFIGS) {
    label <- paste(cfg$sex, cfg$model, cfg$resampling)
    message(sprintf("\n[Stage A: %s] building split and fitting final model...", label))
    hp <- FINAL_HP[[cfg$sex]][[cfg$model]][[cfg$resampling]]
    sp <- build_primary_split(dat, cfg$sx, cfg$feats)
    fit_fn <- make_resampling_fit_fn(cfg$model, hp, cfg$ratio)

    # Threshold selected once by nested PSU-grouped CV on the training set
    # (Youden's J on out-of-fold predictions) and applied to the single-split,
    # CV, and LOCO metrics below.
    thr <- nested_threshold(sp$train, cfg$feats, fit_fn, k = THRESH_K)

    # --- single split ---
    prob_test <- fit_fn(sp$train, sp$test, cfg$feats)
    auc_val <- as.numeric(auc(roc(as.numeric(as.character(sp$test$hivstatusfinal)), prob_test, quiet = TRUE)))
    met <- classification_metrics(prob_test, sp$test$hivstatusfinal, thr)
    cal <- calibration_stats(prob_test, sp$test$hivstatusfinal)
    brier <- brier_score(prob_test, sp$test$hivstatusfinal)
    single_rows[[label]] <- bind_cols(
      tibble(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling,
             threshold = thr, auc = auc_val, brier = brier), met, cal)
    message(sprintf("  single-split balAcc=%.3f AUC=%.3f Brier=%.4f (threshold=%.4f)",
                     met$balanced_acc, auc_val, brier, thr))

    # --- same test-set predictions by country (pooled model, no refit) ---
    test_countries <- sp$test[[COUNTRY_VAR]]
    for (ctry in sort(unique(test_countries))) {
      idx <- which(test_countries == ctry)
      if (length(idx) < 5) next  # guard against a near-empty slice
      p_c <- prob_test[idx]; t_c <- sp$test$hivstatusfinal[idx]
      auc_c <- tryCatch(as.numeric(auc(roc(as.numeric(as.character(t_c)), p_c, quiet = TRUE))),
                         error = function(e) NA_real_)
      met_c <- classification_metrics(p_c, t_c, thr)
      cal_c <- calibration_stats(p_c, t_c)
      brier_c <- brier_score(p_c, t_c)
      by_country_rows[[paste(label, ctry)]] <- bind_cols(
        tibble(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling, country = ctry,
               n = length(idx), threshold = thr, auc = auc_c, brier = brier_c), met_c, cal_c)
    }

    # --- PSU-grouped 5-fold CV on the training set ---
    cv_folds <- grouped_folds(sp$train$svy_psu, k = CV_K)
    for (fold_i in seq_along(cv_folds)) {
      tr_idx <- cv_folds[[fold_i]]; te_idx <- setdiff(seq_len(nrow(sp$train)), tr_idx)
      prob_fold <- fit_fn(sp$train[tr_idx, ], sp$train[te_idx, ], cfg$feats)
      truth_fold <- sp$train$hivstatusfinal[te_idx]
      auc_fold <- as.numeric(auc(roc(as.numeric(as.character(truth_fold)), prob_fold, quiet = TRUE)))
      met_fold <- classification_metrics(prob_fold, truth_fold, thr)
      cal_fold <- calibration_stats(prob_fold, truth_fold)
      brier_fold <- brier_score(prob_fold, truth_fold)
      cv_rows[[paste(label, fold_i)]] <- bind_cols(
        tibble(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling, fold = fold_i,
               threshold = thr, auc = auc_fold, brier = brier_fold), met_fold, cal_fold)
    }
    message(sprintf("  CV done (%d folds)", CV_K))

    # --- leave-one-country-out, using the full sex-filtered dataset ---
    countries <- sort(unique(sp$full[[COUNTRY_VAR]]))
    for (ctry in countries) {
      loco_train <- sp$full[sp$full[[COUNTRY_VAR]] != ctry, ]
      loco_test  <- sp$full[sp$full[[COUNTRY_VAR]] == ctry, ]
      prob_loco <- fit_fn(loco_train, loco_test, cfg$feats)
      auc_loco <- as.numeric(auc(roc(as.numeric(as.character(loco_test$hivstatusfinal)), prob_loco, quiet = TRUE)))
      met_loco <- classification_metrics(prob_loco, loco_test$hivstatusfinal, thr)
      cal_loco <- calibration_stats(prob_loco, loco_test$hivstatusfinal)
      brier_loco <- brier_score(prob_loco, loco_test$hivstatusfinal)
      loco_rows[[paste(label, ctry)]] <- bind_cols(
        tibble(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling, held_out_country = ctry,
               threshold = thr, auc = auc_loco, brier = brier_loco), met_loco, cal_loco)
    }
    message(sprintf("  LOCO done (%d countries)", length(countries)))
  }

  cv_all <- bind_rows(cv_rows)
  loco_all <- bind_rows(loco_rows)

  # Mean and range across CV folds / held-out countries (confidence
  # intervals come from the Stage B cluster bootstrap).
  summarise_spread <- function(df, group_col) {
    df %>% group_by(sex, model, resampling) %>%
      summarise(
        n_groups = n(),
        auc_mean = mean(auc), auc_min = min(auc), auc_max = max(auc),
        sens_mean = mean(sensitivity), sens_min = min(sensitivity), sens_max = max(sensitivity),
        spec_mean = mean(specificity), spec_min = min(specificity), spec_max = max(specificity),
        ppv_mean = mean(ppv, na.rm = TRUE), ppv_min = min(ppv, na.rm = TRUE), ppv_max = max(ppv, na.rm = TRUE),
        balacc_mean = mean(balanced_acc), balacc_min = min(balanced_acc), balacc_max = max(balanced_acc),
        brier_mean = mean(brier), brier_min = min(brier), brier_max = max(brier),
        calib_intercept_mean = mean(calib_intercept, na.rm = TRUE),
        calib_slope_mean = mean(calib_slope, na.rm = TRUE),
        .groups = "drop"
      )
  }

  append_sheets(out_path, list(
    single_split = bind_rows(single_rows),
    primary_model_by_country = bind_rows(by_country_rows),
    cv_results   = cv_all,
    cv_summary   = summarise_spread(cv_all),
    loco_results = loco_all,
    loco_summary = summarise_spread(loco_all)
  ))
  message(sprintf("\n===== Stage A complete. Elapsed: %.1f min =====",
                   as.numeric(difftime(Sys.time(), STAGE_A_START, units = "mins"))))
  message("Written to: ", out_path)
}
