################################################################################
# run_stage_C.R
#
# Stage C: Platt recalibration, calibration-plot data, and feature
# importance, for all eight configurations.
#
# Requires downstream_setup.R and 00_config.R in the working directory.
################################################################################

source("downstream_setup.R")

################################################################################
# STAGE C -- Platt recalibration, calibration-plot data, feature importance
################################################################################
{
  STAGE_C_START <- Sys.time()
  calib_rows <- list(); importance_rows <- list(); calib_plot_rows <- list()

  for (cfg in CONFIGS) {
    label <- paste(cfg$sex, cfg$model, cfg$resampling)
    message(sprintf("\n[Stage C: %s] recalibration + feature importance...", label))
    hp <- FINAL_HP[[cfg$sex]][[cfg$model]][[cfg$resampling]]
    sp <- build_primary_split(dat, cfg$sx, cfg$feats)
    fit_fn <- make_resampling_fit_fn(cfg$model, hp, cfg$ratio)

    # Out-of-fold training predictions used to fit the Platt model.
    folds <- grouped_folds(sp$train$svy_psu, k = CV_K)
    oof_prob <- rep(NA_real_, nrow(sp$train))
    for (tr_idx in folds) {
      te_idx <- setdiff(seq_len(nrow(sp$train)), tr_idx)
      oof_prob[te_idx] <- fit_fn(sp$train[tr_idx, ], sp$train[te_idx, ], cfg$feats)
    }
    y_train <- as.numeric(as.character(sp$train$hivstatusfinal))
    eps <- 1e-6
    oof_clipped <- pmin(pmax(oof_prob, eps), 1 - eps)
    platt_fit <- glm(y ~ logit_p, data = data.frame(y = y_train, logit_p = qlogis(oof_clipped)),
                      family = binomial())

    # Test-set predictions, raw and after Platt recalibration.
    prob_test_raw <- fit_fn(sp$train, sp$test, cfg$feats)
    prob_test_clipped <- pmin(pmax(prob_test_raw, eps), 1 - eps)
    prob_test_platt <- predict(platt_fit, newdata = data.frame(logit_p = qlogis(prob_test_clipped)),
                                type = "response")
    y_test <- as.numeric(as.character(sp$test$hivstatusfinal))

    brier_raw   <- brier_score(prob_test_raw, sp$test$hivstatusfinal)
    brier_platt <- brier_score(prob_test_platt, sp$test$hivstatusfinal)
    p_bar <- mean(y_test)
    brier_baseline <- p_bar * (1 - p_bar)  # constant-prevalence reference

    cal_raw   <- calibration_stats(prob_test_raw, sp$test$hivstatusfinal)
    cal_platt <- calibration_stats(prob_test_platt, sp$test$hivstatusfinal)

    calib_rows[[label]] <- tibble(
      sex = cfg$sex, model = cfg$model, resampling = cfg$resampling,
      brier_raw = brier_raw, brier_platt = brier_platt, brier_baseline = brier_baseline,
      calib_intercept_raw = cal_raw$calib_intercept, calib_slope_raw = cal_raw$calib_slope,
      calib_intercept_platt = cal_platt$calib_intercept, calib_slope_platt = cal_platt$calib_slope
    )
    message(sprintf("  Brier raw=%.4f  Platt=%.4f  baseline=%.4f", brier_raw, brier_platt, brier_baseline))

    # --- calibration-plot source data: decile bins of predicted vs. observed ---
    for (which_prob in c("raw", "platt")) {
      p <- if (which_prob == "raw") prob_test_raw else prob_test_platt
      bins <- tryCatch(ntile(p, 10), error = function(e) rep(NA_integer_, length(p)))
      plot_df <- tibble(bin = bins, pred = p, obs = y_test) %>%
        filter(!is.na(bin)) %>%
        group_by(bin) %>%
        summarise(n = n(), mean_predicted = mean(pred), mean_observed = mean(obs), .groups = "drop")
      calib_plot_rows[[paste(label, which_prob)]] <- plot_df %>%
        mutate(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling,
               probability_type = which_prob, .before = 1)
    }

    # --- feature importance from the model fit on the full training set
    #     (gain for XGBoost; impurity decrease for RF, stored in the same column) ---
    ts_final <- smote_or_none(sp$train, cfg$feats, cfg$ratio)
    if (cfg$model == "RF") {
      m <- ranger(hivstatusfinal ~ ., data = ts_final[, c("hivstatusfinal", cfg$feats)],
                  probability = TRUE, num.trees = 500, mtry = hp$mtry,
                  min.node.size = hp$min_node_size, importance = "impurity")
      imp <- tibble(feature = names(m$variable.importance), gain = as.numeric(m$variable.importance)) %>%
        arrange(desc(gain)) %>% mutate(rank = row_number())
    } else {
      dtr <- xgb.DMatrix(as.matrix(ts_final[, cfg$feats]), label = as.numeric(as.character(ts_final$hivstatusfinal)))
      prm <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                  eta = hp$eta, max_depth = hp$max_depth, subsample = 0.8, colsample_bytree = 0.8)
      m <- xgb.train(prm, dtr, nrounds = 500, verbose = 0)
      imp_raw <- xgb.importance(feature_names = cfg$feats, model = m)
      imp <- tibble(feature = imp_raw$Feature, gain = imp_raw$Gain) %>%
        arrange(desc(gain)) %>% mutate(rank = row_number())
    }
    importance_rows[[label]] <- imp %>% mutate(sex = cfg$sex, model = cfg$model, resampling = cfg$resampling, .before = 1)
  }

  append_sheets(out_path, list(
    calibration        = bind_rows(calib_rows),
    calibration_plot_data = bind_rows(calib_plot_rows),
    feature_importance = bind_rows(importance_rows)
  ))
  message(sprintf("\n===== Stage C complete. Elapsed: %.1f min =====",
                   as.numeric(difftime(Sys.time(), STAGE_C_START, units = "mins"))))
  message("Written to: ", out_path)
}
