################################################################################
# run_stage_B.R
#
# Stage B: cluster (PSU) bootstrap. Each replicate resamples PSUs with
# replacement, re-selects hyperparameters, re-derives the classification
# threshold, refits, and evaluates on the test set.
#
# Progress is checkpointed to bootstrap_checkpoint.rds every 10 replicates;
# rerunning resumes from the checkpoint.
#
# Requires downstream_setup.R and 00_config.R in the working directory.
################################################################################

source("downstream_setup.R")

################################################################################
# STAGE B -- cluster (PSU) bootstrap, refit + clustering + tuning +
#            threshold uncertainty, all re-derived within each replicate
################################################################################
{

  cluster_bootstrap_sample <- function(train, psu_var = "svy_psu") {
    psus  <- unique(train[[psu_var]])
    drawn <- sample(psus, size = length(psus), replace = TRUE)
    pieces <- vector("list", length(drawn))
    for (i in seq_along(drawn)) {
      sub <- train[train[[psu_var]] == drawn[i], ]
      # Repeated draws of the same PSU are relabelled so the inner grouped
      # folds treat them as distinct clusters.
      sub[[psu_var]] <- paste0(sub[[psu_var]], "_boot", i)
      pieces[[i]] <- sub
    }
    bind_rows(pieces)
  }

  select_candidate <- function(boot_train, feats, model_name, ratio, candidates, k) {
    folds <- grouped_folds(boot_train$svy_psu, k = k)
    best_auc <- -Inf; best_hp <- candidates[[1]]
    for (cand in candidates) {
      fit_fn <- make_resampling_fit_fn(model_name, cand, ratio)
      aucs <- sapply(folds, function(tr_idx) {
        te_idx <- setdiff(seq_len(nrow(boot_train)), tr_idx)
        prob <- fit_fn(boot_train[tr_idx, ], boot_train[te_idx, ], feats)
        as.numeric(auc(roc(as.numeric(as.character(boot_train$hivstatusfinal[te_idx])), prob, quiet = TRUE)))
      })
      m <- mean(aucs)
      if (m > best_auc) { best_auc <- m; best_hp <- cand }
    }
    best_hp
  }

  STAGE_B_START <- Sys.time()
  boot_results <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else list()

  configs_b <- if (INCLUDE_SMOTE_IN_BOOTSTRAP) CONFIGS else Filter(function(c) c$resampling == "none", CONFIGS)

  for (cfg in configs_b) {
    label <- paste(cfg$sex, cfg$model, cfg$resampling)
    sp <- build_primary_split(dat, cfg$sx, cfg$feats)
    candidates <- CANDIDATE_HP[[cfg$sex]][[cfg$model]][[cfg$resampling]]

    done <- if (!is.null(boot_results[[label]])) nrow(boot_results[[label]]) else 0
    if (done >= BOOTSTRAP_REPS) {
      message(sprintf("[Stage B: %s] already has %d/%d replicates -- skipping", label, done, BOOTSTRAP_REPS))
      next
    }
    message(sprintf("\n[Stage B: %s] resuming at replicate %d/%d", label, done + 1, BOOTSTRAP_REPS))

    rep_rows <- if (done > 0) boot_results[[label]] else tibble()
    set.seed(1000 + which(sapply(CONFIGS, function(c) paste(c$sex, c$model, c$resampling) == label)))

    for (b in (done + 1):BOOTSTRAP_REPS) {
      boot_train <- cluster_bootstrap_sample(sp$train)
      hp_b <- select_candidate(boot_train, cfg$feats, cfg$model, cfg$ratio, candidates, k = BOOT_TUNE_K)
      fit_fn_b <- make_resampling_fit_fn(cfg$model, hp_b, cfg$ratio)
      thr_b <- tryCatch(nested_threshold(boot_train, cfg$feats, fit_fn_b, k = BOOT_THRESH_K),
                         error = function(e) NA_real_)
      if (is.na(thr_b)) next  # occasional inner-fold degeneracy; drop this replicate

      prob_test <- fit_fn_b(boot_train, sp$test, cfg$feats)
      auc_val <- as.numeric(auc(roc(as.numeric(as.character(sp$test$hivstatusfinal)), prob_test, quiet = TRUE)))
      met <- classification_metrics(prob_test, sp$test$hivstatusfinal, thr_b)
      cal <- calibration_stats(prob_test, sp$test$hivstatusfinal)
      brier <- brier_score(prob_test, sp$test$hivstatusfinal)

      rep_rows <- bind_rows(rep_rows, bind_cols(
        tibble(rep = b, hp = paste(names(hp_b), unlist(hp_b), sep = "=", collapse = ", "),
               threshold = thr_b, auc = auc_val, brier = brier), met, cal))

      if (b %% 10 == 0) {
        boot_results[[label]] <- rep_rows
        saveRDS(boot_results, checkpoint_path)
        message(sprintf("  [%s] replicate %d/%d  (balAcc=%.3f AUC=%.3f)",
                         label, b, BOOTSTRAP_REPS, met$balanced_acc, auc_val))
      }
    }
    boot_results[[label]] <- rep_rows
    saveRDS(boot_results, checkpoint_path)
  }

  boot_all <- bind_rows(lapply(names(boot_results), function(nm) {
    parts <- strsplit(nm, " ")[[1]]
    boot_results[[nm]] %>% mutate(sex = parts[1], model = parts[2], resampling = parts[3], .before = 1)
  }))

  ci_summary <- boot_all %>%
    group_by(sex, model, resampling) %>%
    summarise(
      n_reps = n(),
      auc_median = median(auc), auc_lo = quantile(auc, .025), auc_hi = quantile(auc, .975),
      sens_median = median(sensitivity), sens_lo = quantile(sensitivity, .025), sens_hi = quantile(sensitivity, .975),
      spec_median = median(specificity), spec_lo = quantile(specificity, .025), spec_hi = quantile(specificity, .975),
      ppv_median = median(ppv, na.rm = TRUE), ppv_lo = quantile(ppv, .025, na.rm = TRUE), ppv_hi = quantile(ppv, .975, na.rm = TRUE),
      balacc_median = median(balanced_acc), balacc_lo = quantile(balanced_acc, .025), balacc_hi = quantile(balanced_acc, .975),
      brier_median = median(brier), brier_lo = quantile(brier, .025), brier_hi = quantile(brier, .975),
      calib_intercept_median = median(calib_intercept, na.rm = TRUE),
      calib_intercept_lo = quantile(calib_intercept, .025, na.rm = TRUE),
      calib_intercept_hi = quantile(calib_intercept, .975, na.rm = TRUE),
      calib_slope_median = median(calib_slope, na.rm = TRUE),
      calib_slope_lo = quantile(calib_slope, .025, na.rm = TRUE),
      calib_slope_hi = quantile(calib_slope, .975, na.rm = TRUE),
      .groups = "drop"
    )

  append_sheets(out_path, list(bootstrap_reps = boot_all, bootstrap_ci = ci_summary))
  message(sprintf("\n===== Stage B complete. Elapsed: %.1f min =====",
                   as.numeric(difftime(Sys.time(), STAGE_B_START, units = "mins"))))
  message("Written to: ", out_path)
}
