################################################################################
# benchmark_logistic_regression.R
#
# A conventional logistic regression on a small, pre-specified set of
# indicators (age group, marital status, number of lifetime sexual partners,
# age at first sex, education, and urban/rural residence), fit and evaluated
# under the same PSU-grouped train/test split and out-of-fold threshold-
# selection procedure used for the primary machine learning models, to
# compare predictive performance against a conventional statistical
# approach.
#
# Fit on unweighted data, consistent with the machine learning models it is
# compared against. This model is evaluated for predictive performance only
# and is not used for odds-ratio interpretation.
#
# Requires downstream_setup.R and 00_config.R in the working directory.
################################################################################

source("downstream_setup.R")

out_path_bench <- artifact_path("BENCHMARK_LR_RESULTS.xlsx")

BENCHMARK_FEATS <- c("agecat_25_34", "agecat_35_44", "agecat_45_54", "agecat_55_64", "agecat_65plus",
                      "ever_married", "Num_sexpartners_life", "first_sex_at_age",
                      "education", "Urban_rural")

fit_benchmark_prob <- function(ts, nd, feats) {
  form <- as.formula(paste("hivstatusfinal ~", paste(feats, collapse = " + ")))
  m <- glm(form, data = ts[, c("hivstatusfinal", feats)], family = binomial())
  as.numeric(predict(m, newdata = nd[, feats], type = "response"))
}
make_benchmark_fit_fn <- function() function(train_raw, test, feats) fit_benchmark_prob(train_raw, test, feats)

rows <- list()
for (sx in c(0, 1)) {
  sex_label <- if (sx == 0) "male" else "female"
  message(sprintf("[%s] fitting benchmark logistic regression...", sex_label))
  sp <- build_primary_split(dat, sx, BENCHMARK_FEATS)
  fit_fn <- make_benchmark_fit_fn()
  thr <- nested_threshold(sp$train, BENCHMARK_FEATS, fit_fn, k = THRESH_K)

  prob_test <- fit_fn(sp$train, sp$test, BENCHMARK_FEATS)
  truth <- sp$test$hivstatusfinal
  auc_val <- as.numeric(auc(roc(as.numeric(as.character(truth)), prob_test, quiet = TRUE)))
  met <- classification_metrics(prob_test, truth, thr)
  brier <- brier_score(prob_test, truth)

  rows[[sex_label]] <- bind_cols(
    tibble(sex = sex_label, model = "Benchmark_LR", threshold = thr, auc = auc_val, brier = brier),
    met)
  message(sprintf("  balAcc=%.3f AUC=%.3f Brier=%.4f", met$balanced_acc, auc_val, brier))
}

benchmark_results <- bind_rows(rows)
write_xlsx(list(benchmark_comparison = benchmark_results), out_path_bench)
message("\nWritten to: ", out_path_bench)
