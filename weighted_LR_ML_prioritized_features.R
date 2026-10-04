################################################################################
# weighted_LR_ML_prioritized_features.R
#
# Design-weighted multivariable logistic regression on the predictors
# prioritised by the primary model (XGBoost, no resampling), fit separately
# for males and females. Reports odds ratios, 95% confidence intervals,
# p-values, and variance inflation factors.
#
# Sample: analytic_final.xlsx (complete-case analytic sample; not resampled).
#
# Weights: the PHIA adult biomarker weight (svy_weight = btwt0) is rescaled
# within each country, separately by sex, so that each country's total weight
# equals its sample size in the sex-specific sample:
#     design_weight = svy_weight * n_country / sum(svy_weight in country)
# This preserves each respondent's relative weight within country and stratum
# while making each country's contribution proportional to its sample size.
# A check that weight shares equal sample shares is written to the
# weight_rescaling_check sheet.
#
# Design: strata (svy_strata) and PSUs (svy_psu) are country-unique
# identifiers (country + PHIA varstrat / varunit). Single-PSU strata
# are handled with survey.lonely.psu = "adjust". Country is included as a
# covariate.
#
# Predictor selection: predictors were ranked by gain in each sex's primary
# XGBoost model and the ten lowest-ranked dropped, except that the complete
# indicator sets for age category and household wealth were retained if any
# member fell among the lowest ten, and urban/rural residence was retained in
# both models. Age 15-24 and low wealth are the reference categories.
#
# Because predictors were selected using the outcome, this regression is
# exploratory.
#
# Output: WEIGHTED_LR_RESULTS.xlsx
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(survey); library(writexl)
})
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(car)

source("00_config.R")
options(survey.lonely.psu = "adjust")
results_path <- artifact_path("WEIGHTED_LR_RESULTS.xlsx")
dat <- read_excel(data_path("analytic_final.xlsx"), guess_max = 200000)

stopifnot(nrow(dat) == 79621)   # complete-case analytic sample

# Predictor sets per sex (see header). Reference categories omitted.
male_feats <- c(
  "condomlastsex12months", "agecat_25_34", "agecat_35_44", "agecat_45_54",
  "agecat_55_64", "agecat_65plus", "Num_sexpartners_life", "ever_married",
  "Male_Circumcision", "Members_sleep_here", "avoidpreg", "Urban_rural",
  "sex_12_months", "education", "mid_wealth", "high_wealth"
)
female_feats <- c(
  "Num_sexpartners_life", "condomlastsex12months", "agecat_25_34", "agecat_35_44",
  "agecat_45_54", "agecat_55_64", "agecat_65plus", "HH_head_gender",
  "sex_12_months", "Members_sleep_here", "ever_married", "education",
  "HH_phone", "live_births", "first_sex_at_age", "HH_radio", "Urban_rural",
  "mid_wealth", "high_wealth"
)

# * p<0.05, ** p<0.01, *** p<0.001
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

lr_sheets <- list()
weight_checks <- list()

for (sx in c(0, 1)) {
  lab   <- if (sx == 0) "male" else "female"
  feats <- if (sx == 0) male_feats else female_feats
  dd <- dat %>% filter(sex == sx) %>% mutate(hivstatusfinal = as.integer(hivstatusfinal))

  # Within-country rescaling of the biomarker weight (see header).
  dd <- dd %>%
    group_by(country) %>%
    mutate(design_weight = svy_weight * (dplyr::n() / sum(svy_weight))) %>%
    ungroup()

  des <- svydesign(ids = ~svy_psu, strata = ~svy_strata, weights = ~design_weight, data = dd, nest = TRUE)
  form <- as.formula(paste("hivstatusfinal ~", paste(feats, collapse = " + "), "+ factor(country)"))
  fit_w <- svyglm(form, design = des, family = quasibinomial())

  sm <- summary(fit_w)$coefficients
  ci <- confint(fit_w)
  out <- data.frame(
    term    = rownames(sm),
    OR      = exp(sm[, 1]),
    CI_low  = exp(ci[, 1]),
    CI_high = exp(ci[, 2]),
    p_value = sm[, 4],
    stringsAsFactors = FALSE
  )
  out$stars <- stars(out$p_value)
  out <- out[!(out$term == "(Intercept)" | grepl("^factor\\(country\\)", out$term)), ]
  out$OR <- round(out$OR, 2); out$CI_low <- round(out$CI_low, 2)
  out$CI_high <- round(out$CI_high, 2); out$p_value <- signif(out$p_value, 3)

  # Variance inflation factors, computed from an auxiliary OLS fit on the same
  # predictors (VIF depends only on the predictor design matrix).
  vif_vals <- tryCatch({
    aux_fit <- lm(as.formula(paste("hivstatusfinal ~", paste(feats, collapse = " + "),
                                    "+ factor(country)")), data = dd)
    v <- car::vif(aux_fit)
    v <- if (is.matrix(v)) v[, 1] else v   # vif() returns a matrix for terms with >1 df
    data.frame(term = names(v), VIF = round(as.numeric(v), 2))
  }, error = function(e) {
    message("  [VIF could not be computed: ", conditionMessage(e), "]")
    data.frame(term = character(0), VIF = numeric(0))
  })
  out <- merge(out, vif_vals, by = "term", all.x = TRUE)
  out <- out[order(match(out$term, sm[, 1])), ]  # restore original term order

  print(paste("=====", toupper(lab), "====="))
  print(out, row.names = FALSE)
  if (nrow(vif_vals) == 0) {
    message(sprintf("  [%s] WARNING: VIF could not be computed -- multicollinearity not checked.", lab))
  } else if (any(vif_vals$VIF > 5, na.rm = TRUE)) {
    high_vif <- vif_vals[vif_vals$VIF > 5, ]
    message(sprintf("  [%s] NOTE: %d predictor(s) with VIF > 5: %s",
                     lab, nrow(high_vif), paste(high_vif$term, collapse = ", ")))
  } else {
    message(sprintf("  [%s] No predictors with VIF > 5 (checked %d terms).", lab, nrow(vif_vals)))
  }

  # Rescaling check: each country's weight share should equal its sample share.
  chk <- dd %>%
    group_by(country) %>%
    summarise(n = dplyr::n(), weight_total = sum(design_weight), .groups = "drop") %>%
    mutate(
      n_share_pct      = round(100 * n / sum(n), 1),
      weight_share_pct = round(100 * weight_total / sum(weight_total), 1)
    )
  message(sprintf("\n  [%s] country weight-rescaling check:", lab))
  print(chk, row.names = FALSE)
  chk$sex <- lab
  weight_checks[[lab]] <- chk

  lr_sheets[[paste0("weighted_LR_", lab)]] <- out
}

# ---- write results (sheets replaced by name; other sheets preserved) ----
lr_sheets[["weight_rescaling_check"]] <- bind_rows(weight_checks)

prior_sheets <- list()
if (file.exists(results_path)) {
  sheet_names <- readxl::excel_sheets(results_path)
  prior_sheets <- lapply(sheet_names, function(s) read_excel(results_path, sheet = s))
  names(prior_sheets) <- sheet_names
}
for (nm in names(lr_sheets)) prior_sheets[[nm]] <- lr_sheets[[nm]]
write_xlsx(prior_sheets, results_path)
message("\nWrote weighted_LR_male, weighted_LR_female, and weight_rescaling_check to: ", results_path)
