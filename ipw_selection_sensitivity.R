################################################################################
# ipw_selection_sensitivity.R
#
# Sensitivity analysis for complete-case selection (regression). A logistic
# model predicts inclusion in the complete-case sample among all eligible
# respondents (definitive HIV result, age 15+) from covariates observed for
# both included and excluded respondents: age, sex, country, education,
# household wealth, and HIV status. The stabilised inverse-probability-of-
# inclusion weight is multiplied by the country-rescaled design weight used
# in weighted_LR_ML_prioritized_features.R, and the design-weighted logistic
# regression is refit with this combined weight. Odds ratios from the
# baseline (design weight only) and reweighted models are compared.
#
# Inputs: master_merged_all.xlsx, analytic_final.xlsx
# Output: IPW_SELECTION_SENSITIVITY_RESULTS.xlsx
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(survey); library(writexl)
})
source("00_config.R")
options(survey.lonely.psu = "adjust")
out_path <- artifact_path("IPW_SELECTION_SENSITIVITY_RESULTS.xlsx")

master <- read_excel(data_path("master_merged_all.xlsx"), guess_max = 200000) %>%
  mutate(personid = as.character(personid))
fin <- read_excel(data_path("analytic_final.xlsx"), guess_max = 200000) %>%
  mutate(personid = as.character(personid))
fin_ids <- fin$personid

# Inclusion model on the eligible sample (definitive HIV result, age >= 15).
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

incl_rate      <- mean(elig$included)
elig$p_incl    <- predict(sel_model, type = "response")
elig$ipw_stab  <- incl_rate / elig$p_incl

w_incl <- elig$ipw_stab[elig$included == 1]
weight_diag <- tibble(
  statistic = c("min", "p25", "median", "p75", "max", "n_weight_gt_5", "n_weight_gt_10"),
  value     = c(min(w_incl), quantile(w_incl, .25), median(w_incl), quantile(w_incl, .75),
                max(w_incl), sum(w_incl > 5), sum(w_incl > 10))
)

fin <- fin %>%
  left_join(elig %>% select(personid, ipw_stab), by = "personid")

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

compare_or <- function(sx, feats, lab) {
  dd <- fin %>% filter(sex == sx) %>% mutate(hivstatusfinal = as.integer(hivstatusfinal))

  # Country-rescaled design weight (as in weighted_LR_ML_prioritized_features.R)
  # and the same weight multiplied by the inclusion weight.
  dd <- dd %>%
    group_by(country) %>%
    mutate(design_weight   = svy_weight * (dplyr::n() / sum(svy_weight)),
           combined_weight = design_weight * ipw_stab) %>%
    ungroup()

  form <- as.formula(paste("hivstatusfinal ~", paste(feats, collapse = " + "), "+ factor(country)"))

  des_base <- svydesign(ids = ~svy_psu, strata = ~svy_strata, weights = ~design_weight,   data = dd, nest = TRUE)
  des_ipw  <- svydesign(ids = ~svy_psu, strata = ~svy_strata, weights = ~combined_weight, data = dd, nest = TRUE)

  fit_base <- svyglm(form, design = des_base, family = quasibinomial())
  fit_ipw  <- svyglm(form, design = des_ipw,  family = quasibinomial())

  cb <- coef(fit_base); ci_b <- confint(fit_base)
  cw <- coef(fit_ipw);  ci_w <- confint(fit_ipw)

  out <- tibble(
    term        = names(cb),
    OR_baseline = exp(cb),
    CI_low_baseline  = exp(ci_b[, 1]),
    CI_high_baseline = exp(ci_b[, 2]),
    OR_ipw      = exp(cw[match(names(cb), names(cw))]),
    CI_low_ipw  = exp(ci_w[match(names(cb), names(cw)), 1]),
    CI_high_ipw = exp(ci_w[match(names(cb), names(cw)), 2])
  ) %>% filter(!(term == "(Intercept)" | grepl("^factor\\(country\\)", term)))

  out$pct_change_OR <- round(100 * (out$OR_ipw - out$OR_baseline) / out$OR_baseline, 1)
  out[, -1] <- round(out[, -1], 3)

  message(sprintf("\n[%s] max absolute %% change in OR under IPW reweighting: %.1f%%",
                   lab, max(abs(out$pct_change_OR))))
  out
}

male_out   <- compare_or(0, male_feats,   "male")
female_out <- compare_or(1, female_feats, "female")

sel_summary <- broom::tidy(sel_model, exponentiate = TRUE, conf.int = TRUE)

write_xlsx(
  list(
    inclusion_model  = sel_summary,
    weight_diagnostics = weight_diag,
    or_comparison_male   = male_out,
    or_comparison_female = female_out
  ),
  out_path
)
message("\nWritten to: ", out_path)
