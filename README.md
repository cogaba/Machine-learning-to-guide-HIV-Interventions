# Machine learning to characterize correlates of prevalent HIV-positive status in four PHIA surveys

Analysis code for the manuscript. All scripts are in R (version 4.5.1); package versions
are recorded in `artifacts/sessionInfo.txt` (produced by `session_info.R`).

## Data

The PHIA public-release datasets are controlled-access and are not included. Request them at
https://phia-data.icap.columbia.edu/datasets. The surveys used are MPHIA 2020-2021 (Malawi),
INSIDA 2021 (Mozambique), THIS 2022-2023 (Tanzania), and UPHIA 2020-2021 (Uganda); the adult
biomarker, adult individual, and household files of each. The scripts read two derived files,
constructed from those surveys as described in the Methods and S1 Table of the manuscript:

- `analytic_final.xlsx` — the complete-case analytic sample (n = 79,621): `personid`, `country`,
  `year`, `sex`, `hivstatusfinal`, `aware`, `recentlagvl`, the survey design variables
  (`svy_weight` = adult biomarker weight `btwt0`; `svy_strata`, `svy_psu` = country-unique
  stratum and PSU identifiers), and the 26 recoded features per sex listed in `downstream_setup.R`.
- `master_merged_all.xlsx` — the pooled file before exclusions (eligible sample: definitive HIV
  result, age 15+), used by the missing-data comparison and the inverse-probability-of-inclusion
  sensitivity analyses: `personid`, `country`, `gender`, `age`, `education`, `wealthquintile`,
  `hivstatusfinal`, `condomlastsex12months`, `sex12months`, `svy_weight`.
- `wealth_scores_by_country.xlsx` — sheets `Mal`, `Moz`, `TZ`, `Ug`, each with `personid`, `country`,
  `wealthscorecont`, `wealthquintile` from the household files (GDP-adjusted wealth sensitivity).

Raw PHIA variables used: adult individual — `gender`, `age`, `urban`, `work12mo`, `evermar`, `liveb`,
`avoidpreg`, `mcstatus`, `firstsxage`, `lifetimesex`, `alcfreq`, `education`, `sex12months`,
`condomlastsex12months`; household — `wealthquintile`, `hhqitems_a`-`hhqitems_d`, `householdheadgender`,
`rostercountdefacto`; adult biomarker — `hivstatusfinal`, `aware`, `recentlagvl`, `btwt0`, `varstrat`,
`varunit`.

Set the environment variable `PHIA_DATA_DIR` to the folder holding these files (default: `data/`).

## Run order

| Step | Script | Output |
|---|---|---|
| 1 | `primary_model_selection.R` | `PRIMARY_MODEL_SELECTION.xlsx` (tuning grid and tuned hyperparameters) |
| 2 | `run_stage_A.R`, `run_stage_B.R`, `run_stage_C.R` | `DOWNSTREAM_RESULTS.xlsx` (Table 2, S5 Table, calibration, feature importance) |
| 3 | `weighted_LR_ML_prioritized_features.R` | `WEIGHTED_LR_RESULTS.xlsx` (S2, S3 Tables) |
| 4 | `benchmark_logistic_regression.R` | `BENCHMARK_LR_RESULTS.xlsx` |
| 5 | `gdp_wealth_sensitivity.R` | `GDP_WEALTH_SENSITIVITY_RESULTS.xlsx` |
| 6 | `undiagnosed_population_sensitivity.R` | `UNDIAGNOSED_POPULATION_SENSITIVITY_RESULTS.xlsx` |
| 7 | `ipw_selection_sensitivity.R`, `xgb_ipw_selection_sensitivity.R` | IPW sensitivity results |
| 8 | `missing_data_comparison.R` | S4 Table |
| 9 | `country_specific_concordance.R` | `COUNTRY_CONCORDANCE_RESULTS.xlsx` (S1, S2 Figs) |
| 10 | `generate_figures.R`, `S3_Fig_flow_diagram.R` | Figs 1-4, S1-S4 Figs |
| 11 | `session_info.R` | `artifacts/sessionInfo.txt` |

`downstream_setup.R` and `00_config.R` are sourced by the other scripts and are not run directly.
All outputs are written to `artifacts/` inside the data folder.

## Reproducibility

Random seeds are fixed in the scripts (primary split: seed 42; bootstrap: per-configuration
seeds). Install the packages listed in `session_info.R`; to reproduce the exact environment,
`renv::init()` followed by `renv::restore()` from the included `renv.lock` can be used.
