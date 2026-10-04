################################################################################
# missing_data_comparison.R
#
# Descriptive comparison (S4 Table) of respondents in the analytic sample
# with those excluded from the eligible sample (definitive HIV test result,
# age 15 years or older): demographics, socioeconomic status, HIV status,
# and missingness on the behavioural items with the highest item
# non-response. No models are fit.
#
# Inputs: master_merged_all.xlsx, analytic_final.xlsx
# Output: MISSING_DATA_COMPARISON_RESULTS.xlsx (summary, by_country)
################################################################################

suppressMessages({ library(readxl); library(dplyr); library(tidyr); library(writexl) })
source("00_config.R")
out_path <- artifact_path("MISSING_DATA_COMPARISON_RESULTS.xlsx")

master <- read_excel(data_path("master_merged_all.xlsx"), guess_max = 200000) %>%
  mutate(personid = as.character(personid))
fin_ids <- read_excel(data_path("analytic_final.xlsx"), guess_max = 200000) %>%
  mutate(personid = as.character(personid)) %>% pull(personid)

elig <- master %>%
  filter(!is.na(hivstatusfinal), age >= 15) %>%
  mutate(
    group  = ifelse(personid %in% fin_ids, "Included", "Excluded"),
    age    = suppressWarnings(as.numeric(age)),
    hivpos = as.integer(hivstatusfinal == 1)
  )

# Share coded as val among non-missing responses; share missing (NA, -8, -9, 99).
pct_eq      <- function(x, val) round(100 * mean(x == val, na.rm = TRUE), 1)
pct_missing <- function(x) round(100 * mean(is.na(x) | x %in% c(-8, -9, 99), na.rm = TRUE), 1)

tab <- elig %>% group_by(group) %>% summarise(
  n                = n(),
  `HIV-positive %` = round(100 * mean(hivpos, na.rm = TRUE), 2),
  `Female %`       = pct_eq(gender, 2),
  `Mean age (yrs)` = round(mean(age, na.rm = TRUE), 1),
  `Low education %`           = round(100 * mean(education %in% c(1, 2), na.rm = TRUE), 1),
  `Wealth Q1-2 %`             = round(100 * mean(wealthquintile %in% c(1, 2), na.rm = TRUE), 1),
  `Condom missing %`          = pct_missing(condomlastsex12months),
  `Sexual activity missing %` = pct_missing(sex12months),
  .groups = "drop"
)

ctab <- elig %>% count(group, country) %>%
  pivot_wider(names_from = country, values_from = n, values_fill = 0)

print(as.data.frame(tab))
cat("\nCountry distribution:\n"); print(as.data.frame(ctab))

write_xlsx(list(summary = tab, by_country = ctab), out_path)
message("\nWritten to: ", out_path)
