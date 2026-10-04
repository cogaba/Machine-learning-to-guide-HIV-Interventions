################################################################################
# session_info.R
#
# Records the R version and package versions used, to artifacts/sessionInfo.txt.
################################################################################
source("00_config.R")
pkgs <- c("readxl", "writexl", "dplyr", "tidyr", "ranger", "xgboost", "smotefamily",
          "caret", "pROC", "survey", "car", "broom", "ggplot2", "scales")
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
writeLines(capture.output(sessionInfo()), artifact_path("sessionInfo.txt"))
message("Written to: ", artifact_path("sessionInfo.txt"))
