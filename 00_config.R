################################################################################
# 00_config.R
#
# Shared configuration sourced by every script. Set the environment variable
# PHIA_DATA_DIR to the folder holding the PHIA data files (see README);
# otherwise the "data" folder in the working directory is used.
################################################################################

base_dir     <- Sys.getenv("PHIA_DATA_DIR", unset = "data")
artifact_dir <- file.path(base_dir, "artifacts")
dir.create(artifact_dir, showWarnings = FALSE, recursive = TRUE)

data_path     <- function(...) file.path(base_dir, ...)
artifact_path <- function(...) file.path(artifact_dir, ...)
