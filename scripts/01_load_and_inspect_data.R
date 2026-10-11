# ==============================================================================
# Script: scripts/01_load_and_inspect_data.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Load the dataset and verify sample size, participant count, and core variables
# ==============================================================================

# 1. Load required packages
library(dplyr)

# 2. Define relative path to dataset
data_path <- "data/hartanto_628.csv"

# 3. Read dataset
df <- read.csv(data_path, stringsAsFactors = FALSE)

# 4. Verify overall dimensions
cat("=== DATA DIMENSIONS ===\n")
cat("Rows (Observations):", nrow(df), "\n")
cat("Columns (Variables):", ncol(df), "\n")
cat("Unique Participants:", n_distinct(df$participantID), "\n\n")

# 5. Inspect core variables of interest
core_vars <- c(
  "participantID",
  "day",
  "dayCogFail",
  "dayScreenChecking",
  "dayScreenChecking_dev",
  "dayScreenChecking_mn",
  "dayStressor"
)

cat("=== STRUCTURE OF CORE VARIABLES ===\n")
str(df[core_vars])

cat("\n=== SUMMARY OF CORE VARIABLES ===\n")
summary(df[core_vars])
