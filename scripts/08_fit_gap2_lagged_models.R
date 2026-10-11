# ==============================================================================
# Script: scripts/08_fit_gap2_lagged_models.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Test Gap 2 - Temporal Directionality and Lagged Models (Day t -> Day t+1)
# ==============================================================================

library(dplyr)
library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Construct within-person lagged variables safely
# Note: We must group by participantID and ensure days are sorted
df_lagged <- df %>%
  arrange(participantID, day) %>%
  group_by(participantID) %>%
  mutate(
    # Ensure consecutive days (e.g. Day 2 follows Day 1)
    is_consecutive = (day == lag(day) + 1),
    lag_cogfail = ifelse(is_consecutive, lag(dayCogFail), NA),
    lag_checking_dev = ifelse(is_consecutive, lag(dayScreenChecking_dev), NA),
    lag_screentime_dev = ifelse(is_consecutive, lag(dayScreenTime_dev), NA)
  ) %>%
  ungroup()

# 3. Verify lagged sample size
cat("=== LAGGED SAMPLE SUMMARY ===\n")
cat("Total original rows:", nrow(df), "\n")
cat("Valid consecutive lagged pairs:", sum(!is.na(df_lagged$lag_checking_dev)), "\n")
cat("Unique participants:", n_distinct(df_lagged$participantID[!is.na(df_lagged$lag_checking_dev)]), "\n\n")

# 4. Model 2A: Forward Lagged Model (Yesterday's checking -> Today's cognitive failures)
# Controlling for yesterday's cognitive failures (autoregressive effect)
m_lag_forward <- lmer(
  dayCogFail ~ lag_cogfail + lag_checking_dev + (1 | participantID),
  data = df_lagged,
  REML = FALSE
)

cat("=== FORWARD LAGGED MODEL (YESTERDAY CHECKING -> TODAY COGNITION) ===\n")
print(summary(m_lag_forward)$coefficients)

# 5. Model 2B: Simultaneous Lagged + Same-Day Model
# Tests whether checking effect is acute (same-day) or carry-over (lagged)
m_lag_simultaneous <- lmer(
  dayCogFail ~ lag_cogfail + lag_checking_dev + dayScreenChecking_dev + (1 | participantID),
  data = df_lagged,
  REML = FALSE
)

cat("\n=== SIMULTANEOUS LAGGED + SAME-DAY MODEL ===\n")
coef_table <- summary(m_lag_simultaneous)$coefficients
p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
print(round(coef_table, 6))

# 6. Model 2C: Reverse Directionality Model (Does yesterday's cognitive failure drive today's checking?)
m_lag_reverse <- lmer(
  dayScreenChecking_dev ~ lag_checking_dev + lag_cogfail + (1 | participantID),
  data = df_lagged,
  REML = FALSE
)

cat("\n=== REVERSE DIRECTIONALITY MODEL (YESTERDAY COGNITION -> TODAY CHECKING) ===\n")
coef_rev <- summary(m_lag_reverse)$coefficients
p_rev <- 2 * (1 - pnorm(abs(coef_rev[, "t value"])))
coef_rev <- cbind(coef_rev, "Pr(>|z|)" = p_rev)
print(round(coef_rev, 6))
