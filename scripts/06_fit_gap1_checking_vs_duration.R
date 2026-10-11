# ==============================================================================
# Script: scripts/06_fit_gap1_checking_vs_duration.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Test Gap 1 - Checking Frequency (Interruptions) vs. Screen Time (Duration)
# ==============================================================================

library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Fit the simultaneous model
# Predictors:
# - Within-person checking deviations (dayScreenChecking_dev)
# - Within-person screen time deviations (dayScreenTime_dev, in hours)
# - Between-person checking mean (dayScreenChecking_mn)
# - Between-person screen time mean (dayScreenTime_mn)
# - Participant random intercept (1 | participantID)

model_gap1 <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenTime_dev + 
               dayScreenChecking_mn + dayScreenTime_mn + (1 | participantID),
  data = df,
  REML = FALSE
)

# 3. Print Model Summary
cat("=== GAP 1: CHECKING FREQUENCY VS. DURATION (SCREEN TIME) ===\n")
print(summary(model_gap1))

# 4. Extract Fixed Effects Table with Wald p-values
cat("\n=== FIXED EFFECTS TABLE ===\n")
coef_table <- summary(model_gap1)$coefficients
p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
print(round(coef_table, 6))

# 5. Extract 95% Wald Confidence Intervals
cat("\n=== 95% WALD CONFIDENCE INTERVALS ===\n")
print(confint(model_gap1, parm = "beta_", method = "Wald"))
