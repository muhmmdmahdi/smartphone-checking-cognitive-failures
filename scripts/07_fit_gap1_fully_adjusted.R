# ==============================================================================
# Script: scripts/07_fit_gap1_fully_adjusted.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Test Gap 1 (Checking vs. Screen Time) with full covariate adjustment
# ==============================================================================

library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Fit fully adjusted simultaneous model
# Predictors:
# - Checking (within & between)
# - Screen Time (within & between)
# - Demographics: demogFemale, demogAge, sesIncomeHH, sesLadder
# - Daily & Mean Positive Affect: daySwbPAdds_dev, daySwbPAdds_mn
# - Daily & Mean Negative Affect: daySwbNAdds_dev, daySwbNAdds_mn
# - Daily & Mean Stressors: dayStressor, dayStressor.avg

model_adj <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenTime_dev + 
               dayScreenChecking_mn + dayScreenTime_mn +
               demogFemale + demogAge + sesIncomeHH + sesLadder +
               daySwbPAdds_dev + daySwbPAdds_mn +
               daySwbNAdds_dev + daySwbNAdds_mn +
               dayStressor + dayStressor.avg +
               (1 | participantID),
  data = df,
  REML = FALSE
)

# 3. Print Summary
cat("=== FULLY ADJUSTED GAP 1 MODEL (FREQUENCY VS. DURATION) ===\n")
print(summary(model_adj))

# 4. Extract Fixed Effects Table with Wald p-values
cat("\n=== FIXED EFFECTS TABLE ===\n")
coef_table <- summary(model_adj)$coefficients
p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
print(round(coef_table, 6))

# 5. Extract 95% Wald Confidence Intervals
cat("\n=== 95% WALD CONFIDENCE INTERVALS ===\n")
print(confint(model_adj, parm = "beta_", method = "Wald"))
