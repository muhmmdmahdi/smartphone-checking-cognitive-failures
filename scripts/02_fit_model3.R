# ==============================================================================
# Script: scripts/02_fit_model3.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Fit unadjusted Model 3 (Within-person and Between-person checking effects)
# ==============================================================================

# 1. Load required modeling packages
library(lme4)

# Load lmerTest if available (for Satterthwaite p-values), but proceed with lme4 if not
if (requireNamespace("lmerTest", quietly = TRUE)) {
  library(lmerTest)
}

# 2. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 3. Fit unadjusted Model 3
# Note: REML = FALSE matches the original author script (Maximum Likelihood estimation)
model3 <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = df,
  REML = FALSE
)

# 4. Display model summary
cat("=== UNADJUSTED MODEL 3 SUMMARY ===\n")
print(summary(model3))

# 5. Extract fixed effects and approximate Wald p-values if lmerTest wasn't loaded
cat("\n=== FIXED EFFECTS TABLE ===\n")
coef_table <- summary(model3)$coefficients
if (!"Pr(>|t|)" %in% colnames(coef_table)) {
  # Wald approximation (asymptotic z-test)
  p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
  coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
}
print(round(coef_table, 6))

# 6. Extract 95% Wald confidence intervals for fixed effects
cat("\n=== 95% WALD CONFIDENCE INTERVALS (FIXED EFFECTS) ===\n")
print(confint(model3, parm = "beta_", method = "Wald"))
