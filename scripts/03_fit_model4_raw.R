# ==============================================================================
# Script: scripts/03_fit_model4_raw.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Fit Model 4 (Raw daily checking without within/between centering)
# ==============================================================================

# 1. Load required modeling packages
library(lme4)

# 2. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 3. Fit Model 4 (Raw daily checking)
model4 <- lmer(
  dayCogFail ~ dayScreenChecking + (1 | participantID),
  data = df,
  REML = FALSE
)

# 4. Display model summary
cat("=== MODEL 4 (RAW CHECKING) SUMMARY ===\n")
print(summary(model4))

# 5. Extract fixed effects and approximate Wald p-values
cat("\n=== FIXED EFFECTS TABLE ===\n")
coef_table <- summary(model4)$coefficients
p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
print(round(coef_table, 6))

# 6. Extract 95% Wald confidence intervals for fixed effects
cat("\n=== 95% WALD CONFIDENCE INTERVALS (FIXED EFFECTS) ===\n")
print(confint(model4, parm = "beta_", method = "Wald"))
