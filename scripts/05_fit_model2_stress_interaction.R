# ==============================================================================
# Script: scripts/05_fit_model2_stress_interaction.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Fit Model 2 (Checking deviation by Daily Stressor interaction)
# ==============================================================================

library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Fit Model 2 (Interaction with raw dayStressor)
model2 <- lmer(
  dayCogFail ~ dayScreenChecking_dev * dayStressor + (1 | participantID),
  data = df,
  REML = FALSE
)

# 3. Display summary
cat("=== MODEL 2 (CHECKING x STRESSOR INTERACTION) SUMMARY ===\n")
print(summary(model2))

# 4. Extract fixed effects table with Wald test
cat("\n=== FIXED EFFECTS TABLE ===\n")
coef_table <- summary(model2)$coefficients
p_vals <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
coef_table <- cbind(coef_table, "Pr(>|z|)" = p_vals)
print(round(coef_table, 6))

# 5. Extract 95% Wald confidence intervals
cat("\n=== 95% WALD CONFIDENCE INTERVALS (FIXED EFFECTS) ===\n")
print(confint(model2, parm = "beta_", method = "Wald"))
