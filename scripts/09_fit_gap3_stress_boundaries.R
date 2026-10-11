# ==============================================================================
# Script: scripts/09_fit_gap3_stress_boundaries.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Test Gap 3 - Stress Boundary Conditions (Within- & Between-Person Moderation)
# ==============================================================================

library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Construct within-person daily stressor deviation
# (dayStressor is binary 0/1; dayStressor.avg is participant mean)
df$dayStressor_dev <- df$dayStressor - df$dayStressor.avg

# 3. Model 3A: Within-Person Stress Moderation
# Tests whether today's checking effect is amplified by today's acute stressor
m_stress_within <- lmer(
  dayCogFail ~ dayScreenChecking_dev * dayStressor_dev + 
               dayScreenChecking_mn + dayStressor.avg + 
               (1 | participantID),
  data = df,
  REML = FALSE
)

cat("=== MODEL 3A: WITHIN-PERSON STRESS MODERATION ===\n")
coef_within <- summary(m_stress_within)$coefficients
p_within <- 2 * (1 - pnorm(abs(coef_within[, "t value"])))
coef_within <- cbind(coef_within, "Pr(>|z|)" = p_within)
print(round(coef_within, 6))

# 4. Model 3B: Cross-Level Stress Moderation (Chronic Stress Buffer)
# Tests whether individuals with high chronic stress exposure are more vulnerable to checking
m_stress_cross <- lmer(
  dayCogFail ~ dayScreenChecking_dev * dayStressor.avg + 
               dayStressor_dev + dayScreenChecking_mn + 
               (1 | participantID),
  data = df,
  REML = FALSE
)

cat("\n=== MODEL 3B: CHRONIC STRESS (CROSS-LEVEL) MODERATION ===\n")
coef_cross <- summary(m_stress_cross)$coefficients
p_cross <- 2 * (1 - pnorm(abs(coef_cross[, "t value"])))
coef_cross <- cbind(coef_cross, "Pr(>|z|)" = p_cross)
print(round(coef_cross, 6))

# 5. Model 3C: Final Clean Additive Model (The Combined Synthesis)
# Frequency + Duration + Stress (Separated into Within and Between)
m_synthesis <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenTime_dev + dayStressor_dev +
               dayScreenChecking_mn + dayScreenTime_mn + dayStressor.avg +
               (1 | participantID),
  data = df,
  REML = FALSE
)

cat("\n=== MODEL 3C: FINAL ADDITIVE SYNTHESIS MODEL ===\n")
coef_syn <- summary(m_synthesis)$coefficients
p_syn <- 2 * (1 - pnorm(abs(coef_syn[, "t value"])))
coef_syn <- cbind(coef_syn, "Pr(>|z|)" = p_syn)
print(round(coef_syn, 6))
cat("\n95% Wald Confidence Intervals for Synthesis Model:\n")
print(confint(m_synthesis, parm = "beta_", method = "Wald"))
