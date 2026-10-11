# ==============================================================================
# Script: scripts/04_sensitivity_and_diagnostics.R
# Project: Smartphone Checking & Cognitive Failures (Reanalysis of Hartanto et al.)
# Purpose: Model 3 diagnostics and sensitivity analysis (excluding STCF-1152)
# ==============================================================================

library(lme4)

# 1. Read dataset
df <- read.csv("data/hartanto_628.csv", stringsAsFactors = FALSE)

# 2. Fit full-sample Model 3
m3_full <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = df,
  REML = FALSE
)

# 3. Fit sensitivity Model 3 excluding STCF-1152
df_sens <- subset(df, participantID != "STCF-1152")
m3_sens <- lmer(
  dayCogFail ~ dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = df_sens,
  REML = FALSE
)

# 4. Compare Fixed Effects
cat("=== FULL SAMPLE VS. SENSITIVITY (EXCLUDING STCF-1152) ===\n")
comp <- data.frame(
  Effect = names(fixef(m3_full)),
  Full_Est = round(fixef(m3_full), 6),
  Sens_Est = round(fixef(m3_sens), 6),
  Full_SE  = round(summary(m3_full)$coefficients[, "Std. Error"], 6),
  Sens_SE  = round(summary(m3_sens)$coefficients[, "Std. Error"], 6)
)
print(comp, row.names = FALSE)

# 5. Inspect random intercepts summary
u0 <- ranef(m3_full)$participantID[, 1]
cat("\n=== RANDOM INTERCEPTS SUMMARY (FULL SAMPLE) ===\n")
print(summary(u0))
cat("Participant STCF-1152 random intercept:", round(ranef(m3_full)$participantID["STCF-1152", 1], 4), "(Maximum)\n")

# 6. Generate diagnostic plots and save to output/
png("output/model3_diagnostics.png", width = 1000, height = 500, res = 120)
par(mfrow = c(1, 2))

# Residuals Q-Q Plot
qqnorm(resid(m3_full), main = "Level-1 Residuals Q-Q Plot", pch = 16, col = rgb(0,0,1,0.3))
qqline(resid(m3_full), col = "red", lwd = 2)

# Random Intercepts Q-Q Plot
qqnorm(u0, main = "Level-2 Random Intercepts Q-Q Plot", pch = 16, col = rgb(0.8,0,0,0.6))
qqline(u0, col = "blue", lwd = 2)

dev.off()
cat("\nDiagnostic plots saved to 'output/model3_diagnostics.png'\n")
