# Attentional Fragmentation vs. Screen Immersion: A Multilevel Reanalysis of Daily Smartphone Checking and Cognitive Failures

[![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://www.r-project.org/)
[![lme4](https://img.shields.io/badge/lme4-mixed--effects-green.svg)](https://cran.r-project.org/package=lme4)
[![Open Science](https://img.shields.io/badge/Open%20Science-Reproducible-brightgreen.svg)](https://researchbox.org/628)

An open-science, reproducible reanalysis investigating the within-person and between-person relationships between daily smartphone interactions, acute life stressors, and daily cognitive failures using intensive longitudinal daily diary data (Hartanto et al., ResearchBox #628).

---

## 🔬 Theoretical Motivation & Core Findings

While public health and media narratives frequently treat total "screen time" as a monolithic cognitive hazard, cognitive neuroscience and executive function research suggest that **attentional task-switching (interruption frequency)** and **prolonged immersion (duration)** reflect fundamentally distinct cognitive operations. 

Through multilevel modeling of **1,229 daily observations across 181 participants**, this project establishes three primary empirical contributions:

1. **Pillar 1: Attentional Fragmentation vs. Immersion (Frequency vs. Duration)**
   * **Checking Frequency (Task Switching)** uniquely predicts **increased** daily cognitive failures ($\beta = +0.000506, p = .008$).
   * **Screen Time (Sustained Immersion)** uniquely predicts **decreased** daily cognitive failures ($\beta = -0.012062, p = .018$).
   * Both opposing effects survive adjustment for **11 covariates** (age, sex, household income, subjective SES, daily/mean positive affect, daily/mean negative affect, and daily/mean stressors).
2. **Pillar 2: Temporal Precedence & Directionality ($t \rightarrow t+1$)**
   * The checking cost is strictly an **acute, same-day state phenomenon** ($\beta = +0.000478, p = .024$).
   * Yesterday's phone checking has **zero carry-over effect** on today's cognitive failures ($\beta = +0.000180, p = .430$), ruling out cumulative multi-day depletion.
   * Cross-lagged models confirm that yesterday's cognitive slips do **not** predict today's phone checking ($\beta = -0.105, p = .970$), ruling out reverse causality.
3. **Pillar 3: Parallel Additive Burdens, Not Compounding Vulnerabilities**
   * Both acute daily stressors ($\beta = +0.146, p < .000001$) and chronic baseline stress ($\beta = +0.669, p < .000001$) independently elevate cognitive failures.
   * However, acute stress moderation ($p = .483$) and chronic stress moderation ($p = .770$) are completely null, demonstrating that digital interruptions and life stress act as **independent, parallel burdens** rather than compounding hazards.

---

## 📊 Summary of Primary Multilevel Models

### 1. Final Additive Synthesis Model (Model 3C)
$$\text{dayCogFail}_{ij} = \gamma_{00} + \beta_1 (\text{Checking\_dev}) + \beta_2 (\text{ScreenTime\_dev}) + \beta_3 (\text{Stressor\_dev}) + \gamma_{01} (\text{Checking\_mn}) + \gamma_{02} (\text{ScreenTime\_mn}) + \gamma_{03} (\text{Stressor\_avg}) + u_{0j} + e_{ij}$$

| Predictor Level | Variable Name | Parameter | Estimate ($\beta$) | Std. Error | $t$-value | $p$-value | 95% Wald CI |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Fixed Intercept** | $\gamma_{00}$ | Intercept | `0.1228` | `0.1146` | `1.071` | `.284` | `[-0.1018, 0.3475]` |
| **Level 1 (Within-Person)** | `dayScreenChecking_dev` | $\beta_1$ | **`+0.000649`** | `0.000194` | `3.350` | **`< .001`** | `[ 0.00027,  0.00103]` |
| | `dayScreenTime_dev` (hrs) | $\beta_2$ | **`-0.013546`** | `0.005164` | `-2.623` | **`.008`** | `[-0.02367, -0.00343]` |
| | `dayStressor_dev` (event) | $\beta_3$ | **`+0.146111`** | `0.024590` | `5.942` | **`< .000001`** | `[ 0.09792,  0.19431]` |
| **Level 2 (Between-Person)** | `dayScreenChecking_mn` | $\gamma_{01}$ | `0.000094` | `0.000508` | `0.185` | `.853` | `[-0.00090,  0.00109]` |
| | `dayScreenTime_mn` (hrs) | $\gamma_{02}$ | `0.016427` | `0.014471` | `1.135` | `.256` | `[-0.01193,  0.04479]` |
| | `dayStressor.avg` | $\gamma_{03}$ | **`+0.669343`** | `0.103289` | `6.480` | **`< .000001`** | `[ 0.46690,  0.87178]` |
| **Random Effects** | Participant Intercept ($\tau_{00}$) | | `0.1244` | $SD = 0.353$ | | | |
| | Residual Variance ($\sigma^2$) | | `0.0948` | $SD = 0.308$ | | | |

*Note. $N = 181$ participants, $T = 1,229$ daily observations. Fit with Maximum Likelihood via `lme4::lmer`.*

---

## 📁 Repository Organization

```text
├── README.md                                  # Comprehensive research overview & findings
├── .gitignore                                 # Ignores local R and OS temporary files
├── data/
│   └── hartanto_628.csv                       # Curated daily diary dataset (1,229 obs, 181 participants)
├── scripts/
│   ├── 01_load_and_inspect_data.R             # Step 1: Data verification and summary statistics
│   ├── 02_fit_model3.R                        # Step 2: Unadjusted within/between replication
│   ├── 03_fit_model4_raw.R                    # Step 3: Raw checking model (demonstrating conflation)
│   ├── 04_sensitivity_and_diagnostics.R       # Step 4: Outlier check (STCF-1152) and Q-Q diagnostics
│   ├── 05_fit_model2_stress_interaction.R     # Step 5: Raw checking x stressor interaction audit
│   ├── 06_fit_gap1_checking_vs_duration.R     # Step 6: Simultaneous frequency vs. duration model
│   ├── 07_fit_gap1_fully_adjusted.R           # Step 7: Frequency vs. duration with 11 covariates
│   ├── 08_fit_gap2_lagged_models.R            # Step 8: Cross-lagged directional models (t -> t+1)
│   └── 09_fit_gap3_stress_boundaries.R        # Step 9: Within/between stress boundaries & synthesis
├── output/
│   └── model3_diagnostics.png                 # Residual and random-intercept diagnostic plots
└── ResearchBox 628/                           # Archival replication materials
    ├── Code/ScreenTime & CogFail.R            # Original study R code
    ├── Codebook/appledata.csv___CODEBOOK.csv  # Variable codebook
    └── Other/Supplementary Materials.pdf      # Original supplementary document
```

---

## 💻 Reproducibility Instructions

### Prerequisites
Ensure you have **R (version 4.0.0 or higher)** installed, along with the following packages:
```r
install.packages(c("lme4", "dplyr", "Matrix"))
```

### Reproducing the Complete Analysis
Clone this repository and run the scripts in sequence:
```r
# Set working directory to project root
setwd("/path/to/repository")

# Run full pipeline:
source("scripts/01_load_and_inspect_data.R")
source("scripts/02_fit_model3.R")
source("scripts/04_sensitivity_and_diagnostics.R")
source("scripts/06_fit_gap1_checking_vs_duration.R")
source("scripts/07_fit_gap1_fully_adjusted.R")
source("scripts/08_fit_gap2_lagged_models.R")
source("scripts/09_fit_gap3_stress_boundaries.R")
```

---

## 📖 Citation & Data Provenance
* **Original Dataset**: ResearchBox #628 ([https://researchbox.org/628](https://researchbox.org/628)).
* **Original Study**: Hartanto et al. (2023). Smartphone screen time and daily cognitive failures in everyday life.
