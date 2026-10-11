# ----------------Prepare------------------

library(dplyr)
library(lme4)
library(lmerTest)
library(lavaan)
library(semTools)
library(effectsize)
library(psych)

options(scipen = 999)

# load data
apple.data = read.csv("appledata.csv")

# create cognitive failure factors for exploratory analysis portion
apple.data <- apple.data %>%
  mutate(
    dayCogFailDistract = (itemDAYCOGFAIL01 + itemDAYCOGFAIL02 + itemDAYCOGFAIL04 + itemDAYCOGFAIL08 + itemDAYCOGFAIL13)/5,
    dayCogFailBlunders = (itemDAYCOGFAIL03 + itemDAYCOGFAIL05 + itemDAYCOGFAIL06 + itemDAYCOGFAIL11 + itemDAYCOGFAIL12)/5,
    dayCogFailMem = (itemDAYCOGFAIL07 + itemDAYCOGFAIL09 + itemDAYCOGFAIL10)/3
    )

# get standardised data so that std coeffs can be extracted later
apple.data.std = effectsize::standardize(apple.data)

# -------------------Descriptives-------------------------------------

# L2 descriptives
apple.data %>%
  dplyr::distinct(participantID, .keep_all = TRUE) %>%
  dplyr::select(demogNonChinese, demogFemale, demogAge, sesIncomeHH, sesLadder) %>%
  psych::describe()

# L1 descriptives
apple.data %>%
  dplyr::select(
    dayCogFail,
    dayScreenChecking, dayScreenTime,
    dayScreenSocial, dayScreenTools, dayScreenSpending,
    dayScreenEntGame, dayScreenHealth, dayScreenOthers,
    dayStressor.avg, daySwbPAdds, daySwbNAdds
  ) %>%
  psych::describe()

# L1 reliablity: cognitive failure
" 
level: 1
  cogfailW =~ itemDAYCOGFAIL01 + itemDAYCOGFAIL02 + itemDAYCOGFAIL03 + itemDAYCOGFAIL04 + itemDAYCOGFAIL05 + itemDAYCOGFAIL06 + itemDAYCOGFAIL07 + itemDAYCOGFAIL08 + itemDAYCOGFAIL09 + itemDAYCOGFAIL10 + itemDAYCOGFAIL11 + itemDAYCOGFAIL12 + itemDAYCOGFAIL13
level: 2
  cogfailB =~ itemDAYCOGFAIL01 + itemDAYCOGFAIL02 + itemDAYCOGFAIL03 + itemDAYCOGFAIL04 + itemDAYCOGFAIL05 + itemDAYCOGFAIL06 + itemDAYCOGFAIL07 + itemDAYCOGFAIL08 + itemDAYCOGFAIL09 + itemDAYCOGFAIL10 + itemDAYCOGFAIL11 + itemDAYCOGFAIL12 + itemDAYCOGFAIL13
" %>% lavaan::cfa(data = apple.data, cluster = "participantID") %>% semTools::reliability()

# L1 reliablity: positive affect

" 
level: 1
  paW =~ itemDAYPA01 + itemDAYPA02 + itemDAYPA03 + itemDAYPA04 + itemDAYPA05 + itemDAYPA06 + itemDAYPA07 + itemDAYPA08 + itemDAYPA09 + itemDAYPA10 + itemDAYPA11 + itemDAYPA12 + itemDAYPA13
level: 2
  paB =~ itemDAYPA01 + itemDAYPA02 + itemDAYPA03 + itemDAYPA04 + itemDAYPA05 + itemDAYPA06 + itemDAYPA07 + itemDAYPA08 + itemDAYPA09 + itemDAYPA10 + itemDAYPA11 + itemDAYPA12 + itemDAYPA13
" %>% lavaan::cfa(data = apple.data, cluster = "participantID") %>% semTools::reliability()

# L1 reliablity: negative affect

" 
level: 1
  naW =~ itemDAYNA01 + itemDAYNA02 + itemDAYNA03 + itemDAYNA04 + itemDAYNA05 + itemDAYNA06 + itemDAYNA07 + itemDAYNA08 + itemDAYNA09 + itemDAYNA10 + itemDAYNA11 + itemDAYNA12 + itemDAYNA13 + itemDAYNA14
level: 2
  naB =~ itemDAYNA01 + itemDAYNA02 + itemDAYNA03 + itemDAYNA04 + itemDAYNA05 + itemDAYNA06 + itemDAYNA07 + itemDAYNA08 + itemDAYNA09 + itemDAYNA10 + itemDAYNA11 + itemDAYNA12 + itemDAYNA13 + itemDAYNA14
" %>% lavaan::cfa(data = apple.data, cluster = "participantID") %>% semTools::reliability()


# ---------- Start of Analyses ------------------------------------------

# custom function to get both main summary and confint at once
summary_and_confint = function(lmer.output) {
  lmer.output %>% summary() %>% print(correlation = FALSE)
  lmer.output %>% confint(parm = "beta_") %>% print()
  return(invisible(lmer.output))
}

# set up covariates to allow pasting instead of retyping (minimise human error)
covariates_model2 = "+ demogFemale + demogAge + sesIncomeHH + sesLadder"
covariates_model3 =
  paste0(
    covariates_model2,
    "+ daySwbPAdds_dev + daySwbPAdds_mn + daySwbNAdds_dev + daySwbNAdds_mn + dayStressor + dayStressor.avg"
  )

# ---------- Checking ------------------------------------------
# Checking predict Cognitive Failure (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Cognitive Failure (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Tools  ------------------------------------------
# Tools Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Health ------------------------------------------
# Health Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Others ------------------------------------------
# Others Smartphone Usage predict cognitive failure?

#> ------ Unadjusted Model -------

lmer(
  dayCogFail ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFail ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFail ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


# --------------------Multilevel Confirmatory Analysis -----------------------------------------------------
twolevel <- '
level: 1
  distractibilityW =~ itemDAYCOGFAIL01 + itemDAYCOGFAIL02 + itemDAYCOGFAIL04 + itemDAYCOGFAIL08 + itemDAYCOGFAIL13
  blundersW =~ itemDAYCOGFAIL03 + itemDAYCOGFAIL05 + itemDAYCOGFAIL06 + itemDAYCOGFAIL11 + itemDAYCOGFAIL12
  memoryW =~ itemDAYCOGFAIL07 + itemDAYCOGFAIL09 + itemDAYCOGFAIL10

level: 2
  distractibilityB =~ itemDAYCOGFAIL01 + itemDAYCOGFAIL02 + itemDAYCOGFAIL04 + itemDAYCOGFAIL08 + itemDAYCOGFAIL13
  blundersB =~ itemDAYCOGFAIL03 + itemDAYCOGFAIL05 + itemDAYCOGFAIL06 + itemDAYCOGFAIL11 + itemDAYCOGFAIL12
  memoryB =~ itemDAYCOGFAIL07 + itemDAYCOGFAIL09 + itemDAYCOGFAIL10
'

results <- cfa(twolevel, data = apple.data, cluster = 'participantID', mimic="Mplus")
summary(results, fit.measures = T, standardized = T)

lavaanPlot::lavaanPlot(
  model = results,
  node_options = list(shape = "box", fontname = "Helvetica"),
  edge_options = list(color = "grey"),
  coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE)

semPlot::semPaths(results,
                  whatLabels="std",
                  layout="tree")

# --------------------Exploratory Analysis: Cognitive Failure (Distractibility) ---------------------------------------------------------
# ---------- Cognitive Failure (Distractibility): Checking ------------------------------------------
# Checking predict Cognitive Failure (Distractibility) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Cognitive Failure (Distractibility) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): Tools  ------------------------------------------
# Tools Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): Health ------------------------------------------
# Health Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Distractibility): Others ------------------------------------------
# Others Smartphone Usage predict cognitive failure (distractibility)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailDistract ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailDistract ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# --------------------Exploratory Analysis: Cognitive Failure (Blunders) ---------------------------------------------------------
# ---------- Cognitive Failure (Blunders): Checking ------------------------------------------
# Checking predict Cognitive Failure (Blunders) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Cognitive Failure (Blunders) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): Tools  ------------------------------------------
# Tools Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): Health ------------------------------------------
# Health Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Blunders): Others ------------------------------------------
# Others Smartphone Usage predict cognitive failure (blunders)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailBlunders ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailBlunders ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# --------------------Exploratory Analysis: Cognitive Failure (Memory) ---------------------------------------------------------
# ---------- Cognitive Failure (Memory): Checking ------------------------------------------
# Checking predict Cognitive Failure (Memory) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Cognitive Failure (Memory) (DV)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): Tools  ------------------------------------------
# Tools Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()


#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): Health ------------------------------------------
# Health Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- Cognitive Failure (Memory): Others ------------------------------------------
# Others Smartphone Usage predict cognitive failure (memory)?

#> ------ Unadjusted Model -------

lmer(
  dayCogFailMem ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  dayCogFailMem ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "dayCogFailMem ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model3
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# --------------------Exploratory Analysis: Negative Affect  ---------------------------------------------------------

# ---------- NA: Checking ------------------------------------------
# Checking predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Positive Affect (DV)?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: Tools  ------------------------------------------
# Tools Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: Health ------------------------------------------
# Health Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- NA: Others ------------------------------------------
# Others Smartphone Usage predict negative affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbNAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbNAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbNAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# --------------------Exploratory Analysis: Positive Affect  ---------------------------------------------------------

# ---------- PA: Checking ------------------------------------------
# Checking predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenChecking_dev + dayScreenChecking_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: Overall Smartphone Use ------------------------------------------

# Overall Smartphone usage (IV) predict Positive Affect (DV)?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenTime_dev + dayScreenTime_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: Social Smartphone Use ------------------------------------------
# Social Category Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data,REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenSocial_dev + dayScreenSocial_mn + (1 + dayScreenSocial_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: Tools  ------------------------------------------
# Tools Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenTools_dev + dayScreenTools_mn + (1 + dayScreenTools_dev | participantID) +",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: ProductivityFinance ------------------------------------------
# ProductivityFinance Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenSpending_dev + dayScreenSpending_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: EntertainmentGames  ------------------------------------------
# EntertainmentGames Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenEntGame_dev + dayScreenEntGame_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: Health ------------------------------------------
# Health Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenHealth_dev + dayScreenHealth_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

# ---------- PA: Others ------------------------------------------
# Others Smartphone Usage predict positive affect?

#> ------ Unadjusted Model -------

lmer(
  daySwbPAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Unadjusted Model with Standardized Coefficient -------

lmer(
  daySwbPAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()

#> ------ Adjusted Model with Standardized Coefficient ------

lmer(
  as.formula(paste0(
    "daySwbPAdds ~ 1 + dayScreenOthers_dev + dayScreenOthers_mn + (1 | participantID)",
    covariates_model2
  )),
  data = apple.data.std,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
) %>% summary_and_confint()



# --------------------End of Analysis---------------------------------------------------------