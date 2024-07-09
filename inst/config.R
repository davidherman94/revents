##########
# MODELS
##########

## Model specifications
MAIN_INDEPENDENT <- "DISEASE_COURSE"
COVARIATES <- c("AGE", "SEX", "RACE","DISEASE_DURATION")
CLUSTER <- "cluster(USUBJID)"
STRATA <- "strata(SEVENT)"
FRAILTY_TERM <- "frailty(USUBJID)"

## Formulas
formula_count <- as.formula(paste(
  "COUNT ~", "offset(log(LENGHT.TIME))",
  " + ", paste(COVARIATES, collapse = " + ")
))


formula_cox <- as.formula(paste(
  "Surv(TSTART, TSTOP, EVENT) ~",
  MAIN_INDEPENDENT,
  "+",
  paste(COVARIATES, collapse = " + ")
))

formula_cox_ag <- as.formula(paste(
  "Surv(TSTART, TSTOP, EVENT) ~",
  MAIN_INDEPENDENT,
  "+",
  paste(COVARIATES, collapse = " + ")
))

formula_cox_lwyy <- as.formula(paste(
  "Surv(TSTART, TSTOP, EVENT) ~",
  MAIN_INDEPENDENT,
  "+",
  paste(COVARIATES, collapse = " + "),
  " + ", CLUSTER
))

formula_cox_pwp <-
  # was followed the traditional function without clustering (robust variance)
  as.formula(
    paste(
      "Surv(TSTART, TSTOP, EVENT) ~",
      MAIN_INDEPENDENT,
      "+",
      paste(COVARIATES, collapse = " + "),
      " + ", STRATA
    )
  )

formula_cox_pwp.gp <-
  as.formula(
    paste(
      # can be added also a 0 column rep(0, nrow(df_relapses_model))
      # but the results are the same. Here it the simplest formula as recommend
      # Buhler and Sousa-Ferreira 2019.
      "Surv(TGAP, EVENT) ~",
      "(",
      MAIN_INDEPENDENT,
      "+",
      paste(COVARIATES, collapse = " + "), ")",
      " + ", STRATA
      # was followed the traditional function without clustering (robust variance)
    )
  )

formula_cox_frailty <-
  as.formula(
    paste(
      "Surv(TSTOP, EVENT) ~",
      MAIN_INDEPENDENT,
      "+",
      paste(COVARIATES, collapse = " + "),
      "+",
      FRAILTY_TERM
    )
  )

formula_cox_wlw <-
  as.formula(
    paste(
      "Surv(TSTOP, EVENT) ~",
      "(",
      MAIN_INDEPENDENT,
      "+",
      paste(COVARIATES, collapse = " + "), ")",
      " + ", CLUSTER,
      " + ", STRATA
    )
  )

formula_cox_lwa <- as.formula(paste(
  # LWA model uses the TT formulation as well but is a non-stratified model
  # such as the AG model. Uses the layout of data type WLW # Sousa-Ferreira 2019.
  "Surv(TSTOP, EVENT) ~",
  "(",
  MAIN_INDEPENDENT,
  "+",
  paste(COVARIATES, collapse = " + "), ")",
  " + ",
  CLUSTER
))

formula_cox_event.sp.pwp <- as.formula(
  paste(
    "Surv(TSTART, TSTOP, EVENT) ~",
    STRATA, "/", "(",
    MAIN_INDEPENDENT,
    "+",
    paste(COVARIATES, collapse = " + "), ")"
  )
)

formula_cox_event.sp.wlw <- as.formula(
  paste(
    "Surv(TSTOP, EVENT) ~",
    STRATA, "/", "(",
    MAIN_INDEPENDENT,
    "+",
    paste(COVARIATES, collapse = " + "),")",
    " + ",
    CLUSTER
  )
)

formula_cox_event.sp.prbc <- as.formula(
  paste(
    "Surv(TSTART, TSTOP, EVENT) ~",
    STRATA, "/","(",
    MAIN_INDEPENDENT,
    "+",
    paste(COVARIATES, collapse = " + "),")",
    " + ",
    CLUSTER
  )
)

