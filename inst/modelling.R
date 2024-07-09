rm(list = ls())

# Set environment
setwd("~/dh_recurrent_events/recurrent_event_project/")

# Load config and loading functions
source("~/dh_recurrent_events/recurrent_event_project/derived/data_prep.R") 
source("~/dh_recurrent_events/recurrent_event_project/derived/endpoints.model.df.R")

#####################################
## MODEL APPLICACION FOR RELAPSES ###
#####################################
# load dataframes for modelling
## merging dataframes for survival overall estimates models
df.recurrent.models <- bind_rows(df.model.relapses, df.model.cdp) 
## Count based model dataframes
df.count.relapses_model <- df.count.models(df.model.relapses) # ARR endpoint
df.count.cdp_model <- df.count.models(df.model.cdp) # CDP endpoint

# Dataframes ordered by SEVENT (event sequence) - WLW-LWA layout
df.wlw.relapses_model <- df.wlw.layout(df.model.relapses) # ARR endpoint
df.wlw.cdp_model <- df.wlw.layout(df.model.cdp) # CDP endpoint

############################
# MEAN CUMULATIVE FUNCTION #
############################
# Survr function changed into "Recur" function from Reda pck
mcf.model <- function(data) {
  fit <-
    # check soft: throw warnings when problem of data structure
    # origin: time origin of all subjects (0 default)
    mcf(Recur(TSTOP, USUBJID, EVENT, origin = 1) ~ DISEASE_COURSE,
        data = data, na.action = na.exclude)

    return(fit)
}

fit_mcf.relapse <- mcf.model(subset(df.recurrent.models, PARAMCD == "ARR"))
fit_mcf.cdp <- mcf.model(subset(df.recurrent.models, PARAMCD == "DP"))

##################################
#  COX PROPORTIONAL HAZARD MODEL #
##################################
cox.model <- function(data, formula_cox) {
  fit <- coxph(formula = formula_cox, ties = "breslow", data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox), collapse = ""))
  formula <- print(paste("coxph(", formula, ", SEVENT == 1, data = data.layout.1", ")", sep = ""))
  
  ## scientific false
  options(scipen = 999)
  
  return(list(formula,
         fit))
}

cox.relapses <- cox.model(subset(df.recurrent.models, PARAMCD == "ARR" & SEVENT == 1), formula_cox)
cox.cdp <- cox.model(subset(df.recurrent.models, PARAMCD == "DP" & SEVENT == 1), formula_cox)

########################
## COUNT BASED MODELS ##
########################
# POISSON regression model #
poisson.model <- function(data) {
  fit <- glm(
    COUNT ~ offset(log(LENGHT.TIME)) + DISEASE_COURSE + AGE + SEX +
      RACE + DISEASE_DURATION, family = poisson(link = "log"), data = data,
    na.action = na.exclude)
  
  formula <- gsub("\\s+", " ", paste(deparse(formula_count), collapse = ""))
  formula <- print(paste("glm(", formula,"),family = poisson(link = 'log'), data = data.layout.1", sep = ""))
  
  poisson_estimates <- get_estimates(fit)
   
  # Get the residual degrees of freedom
  # residual_deviance <- deviance(fit)
  # residual_df <- df.residual(fit)
  # Calculate the ratio
  #overdispersion_ratio <- residual_deviance / residual_df
  return(list(formula,
              summary(fit),
              poisson_estimates
              #overdispersion_ratio
              ))
} 

possion.relapses <- poisson.model(df.count.relapses_model)
possion.cdp <- poisson.model(df.count.cdp_model)


# QUASI-POISSON regression model #
quasi.poisson.model <- function(data) {
  fit <- glm(formula_count, family = quasipoisson(link = "log"), data = data,
    na.action = na.exclude)
  
  formula <- gsub("\\s+", " ", paste(deparse(formula_count), collapse = ""))
  formula <- print(paste("glm(", formula,"),family = quasipoisson(link = 'log'), data = data.layout.1", sep = ""))
  
  quasi.poisson_estimates <- get_estimates(fit)
  
  ## manual log-lik and aic calculation for this particular model
  # BIC seems to be not available for quasi poisson
  loglik <- sum(dpois(fit$y, fit$fitted.values, log = TRUE))
  phi <- summary(fit)$dispersion
  qaic <- -2 * loglik/phi + 2 * length(data)
  
  # print(loglik)
  # print(qaic)

  return(list(formula,
              summary(fit),
              quasi.poisson_estimates))
} 

qpoisson.relapses <- quasi.poisson.model(df.count.relapses_model)
qpoisson.cdp <- quasi.poisson.model(df.count.cdp_model)


##############################################
# NEGATIVE BINOMIAL WITH RANDOM EFFECT (NB) #
# https://www.rdocumentation.org/packages/MASS/versions/7.3-60.0.1/topics/glm.nb
# the common version doesn't account for the heterogeneity, simiarly to poisson
NB.model <- function(data) {
  fit <- glm.nb(formula_count, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_count), collapse = ""))
  formula <- print(paste("glm.nb(", formula,"), data = data.layout.1", sep = ""))
  
  NB_estimates <- get_estimates(fit)
  
  return(list(formula, 
              summary(fit),
              NB_estimates))
} 

nb.relapses <- NB.model(df.count.relapses_model)
nb.cdp <- NB.model(df.count.cdp_model)

##################
## CONDITIONALS ##
##################

# ANDERSEN-GILL (AG) #
AG.model <- function(data, formula_cox_ag) {
  fit <- coxph(formula_cox_ag, na.action = na.exclude, data = data)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_ag), collapse = ""))
  formula <- print(paste("coxph(", formula,"), data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

ag.relapses <- AG.model(subset(df.recurrent.models, PARAMCD == "ARR"), formula_cox_ag)
ag.cdp <- AG.model(subset(df.recurrent.models, PARAMCD == "DP"), formula_cox_ag)

# Prentice, Williams and Peterson (PWP)
# PWP-TT model common (overall effect) #
PWP_TT.model <- function(data, formula_cox_pwp) {
  fit <- coxph(formula_cox_pwp, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_pwp), collapse = ""))
  formula <- print(paste("coxph(", formula,"), data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
  }

pwp.tt.relapses <- PWP_TT.model(subset(df.recurrent.models, PARAMCD == "ARR"), formula_cox_pwp)
pwp.tt.cdp <- PWP_TT.model(subset(df.recurrent.models, PARAMCD == "DP"), formula_cox_pwp)

# PWP-GT model common (overall effect) #
PWP_GT.model <- function(data, formula_cox_pwp.gp) {
  fit <- coxph(formula_cox_pwp.gp, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_pwp.gp), collapse = ""))
  formula <- print(paste("coxph(", formula,"), data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

pwp.gt.relapses <- PWP_GT.model(subset(df.recurrent.models, PARAMCD == "ARR"),formula_cox_pwp.gp)
pwp.gt.cdp <- PWP_GT.model(subset(df.recurrent.models, PARAMCD == "DP"),formula_cox_pwp.gp)

# PWP-TT model event specific (event-strata specific effect)#
PWP.model.specific <- function(data, formula_cox_event.sp.pwp) {
  fit <- coxph(formula_cox_event.sp.pwp, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_event.sp.pwp), collapse = ""))
  formula <- print(paste("coxph(", formula,", SEVENT <= 3), data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

# call only sevent <= 3 as higher sevent has less observations as less reliability of estimates
pwp.ev.sp.relapses <- PWP.model.specific(data = subset(df.recurrent.models, PARAMCD == "ARR" & SEVENT <= 3),
                   formula_cox_event.sp.pwp)
pwp.ev.sp.cdp <- PWP.model.specific(data = subset(df.recurrent.models, PARAMCD == "DP" & SEVENT <= 3),
                   formula_cox_event.sp.pwp)

# Frailty model #
frailty.model <- function(data, formula_cox_frailty) {
  # coxph extended frailty model
  fit <- coxph(formula_cox_frailty, data = data, na.action = na.exclude)

  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_frailty), collapse = ""))
  formula <- print(paste("coxph(", formula,", data = data.layout.1", sep = ""))
  
  # get the frailty variance and Kendall's tau
  # https://journals.sagepub.com/doi/full/10.1177/0962280220921889
  # frailty_var <- emfrail(Surv(TSTOP, EVENT) ~ cluster(USUBJID) + DISEASE_COURSE 
  #                    + AGE + SEX + RACE + DISEASE_DURATION, data = data)
  # summary(frailty_var)
  
  frailty_estimates <- get_estimates(fit)
  
  return(list(formula,
              summary(fit),
              # frailty_var,
              frailty_estimates
              ))
}

frailty.relapses <- frailty.model(subset(df.recurrent.models, PARAMCD == "ARR"),formula_cox_frailty)
frailty.cdp <- frailty.model(subset(df.recurrent.models, PARAMCD == "DP"),formula_cox_frailty)

#############
# MARGINALS #
#############

# Wei-Lin-Weissfeld model common (overall efect) #
WLW.model.common <- function(data, formula_cox_wlw) {
    fit <- coxph(formula_cox_wlw, data = data, na.action = na.exclude)
    
    # printing the used formula
    formula <- gsub("\\s+", " ", paste(deparse(formula_cox_wlw), collapse = ""))
    formula <- print(paste("coxph(", formula,", data = data.layout.3", sep = ""))
    
    options(scipen = 999) ## scientific notation false
    
    return(list(formula,
                fit))
}

wlw.relapses <- WLW.model.common(df.wlw.relapses_model, formula_cox_wlw)
wlw.cpd <- WLW.model.common(df.wlw.cdp_model, formula_cox_wlw)

# Wei-Lin-Weissfeld model specific (event-strata specific effect) #
WLW.model.specific <- function(data,formula_cox_event.sp.wlw) {
  fit <- coxph(formula_cox_event.sp.wlw, data = data, na.action = na.exclude)

  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_event.sp.wlw), collapse = ""))
  formula <- print(paste("coxph(", formula,", SEVENT <= 3), data = data.layout.3", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

wlw.ev.sp.relapses <- WLW.model.specific(data = subset(df.wlw.relapses_model, SEVENT <= 3), formula_cox_event.sp.wlw)
wlw.ev.sp.cdp <- WLW.model.specific(data = subset(df.wlw.cdp_model, SEVENT <= 3), formula_cox_event.sp.wlw)

# fit <- coxph(Surv(TSTOP, EVENT) ~ strata(SEVENT) / (DISEASE_COURSE + AGE + SEX + RACE + 
#         DISEASE_DURATION) + cluster(USUBJID), na.action = na.exclude, data = subset(df.wlw.relapses_model, SEVENT <= 3))

# Lee-Wei-Amato model (LWA) 
LWA.model <- function(data, formula_cox_lwa) {
  fit <- coxph(formula_cox_lwa, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_lwa), collapse = ""))
  formula <- print(paste("coxph(", formula,", data = data.layout.3", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

lwa.relapses <- LWA.model(df.wlw.relapses_model, formula_cox_lwa)
lwa.cdp <- LWA.model(df.wlw.cdp_model, formula_cox_lwa)

# Lin-Wei-Yang-Ying (LWYY) #
# The argument cluster(id) identifies the clusters of correlated observations 
# and estimates the variance based on a grouped jacknife.
LWYY.model <- function(data, formula_cox_lwyy) {
  fit <- coxph(formula_cox_lwyy, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_lwyy), collapse = ""))
  formula <- print(paste("coxph(", formula,", data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

lwyy.relapses <- LWYY.model(subset(df.recurrent.models, PARAMCD == "ARR"), formula_cox_lwyy)
lwyy.cdp <- LWYY.model(subset(df.recurrent.models, PARAMCD == "DP"), formula_cox_lwyy)

# Partially conditional rate-based (PCRB) #
PRCB.model <- function(data, formula_cox_event.sp.prbc) {
  fit <- coxph(formula_cox_event.sp.prbc, data = data, na.action = na.exclude)
  
  # printing the used formula
  formula <- gsub("\\s+", " ", paste(deparse(formula_cox_event.sp.prbc), collapse = ""))
  formula <- print(paste("coxph(", formula,", SEVENT <= 3), data = data.layout.2", sep = ""))
  
  options(scipen = 999) ## scientific notation false
  
  return(list(formula,
              fit))
}

prcb.relapses <- PRCB.model(data = subset(df.recurrent.models, PARAMCD == "ARR" & SEVENT <= 3),
           formula_cox_event.sp.prbc)
prcb.cdp <- PRCB.model(data = subset(df.recurrent.models, PARAMCD == "DP" & SEVENT <= 3),
           formula_cox_event.sp.prbc)


# Running models together ##
# list of models: exclude mcf
models.rec <- list(
  Coxph = list(cox.relapses, cox.cdp),
  Poisson = list(possion.relapses, possion.cdp),
  QPoisson = list(qpoisson.relapses, qpoisson.cdp),
  NB = list(nb.relapses, nb.cdp),
  AG = list(ag.relapses, ag.cdp),
  PWP.TT = list(pwp.tt.relapses, pwp.tt.cdp),
  PWP.GT = list(pwp.gt.relapses, pwp.gt.cdp),
  PWP.SP = list(pwp.ev.sp.relapses, pwp.ev.sp.cdp),
  Frailty = list(frailty.relapses, frailty.cdp),
  WLW = list(wlw.relapses, wlw.cpd),
  WLW.SP = list(wlw.ev.sp.relapses, wlw.ev.sp.cdp),
  LWA = list(lwa.relapses, lwa.cdp),
  LWYY = list(lwyy.relapses, lwyy.cdp),
  PRCB.SP = list(prcb.relapses, prcb.cdp)
)

# function run models
recurrent.models <- run_models(data, models.rec)

# ## Checking assumptions for traditional cox model
# ph_assumption.relapses <- coxph(formula_cox, ties = "breslow", data = data, 
#                        na.action = na.exclude)
# 
# ph_assumption.dp <- coxph(formula_cox, ties = "breslow", data = 
#                             data, na.action = na.exclude)
# 
# test_ph.relapses <- cox.zph(ph_assumption.relapses)
# test_ph.dp <- cox.zph(ph_assumption.dp)
# 
# # the second one is not fullfilling the assumption
# print(list(test_ph.relapses, test_ph.dp))
# 
# plot(test_ph.dp)
# ggcoxzph(test_ph.dp)

# multicollinearity check
#cvif <- vif(test_ph.relapses) # library(rms)

# # save two tables as excel format.
# layout_data <- list(
#   "recurrent_data" = as.data.frame(df.model.relapses),
#   "Event-Specific" = as.data.frame(df.wlw.relapses_model),
#   "count_data" = as.data.frame(df.count.relapses_model)
# )
# 
# file_path_datastructure <- "~/dh_recurrent_events/recurrent_event_project/output/table/layout.xlsx"
# write_xlsx(layout_data, file_path_datastructure)
