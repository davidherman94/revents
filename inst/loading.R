# Load function to read bucket S3
load_data <- function(path) {
  df <- data.frame(botor::s3_read(path, read.csv)) ##function load with botor

  return(df)
}

# Load packages
load_libraries <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# get estimates in frailty, NB and poisson models
get_estimates <- function(estimate, std.error, p.value, conf.low, conf.high, digits = 2) {
  estimates <- tidy(estimate, exp = TRUE, conf.int = TRUE) 
  
  estimates$estimate <- round(estimates$estimate, digits)
  estimates$std.error <- round(estimates$std.error, digits)
  estimates$conf.low <- round(estimates$conf.low, digits)
  estimates$conf.high <- round(estimates$conf.high, digits)
  
  
  return(estimates)
}

## count layout data models ##
df.count.models <- function(data) {
  data <- data %>%
    group_by(USUBJID) %>%
    reframe(
      DISEASE_COURSE = first(DISEASE_COURSE),
      COUNT = as.numeric(sum(EVENT)),
      LENGHT.TIME = last(TSTOP),
      AGE = first(AGE),
      SEX = first(SEX),
      RACE = first(RACE),
      DISEASE_DURATION = first(DISEASE_DURATION)) 
  
  return(data)
}

### wlw model layout ##
df.wlw.layout <- function(data) {
  # Find the maximum SEVENT value in your data
  max_SEVENT <- max(data$SEVENT)
  
  ## to solve the problem of filling missing values in disease course and race
  data <- data[complete.cases(data), ] 
  
  # Create a data frame with all combinations of USUBJID and SEVENT
  all_combinations <- expand.grid(USUBJID = unique(data$USUBJID),
                                  SEVENT = 1:max_SEVENT)
  
  # Merge the original data frame with all combinations of USUBJID and SEVENT
  data <-
    merge(
      all_combinations,
      data,
      by = c("USUBJID", "SEVENT"),
      all.x = TRUE
    ) %>%
    arrange(USUBJID, SEVENT) %>%
    dplyr::select(
      USUBJID,
      PARAMCD,
      TSTOP,
      EVENT,
      DISEASE_COURSE,
      SEVENT,
      SEX,
      AGE,
      RACE,
      DISEASE_DURATION
    ) %>%
    fill(
      PARAMCD,
      TSTOP,
      EVENT,
      DISEASE_COURSE,
      SEVENT,
      SEX,
      AGE,
      RACE,
      DISEASE_DURATION
    )
  
  return(data)
}

###############################################################################
# function run all models

run_models <- function(data, models.rec) {
  results <- lapply(models.rec, function(models.rec) eval(models.rec))
  
  return(results)
}


################################################################################
## Functions for obteining estimates, confidence intervals, AIC, BIC and loglik
get.forest <- function(models) {
  results_df <- data.frame(
    model = character(),
    variable = character(),
    estimate = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    p_value = numeric(),
    AIC = numeric(),
    log_likelihood = numeric(), # Agrega log-likelihood
    BIC = numeric(), # Agrega BIC
    stringsAsFactors = FALSE
  )
  
  for (model_list_name in names(models)) {
    cat("Model list name:", model_list_name, "\n")
    model_list <- models[[model_list_name]]
    
    for (current_model in model_list) {
      cat("Current model class:", class(current_model), "\n")
      
      ## SURVIVAL MODELS
      if (inherits(current_model, "coxph")) { 
        cat("Survival model found.\n")
        # finalfit package for extracting coef from coxmodels
        coef_summary_sur <- fit2df(current_model, condense = FALSE) 
        conf_int_sur <- confint(current_model) 
        p_value <- summary(current_model)$coefficients[,"Pr(>|z|)"] 
        aic <- AIC(current_model) # Extract AIC
        log_likelihood <- logLik(current_model) # Extract log-likelihood
        bic <- BIC(current_model) # Calculate BIC
        # Storing results
        results_df <- rbind(results_df, data.frame(
          model = model_list_name,
          estimate = round(exp(coef(current_model)),2),
          conf_low = round(exp(conf_int_sur[, 1]), 2),
          conf_high = round(exp(conf_int_sur[, 2]), 2),
          p_value = round(p_value, 4),
          AIC = aic,
          log_likelihood = log_likelihood,
          BIC = bic
        )) 
        
        # List models: frailty and glm
      } else if (inherits(current_model, "list")) { # Lista de modelos
        cat("List of models found.\n")
        for (sub_model in current_model) {
          # frailty
          if (inherits(sub_model, "coxph")) { # Modelo de fragilidad
            cat("Frailty model found.\n")
            coef_summary <- coef(summary(sub_model))
            conf_int <- confint(sub_model)
            p_value <- summary(sub_model)$logtest[3]
            print(p_value)
            aic <- AIC(sub_model) # Extract AIC
            log_likelihood <- logLik(sub_model) # Extract log-likelihood
            bic <- BIC(sub_model) # Calculate BIC
            # Storing results
            results_df <- rbind(results_df, data.frame(
              model = model_list_name,
              estimate = round(exp(coef(sub_model)), 2),
              conf_low = round(exp(conf_int[, 1]), 2),
              conf_high = round(exp(conf_int[, 2]), 2),
              p_value = round(p_value, 4),
              AIC = aic,
              log_likelihood = log_likelihood,
              BIC = bic
            ))  
            # glm poisson
          } else if (inherits(sub_model, "glm")) { # Modelo Poisson
            cat("Poisson model found.\n")
            coef_summary <- coef(summary(sub_model))
            conf_int <- confint(sub_model)
            p_value <- tidy(sub_model)$p.value 
            aic <- AIC(sub_model) # Extract AIC
            log_likelihood <- logLik(sub_model) # Extract log-likelihood
            bic <- BIC(sub_model) # Calculate BIC
            # Storing results
            results_df <- rbind(results_df, data.frame(
              model = model_list_name,
              estimate = round(exp(coef_summary[, "Estimate"]), 2),
              conf_low = round(exp(conf_int[, 1]), 2),
              conf_high = round(exp(conf_int[, 2]), 2),
              p_value = round(p_value, 4),
              AIC = aic,
              log_likelihood = log_likelihood,
              BIC = bic
            ))  
          }
        }
      }
    }
  }
  
  return(results_df)
}
