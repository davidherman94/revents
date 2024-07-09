# Install and load the necessary packages
# install.packages("reda")
library(dplyr)
library(tibble)
library(reda)
library(tidyr)
library(gtsummary)

# Set seed for reproducibility
set.seed(123)

# Define a function to simulate recurrent event data using reda
simulate_recurrent_event_data <- function(n, lambda, shape, scale, max_time, mstype, outcome) {
  # Define a baseline hazard function (Weibull in this case)
  basehaz <- rweibull(n, shape = shape, scale = scale)

  # Simulate recurrent event data using the baseline hazard function
  sim_data <- simEventData(n = n, Lambda0 = basehaz, beta = c(lambda = log(lambda)),
                       X = data.frame(id = 1:n), endTime = max_time)

  # Generate censoring status (1 if event observed, 0 if censored)
  censoring_status <- as.integer(sim_data$event == 1)

  #Add rrms column


  # Combine and return the simulated data
  return(tibble(id = sim_data$ID,
                status = censoring_status,
                event_time = sim_data$time,
                mstype = mstype,
                outcome = outcome ))
}

# Define simulation parameters for CDP
lambda_rrms_relapse <- 0.5
shape_rrms_relapse <- 1 #Weibull slope or the threshold parameter (gamma)
scale_rrms_relapse <- 0.97 #characteristic life parameter (alfa)
lambda_spms_relapse <- 0.03
shape_spms_relapse <- 1.1
scale_spms_relapse <- 0.66
max_time <- 1  # Maximum time for censoring

# Define simulation parameters for relapses
lambda_rrms_relapse <- 0.02
shape_rrms_relapse <- 2
scale_rrms_relapse <- 2.5
lambda_spms_relapse <- 0.3
shape_spms_relapse <- 2
scale_spms_relapse <- 2.5
max_time <- 1  # Maximum time for censoring

# Generate the datasets for CDP
rrms_cdp_data <- simulate_recurrent_event_data(791, lambda_rrms_cdp, shape_rrms_cdp, scale_rrms_cdp, max_time, "RRMS", "CDP")
spms_cdp_data <- simulate_recurrent_event_data(522, lambda_spms_cdp, shape_spms_cdp, scale_spms_cdp, max_time, "SPMS", "CDP")
spms_cdp_data$id <- spms_cdp_data$id + 791
# Generate the datasets for relapse
rrms_relapse_data <- simulate_recurrent_event_data(791, lambda_rrms_relapse, shape_rrms_relapse, scale_rrms_relapse, max_time, "RRMS", "RELAPSE")
spms_relapse_data <- simulate_recurrent_event_data(522, lambda_spms_relapse, shape_spms_relapse, scale_spms_relapse, max_time, "SPMS", "RELAPSE")
spms_relapse_data$id <- spms_relapse_data$id + 791

# Bind datasets
df <- bind_rows(rrms_cdp_data,
                spms_cdp_data,
                rrms_relapse_data,
                spms_relapse_data)

#Add baaseline characteristics
# Function to generate data for RRMS
generate_rrms_data <- function(n) {
  tibble(
    id = 1:n,
    Age = rnorm(n, mean = 36.5, sd = 9.1),
    Sex = factor(rbinom(n, 1, prob = 0.70), labels = c("Male", "Female")),
    Race = factor(rbinom(n, 1, prob = 0.88), labels = c("No white", "White")),
    Time_since_diagnosis = rlnorm(n, meanlog = log(2), sdlog = log(5/2)),
    EDSS_overall = rlnorm(n, meanlog = log(2), sdlog = log(3.5/1.5))
  )
}

# Function to generate data for SPMS
generate_spms_data <- function(n) {
  tibble(
    id = 1:n,
    Age = rnorm(n, mean = 49.4, sd = 8.1),
    Sex = factor(rbinom(n, 1, prob = 0.63), labels = c("Male", "Female")),
    Race = factor(rbinom(n, 1, prob = 0.96), labels = c("No white", "White")),
    Time_since_diagnosis = rlnorm(n, meanlog = log(14.5), sdlog = log(22/7.8)), # approximate log-normal
    EDSS_overall = rlnorm(n, meanlog = log(6), sdlog = log(6.5/4.5)) # approximate log-normal
  )
}

# Generate the datasets
rrms_data <- generate_rrms_data(791)
spms_data <- generate_spms_data(522)
spms_data$id <- spms_data$id + 791

#Merge with the previous part
df <- inner_join(bind_rows(rrms_data, spms_data),df, by="id") %>%
  filter(outcome == "RELAPSE")

#Count number of CDP
df_temp <- df %>%
      group_by(id,outcome) %>%
      mutate(cumevent = cumsum(status),
             tot_event = max(cumevent))

#Pivot the dataset
df_temp <- df_temp %>%
           select(id, outcome, tot_event) %>%
           distinct() %>%
           pivot_wider(
              names_from = outcome,
              values_from = tot_event
          )


one_row_df <- df %>%
  select(id,
         mstype,
         Age,
         Sex,
         Race,
         Time_since_diagnosis,
         EDSS_overall) %>%
  inner_join(df_temp, by = "id") %>%
  distinct()


descriptive_table <-
  one_row_df %>%
  tbl_summary(include = c(Age,
                          Sex,
                          Race,
                          Time_since_diagnosis,
                          EDSS_overall,
                          RELAPSE),
              by = mstype,
              statistic = list(
                all_continuous() ~ "{mean} ({sd})"
              ),
              digits = all_continuous() ~ 1,
              label = list(
                Age = "Age (years)",
                Sex = "Sex",
                Race = "Race",
                EDSS_overall = "EDSS overall",
                mstype = "Disease course",
                Time_since_diagnosis = "Time since diagnosis (years)",
                RELAPSE = "n° of relapses"
              ),
              missing_text = "(Missing)") %>%
  modify_header(label = "**Characteristics**") %>%
  modify_caption("**Baseline characteristics** (N = {N})") %>%
  bold_labels() %>%
  italicize_levels()


# Print the table
descriptive_table
