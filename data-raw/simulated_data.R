## code to prepare `simulated_data` dataset goes here

simulated_data <- read_csv("data-raw/estimates.csv")

spec(simulated_data)

usethis::use_data(simulated_data, overwrite = TRUE)

usethis::use_data(data, overwrite = TRUE)

# https://psyteachr.github.io/intro-r-pkgs/02-data.html ## explanation for adding data

c(
  "Model",
  "paramcd",
  "overall_estimate",
  "conf.int",
  "conf_low",
  "conf_high",
  "p_value",
  "log_likelihood",
  "AIC",
  "BIC",
  "type_model"
)
