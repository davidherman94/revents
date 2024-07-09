## running get forest function from all models.
final_results_df <- get.forest(recurrent.models)

#filter only disease course estimate
final_results_df <- final_results_df %>%
  filter(grepl("DISEASE_COURSE", rownames(.))) %>%
  `rownames<-`(NULL)

# Overall effect table
final_results_df.gral <- final_results_df %>%
  filter(!model %in% c("PWP.SP","WLW.SP","PRCB.SP")) %>%
  mutate(
    paramcd = ifelse(row_number() %% 2 == 1, "ARR", "CDP"),
    paramcd = factor(paramcd, levels = c("CDP", "ARR")),
    conf.int = paste(sprintf("%.2f", conf_low), sprintf("%.2f", conf_high), sep = " - "),
    model = fct_relevel(model, "Coxph", "Poisson", "QPoisson", "NB", 
                        "AG", "PWP.TT", "PWP.GT", "Frailty", "WLW",
                        "LWA", "LWYY")
  ) %>%
  mutate(
    type_model = case_when(
      model %in% c("Coxph") ~ "Time to first event",
      model %in% c("Poisson", "QPoisson", "NB") ~ "Count-based models",
      TRUE ~ "Survival Cox extensions models"
    )
  ) %>%
  dplyr::select(
    model, paramcd, estimate, conf.int, conf_low, conf_high, p_value, log_likelihood, AIC, BIC, type_model
  ) %>%
  rename(
    Model = model, 
    overall_estimate = estimate
  ) %>%
  arrange(paramcd, Model)


# Event specific table 
final_results_df.event.specific <- final_results_df %>%
  filter(model %in% c("PWP.SP", "WLW.SP", "PRCB.SP")) %>%
  group_by(model) %>%
  mutate(paramcd = ifelse(row_number() <= 3, "ARR", "CDP"),
         paramcd = factor(paramcd, levels = c("CDP", "ARR")),
         conf.int = paste(sprintf("%.2f", conf_low), sprintf("%.2f", conf_high), sep = " - ")) %>%
  group_by(model,paramcd) %>%
  mutate(sevent = row_number(),
         sevent_model = paste(model, sevent, sep = ".")) %>% # create an order for chart
  dplyr::select(model, paramcd, sevent_model, estimate, conf.int, conf_low, conf_high, p_value, log_likelihood, AIC, BIC) %>%
  rename(Model =  model, 
         overall_estimate = estimate) %>%
  arrange(paramcd, sevent_model)

# save two tables as excel format.
event_specific_tables <- list(
  "Overall Estimates" = as.data.frame(final_results_df.gral),
  "Event-Specific Estimates" = as.data.frame(final_results_df.event.specific)
)

file_path_estimates <- "~/dh_recurrent_events/recurrent_event_project/output/table/estimates_tables.xlsx"
write_xlsx(event_specific_tables, file_path_estimates)
