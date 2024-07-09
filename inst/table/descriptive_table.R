# normality test continue variables and shapiro test
variables_of_interest <- merged_data_r_events %>%
  dplyr::select(EDSS, AGE, DISEASE_DURATION)

histogram_plots <- variables_of_interest %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Histograms for Variables of Interest")


shapiro_test_results_AGE <- shapiro.test(df.count.relapses_model$AGE)

shapiro_test_results_EDSS <- merged_data_r_events %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, TIME_EDSS) %>%
  slice(1) %>%
  ungroup()

shapiro_test_results_EDSS <- shapiro.test(shapiro_test_results_EDSS$EDSS)
shapiro_test_results_DISEASE_COURSE <- shapiro.test(df.count.relapses_model$DISEASE_DURATION)


## merged_data_descriptive for baseline description analyses
baseline_table_strata <- merged_data_r_events %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, TIME_EDSS) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-USUBJID, -TIME_EDSS, -ENDFUP_TIME,
                -RELAPSE_TIME, -EVENT_RELAPSE) %>%
    droplevels() %>%
    tbl_summary(
      by = DISEASE_COURSE,
      type = all_continuous() ~ 'continuous2',
      statistic =
        list(
          all_continuous() ~ c("{mean} ({sd})",
                               "{median} ({p25}, {p75})"),
          all_categorical() ~ "{n} ({p}%)"
        ),
      digits = list(
        AGE = 1,
        SEX = 0,
        RACE = 0,
        EDSS = 1,
        DISEASE_DURATION = 1
      ),
      label = list(
        AGE = "Age (years)",
        SEX = "Sex",
        RACE = "Race",
        EDSS = "EDSS overall",
        DISEASE_COURSE = "Disease course",
        DISEASE_DURATION = "Time since diagnosis (years)"
        ),
      missing = "ifany",
      missing_text = "Missing",
      sort = list(everything() ~ "alphanumeric")
    ) %>%
    add_n() %>%
    add_p() %>% 
    modify_header(label = "**Covariates**") %>%
    modify_footnote(label = " Missing values are not displayed") %>%
    modify_caption("**Baseline characteristics** (N = {N})") %>%
    bold_labels() %>%
    italicize_levels()


## relapses number merged_data_descriptive 
relapses_number_table_strata <- df.model.relapses %>%
  group_by(USUBJID) %>%
  mutate(NEVENTS = factor(NEVENTS)) %>%  # Convertir NEVENTS en factor
  distinct(USUBJID, NEVENTS, DISEASE_COURSE) %>%
  ungroup() %>%
  dplyr::select(-USUBJID, NEVENTS, DISEASE_COURSE) %>%
  droplevels() %>%
  tbl_summary(
    by = DISEASE_COURSE,
    label = list(NEVENTS = "Number of relapses"),
    missing = "ifany",
    sort = list(everything() ~ "frequency")
  ) %>%
  add_n() %>%
  modify_header(label = "**Covariates**") %>%
  bold_labels() %>%
  italicize_levels()
  
  ## cdp_counting for strata
  cdp_number_strata <- df.model.cdp %>%
    group_by(USUBJID) %>%
    distinct(USUBJID, NEVENTS, DISEASE_COURSE) %>%
    ungroup() %>%
    dplyr::select(NEVENTS, DISEASE_COURSE) %>%
    droplevels() %>%
    tbl_summary(
      by = DISEASE_COURSE,
      statistic =
        list(NEVENTS ~ "{n} ({p}%)"),
      label = list(NEVENTS = "Number of CDP"),
      missing = "ifany",
      sort = list(everything() ~ "frequency")
    ) %>%
    add_n() %>%
    modify_header(label = "**Covariates**") %>%
    bold_labels() %>%
    italicize_levels()



# save all tables in same excel editable file
baseline_descriptive.table <- list(
    "Baseline Characteristics" = as.data.frame(baseline_table_strata),
    "Relapses Number" = as.data.frame(relapses_number_table_strata),
    "CDP Number" = as.data.frame(cdp_number_strata)
  )

file_path <-
    "~/dh_recurrent_events/recurrent_event_project/output/table/baseline_tables.xlsx"

write_xlsx(baseline_descriptive.table, file_path)
  
  
  
  
# # merged_data_ NO STRATA descriptive for baseline description analyses
  # baseline_table <- merged_data_r_events %>%
  #   dplyr::select(-USUBJID, -MaxRelapses) %>%
  #   droplevels() %>%
  #   tbl_summary(
  #     type = all_continuous() ~ 'continuous2',
  #     statistic =
  #       list(
  #         all_continuous() ~ c("{mean} ({sd})",
  #                              "{median} ({p25}, {p75})"),
  #         all_categorical() ~ "{n} ({p}%)"
  #       ),
  #     digits = list(
  #       AGE = 1,
  #       SEX = 0,
  #       RACE = 0,
  #       QSORRES = 1,
  #       QSORRES_CAT = 0,
  #       MHDECOD = 0,
  #       DISEASE_DURATION = 1
  #     ),
  #     label = list(
  #       AGE = "Age (years)",
  #       SEX = "Sex",
  #       RACE = "Race",
  #       QSORRES = "EDSS overall",
  #       QSORRES_CAT = "EDSS category",
  #       MHDECOD = "Disease course",
  #       DISEASE_DURATION = "Disease duration (years)"
  #     ),
  #     missing = "no",
  #     sort = list(everything() ~ "alphanumeric")
  #   ) %>%
  #   add_n() %>%
  #   modify_header(label = "**Covariates**") %>%
  #   modify_caption("**Baseline characteristics** (N = {N})") %>%
  #   bold_labels() %>%
  #   italicize_levels()
  # 