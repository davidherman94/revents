### RELAPSES DERIVATION ####
# a. clinical_event_data_filter in data prep has the relapse time
# b. relapse time and database structure with t-start and t-stop based on CDP_TIME
final_relapses <- supp_dm_data_filter %>%
  # left join subset event_relapse == 1 (in this step for keeping the variable in the original dataset)
  left_join(subset(clinical_event_data_filter, EVENT_RELAPSE == 1), by = "USUBJID") %>%
  dplyr::select(-EVENT_RELAPSE) %>% ## event based on time and not necessary to keep it
  mutate(RELAPSE_TIME = ifelse(RELAPSE_TIME >= ENDFUP_TIME, NA, RELAPSE_TIME)) %>%
  pivot_longer(!USUBJID, names_to = "EVENT", values_to = "TIME") %>%
  distinct() %>%
  filter(!(EVENT == "RELAPSE_TIME" & is.na(TIME) == T)) %>%
  mutate(
    EVENT = ifelse(EVENT == "RELAPSE_TIME", 1, 0),
    PARAMCD = "ARR"
   ) %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, TIME) %>%
  mutate(TSTART = replace_na(lag(TIME),1),
         SEVENT = row_number(),
         NEVENTS = as.numeric(sum(STATUS == 1))) %>%
  rename(TSTOP = TIME) %>%
  dplyr::select(USUBJID, TSTART, TSTOP, EVENT, PARAMCD, SEVENT,NEVENTS)

# c. joining covariates #
df.model.relapses <- final_relapses %>%
  left_join(merged_data_r_events, by = "USUBJID", relationship = "many-to-many") %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, SEVENT) %>%
  # to solve 2 cases with tstart and tstop same day
  mutate(TSTART = ifelse(TSTART == TSTOP, TSTART - 1, TSTART),
         TGAP = TSTOP - TSTART) %>%
  dplyr::select(
    USUBJID,
    PARAMCD,
    TSTART,
    TSTOP,
    TGAP,
    EVENT,
    SEVENT,
    NEVENTS,
    AGE,
    SEX,
    DISEASE_COURSE,
    RACE,
    DISEASE_DURATION
  ) %>%
  distinct()

# length(unique(final_relapses$USUBJID))
# with(final_relapses, table(tapply(EVENT, USUBJID, sum)))

############### Derivation of the CDP ###############
# a. cdp time and counting by definition
cdp <- questionaries_data_filter_r_events %>%
  arrange(USUBJID, TIME_EDSS) %>%
  group_by(USUBJID) %>%
  mutate(
    # Disability progression
    #Time
    disability_progression_time = lag(TIME_EDSS, 1) - lag(TIME_EDSS, 2),
    #Value
    disability_progression_value = lag(EDSS, 1) - lag(EDSS, 2),

    # Clinical Disability progression
    #Time
    cdp_time = TIME_EDSS - lag(TIME_EDSS, 1),
    #Value
    cdp_value = EDSS - lag(EDSS, 1),

    # Baseline EDSS value
    baseline_edss = first(EDSS),

    reference_score = ifelse(baseline_edss <= 5.5, 1, 0.5),

    # Disability progression -> Increase in EDSS by 1 point if baseline EDSS was 5.5 or lower, or increase in EDSS by 0.5 point if baseline EDSS was above 5.5
    disability_progression = if_else(
      disability_progression_time >= 1,
      disability_progression_value >= reference_score, 1, 0),

    #Confirmed progression -> Subsequent visit >=84 days still at the same level or higher than disability progression
    CDP = if_else(disability_progression == 1 & cdp_time >= 84 & cdp_value >= 0, 1, 0)
  ) %>%
  ungroup() %>%
  dplyr::filter(disability_progression == 1) %>%
  dplyr::select(USUBJID, TIME_EDSS) %>%
  rename(CDP_TIME = TIME_EDSS) %>%
  distinct()

# b. database structure with t-start and t-stop based on CDP_TIME
final_cdp <- supp_dm_data_filter %>%
  left_join(cdp, by = "USUBJID") %>%
  mutate(CDP_TIME = ifelse(CDP_TIME >= ENDFUP_TIME, NA, CDP_TIME)) %>%
  pivot_longer(!USUBJID, names_to = "EVENT", values_to = "TIME") %>%
  distinct() %>%
  filter(!(EVENT == "CDP_TIME" & is.na(TIME) == T)) %>%
  mutate(EVENT = ifelse(EVENT == "CDP_TIME", 1, 0)) %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, TIME) %>%
  mutate(
    TSTART = replace_na(lag(TIME), 1),
    PARAMCD = "DP",
    NEVENTS = as.numeric(sum(EVENT == 1)),
    SEVENT = row_number()
  ) %>%
  rename(TSTOP = TIME)  %>%
  dplyr::select(USUBJID, TSTART, TSTOP, EVENT, PARAMCD, NEVENTS, SEVENT)

# c. joining covariates #
df.model.cdp <- final_cdp %>%
  left_join(merged_data_r_events, by = "USUBJID", relationship = "many-to-many") %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, SEVENT) %>%
  ## Avoid errors in the model due few cases (10) to same day of event
  mutate(TSTART = ifelse(TSTART == TSTOP, TSTART - 1, TSTART),
         TGAP = TSTOP - TSTART) %>%
  dplyr::select(
    USUBJID,
    PARAMCD,
    TSTART,
    TSTOP,
    TGAP,
    EVENT,
    SEVENT,
    NEVENTS,
    AGE,
    SEX,
    DISEASE_COURSE,
    RACE,
    DISEASE_DURATION
  ) %>%
  distinct()


# length(unique(df.model.cdp$USUBJID))
# with(final_cdp, table(tapply(EVENT, USUBJID, sum)))
