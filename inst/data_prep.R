rm(list = ls())

# Set environment
setwd("~/dh_recurrent_events/recurrent_event_project/")

# Load config and loading functions
source("~/dh_recurrent_events/recurrent_event_project/config/config.R") # set path for datasets and working directory
source("~/dh_recurrent_events/recurrent_event_project/functions/loading.R") # load functions

# Load packages
load_libraries(PACKAGES)

# load and define MSOAC dataframes #
demographics_data <- load_data(DEMOGRAPHICS)
clinical_event_data <- load_data(CLINICAL_EVENTS)
medical_history_data <- load_data(MEDICAL_HISTORY)
questionaries_data <- load_data(QUESTIONARIES)
supp_mh_data <- load_data(SUPP_MH)
supp_dm_data <- load_data(SUPP_DM)

####################################
### data preparation of datasets ###
####################################

########################
# DEMOGRAPHIC (DM) DATA#
########################
demographics_data <- demographics_data %>%
  mutate(RACE = factor(
    case_when(
      # recode of race as most of caterogies were under 20 cases or less#
      RACE == "WHITE" ~ "White",
      RACE %in% c(
        "OTHER",
        "ASIAN",
        "AMERICAN INDIAN OR ALASKA NATIVE",
        "BLACK OR AFRICAN AMERICAN",
        "HISPANIC OR LATINO"
      ) ~ "No white",
      TRUE ~ NA_character_
    ),
    # "healthy" reference
    levels = c("White", "No white")
  ),
  SEX = factor(
    case_when(SEX == "M" ~ "Male",
              SEX == "F" ~ "Female",
              TRUE ~ NA_character_),
    # "healthy" reference
    levels = c("Male", "Female")
  ))

# Filter relevant variables
demographics_data_filter <- demographics_data %>%
  dplyr::select(USUBJID, AGE, SEX, RACE)

# CLINICAL EVENT (CE) DATA#
clinical_event_data_filter <- clinical_event_data %>%
  mutate(EVENT_RELAPSE = as.numeric(str_extract(MIDS, "\\d$"))) %>%
  mutate(EVENT_RELAPSE = ifelse(!is.na(EVENT_RELAPSE), 1, 0)) %>% 
  filter(CESTDY >= 1) %>%
  dplyr::select(USUBJID, CESTDY, EVENT_RELAPSE) %>%
  rename(RELAPSE_TIME = CESTDY) %>%
  distinct()

#############################
# MEDICAL HISTORY (MH) DATA #
#############################
medical_history_data_filter <- medical_history_data %>%
  dplyr::filter(
    MHTERM %in% c(
      "PPMS",
      "RRMS",
      "SPMS",
      "RELAPSING-REMITTING",
      # synonym of RRMS
      "MULTIPLE SCLEROSIS",
      # some RRMS were inside of that TERM
      "PRIMARY-PROGRESSIVE",
      # synonym of PPMS
      "SECONDARY-PROGRESSIVE"
    ),
    #synonym of SPMS
    MHDECOD %in% c(
      "Primary progressive multiple sclerosis",
      "Relapsing-remitting multiple sclerosis",
      "Secondary progressive multiple sclerosis"
    )
  ) %>%
  mutate(DISEASE_COURSE = factor(
    case_when(
#      MHDECOD == "Primary progressive multiple sclerosis" ~ "PPMS",
      MHDECOD == "Relapsing-remitting multiple sclerosis" ~ "RRMS",
      MHDECOD == "Secondary progressive multiple sclerosis" ~ "SPMS",
      TRUE ~ NA_character_
    ),
    # "healthy" reference
    levels = c("RRMS", "SPMS")
  )) %>%
  dplyr::select(USUBJID, DISEASE_COURSE)

### DISEASE DURATION ###
medical_history_data_disease_year <- supp_mh_data %>%
  dplyr::select(USUBJID, QNAM, QVAL) %>%
  filter(QNAM %in% c("STSTUDYR", "STSTUDMO", "YRSDIAG")) %>%
  group_by(USUBJID) %>%
  mutate(
    DISEASE_DURATION = case_when(
      ## year of diagnosis if QNAM is "STSTUDYR" and QVAL is not NA
      QNAM == "YRSDIAG" &
        !is.na(QVAL) ~ as.numeric(QVAL),
      QNAM == "STSTUDYR" &
        !is.na(QVAL) ~ abs(as.numeric(QVAL)),
      ## Absolute year number if QNAM is "STSTUDYR" and QVAL is not NA
      QNAM == "STSTUDMO" &
        !is.na(QVAL) ~ round(abs(as.numeric(QVAL) / 12), 1),
      ## Convert months to years if QNAM is "STSTUDMO" and QVAL is not NA
      TRUE ~ NA_real_
    )
  ) %>%
  # keep the higher value per each ID
  slice_max(order_by = DISEASE_DURATION) %>%
  # keep only uniques observations
  distinct() %>%
  dplyr::select(USUBJID, DISEASE_DURATION)
  

#match this data with the original data set of medical history data
medical_history_data_merged <- left_join(
  medical_history_data_filter, medical_history_data_disease_year, 
  by = "USUBJID" )

###########################
#QUESTIONNARIES (QS) DATA#
##########################
questionaries_data_filter_r_events <- questionaries_data %>%
  dplyr::filter(QSCAT == "EDSS" & is.na(QSDY) == F) %>%
  mutate(QSDY = ifelse(QSDY < 1, 1, QSDY)) %>%
  distinct() %>%
  group_by(USUBJID,QSDY) %>%
  mutate(EDSS = round(mean(as.numeric(QSORRES), na.rm = T), digits = 2)) %>%
  rename(TIME_EDSS = QSDY) %>%
  dplyr::select(USUBJID, TIME_EDSS, EDSS) %>%
  distinct()

###############################
## Supplemental Demographics ## 
# Extracting last day of following up #
# 1313 participants with end date of FUP
supp_dm_data_filter <- supp_dm_data %>%
  filter(QNAM == "RFENDY") %>%
  mutate(ENDFUP_TIME = ifelse(!is.na(as.numeric(QVAL)), as.numeric(QVAL), NA)) %>%
  filter(is.na(ENDFUP_TIME) == FALSE) %>%
  dplyr::select(USUBJID, ENDFUP_TIME) %>%
  distinct()

median(supp_dm_data_filter$ENDFUP_TIME) # 540 days
quantile(supp_dm_data_filter$ENDFUP_TIME) # 25% 337  , 75% 673  

########################
### Merging datasets####
########################

## DATAFRAMES PREMODELLING AND GRAPHS RELAPSES AND CDP_COUNT
#dataset with relevant variables
datasets <- list(
  supp_dm_data_filter,
  demographics_data_filter,
  medical_history_data_merged,
  questionaries_data_filter_r_events,
  clinical_event_data_filter
)

#Perform left joins iteratively using reduce
merged_data_r_events <- purrr::reduce(datasets,
                                      dplyr::left_join,
                                      by = c("USUBJID")) %>%
  distinct() %>%
  arrange(USUBJID, TIME_EDSS)

### Adding the CDP variable ###
#merged_data_r_events_cdp <- cdp_function(merged_data_r_events)

# #save marged file in import folder
# file_path_r_events <-
#   "~/dh_recurrent_events/recurrent_event_project/import/merged_data_r_events.csv"
# write.csv(merged_data_r_events,
#           file = file_path_r_events,
#           row.names = FALSE)
