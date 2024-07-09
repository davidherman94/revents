# ###################################
# ## MISSING VALUES ANALYSIS ########
# ###################################
# ##Frecuency of missing values
baseline_na <- merged_data_r_events %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, TIME_EDSS) %>%
  slice(1) %>%
  dplyr::select(USUBJID, AGE, SEX, EDSS, RACE, DISEASE_COURSE, DISEASE_DURATION) %>%
  distinct() %>%
  ungroup()

# Distribution of missing values per variable
skim_output <- skim(baseline_na)

# total number of missing values (no total observations)
skim_output$n_missing %>% sum()

## Looking for patterns in the NA values in those variables with > or close to 5% of NA values
visdat::vis_dat(baseline_na)


baseline_na <- baseline_na %>%
  rename(D.DURATION = DISEASE_DURATION,
         D.COURSE = DISEASE_COURSE)

aggr_plot_VIM <- aggr(baseline_na, plot = F)

plot_vim <- plot(aggr_plot_VIM,
     col = c('blue','red'),
     ylab = c("Histogram","Pattern"),
     numbers = T,
     sortVars = T,
     sortCombs = T,
     gap = 2,
     cex.axis = 0.8,
     cex.names = 0.8)

#### Missing values analyses table ###
baseline_na_table <- baseline_na[!complete.cases(baseline_na), ]

baseline_na_table <- baseline_na_table %>%
  dplyr::select(-USUBJID) %>%
  droplevels() %>%
  tbl_summary(
    by = D.COURSE,
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
      QSORRES = 1,
      D.DURATION = 1
    ),
    label = list(
      AGE = "Age (years)",
      SEX = "Sex",
      RACE = "Race",
      QSORRES = "EDSS overall",
      D.COURSE = "Disease course",
      D.DURATION = "Disease duration (years)"
    ),
    missing = "always",
    missing_text = "(Missing)",
    sort = list(everything() ~ "alphanumeric")
  ) %>%
  add_n() %>%
  modify_header(label = "**Covariates**") %>%
  modify_footnote(label = "Missing values are not shown") %>%
  modify_caption("**Baseline characteristics** (N = {N})") %>%
  bold_labels() %>%
  italicize_levels()
