## data prep includes packages, load and data profiling
#source("~/dh_recurrent_events/recurrent_event_project/derived.data_prep.R")

######################
###### RELAPSES ######
######################

##################################
## time to event relapses plot ###
##################################
filtered_data <- filter(
  df.model.relapses,
  USUBJID %in% c(
    "MSOAC/1422",
    "MSOAC/4481",
    "MSOAC/2082",
    "MSOAC/5539",
    "MSOAC/4781",
    "MSOAC/5527"
  )
)

last_event_0 <- filtered_data %>%
  group_by(USUBJID) %>%
  filter(last(EVENT) == 0) %>%
  slice_tail(n = 1)

survival_relapse_plot <- ggplot(filtered_data,
                                aes(x = TSTOP, y = USUBJID, group = USUBJID)) +
  geom_segment(aes(
    x = TSTART,
    xend = TSTOP,
    yend = USUBJID,
    color = EVENT
  ),
  linewidth = 0.3) +
  geom_point(
    data = filter(filtered_data, EVENT == 1),
    color = "black",
    size = 2
  ) +
  geom_point(
    data = last_event_0,
    color = "black",
    size = 2,
    shape = 21,
    fill = "white",
    stroke = 0.5
  ) +  # Puntos blancos con borde negro
  geom_line(aes(group = USUBJID), linewidth = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +  # Vertical line at day 0
  annotate(
    "text",
    x = 0,
    y = max(filtered_data$USUBJID),
    label = "Start of Study",
    color = "black",
    hjust = -0.1,
    vjust = -1,
    size = 2.5
  ) +  # Label "End of Study"
  geom_vline(xintercept = 1095,
             linetype = "dashed",
             color = "red") +  # Vertical line at day 1095
  annotate(
    "text",
    x = 1095,
    y = max(filtered_data$USUBJID),
    label = "End of Study",
    color = "black",
    hjust = 1.1,
    vjust = -1,
    size = 2.5
  ) +  # Label "End of Study"
  scale_x_continuous(
    breaks = seq(0, 1095, by = 365),
    limits = c(0, 1095),
    labels = seq(0, 1095, by = 365) / 365
  ) +
  # scale_color_manual(values = c("black", "white"), labels = NULL) +
  labs(x = "Time of observation (years)", y = "ID", color = "Status") +
  theme_minimal() +
  guides(color = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# MEAN CUMULATIVE FUNCTION #
mcf_plot.relapses <-
  plot(
    fit_mcf.relapse,
    mcf = TRUE,
    conf.int = FALSE,
    lty = 1:4,
    legendName = "Disease course"
  ) +
  ggplot2::xlab("Time since study entry (years)") +
  ggplot2::ylab("Mean cumulative function (n° relapses)") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = element_blank(),
                 axis.text = ggplot2::element_text(size = 15),
                 axis.title.x = ggplot2::element_text(size = 15),
                 axis.title.y = ggplot2::element_text(size = 15)) + 
  ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 830, by = 182.5),
    limits = c(0, 830),
    labels = seq(0, 830, by = 182.5) / 365
  )

# save png
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/mcf.relapses.png",
       mcf_plot.relapses, units = "mm", width = 300, height = 150 , dpi = 300)

# DISEASE COURSE COMPARATION WITH PLOTEVENTS FUNCTION
comparison_group <- plotEvents(Recur(
  TSTOP, USUBJID, EVENT) ~ DISEASE_COURSE,
  data = df.model.relapses)

labels <- c("RRMS", "SPMS")

comparison_group <- comparison_group +
  facet_grid(
    DISEASE_COURSE ~ .,
    scales = "free",
    space = "free",
    switch = "both",
    labeller = labeller(labels, label_parsed)
  ) + 
  xlab("Subject") +
  ylab("Time since start of the study (days)") +
  theme(
    axis.text = element_text(size = 10), 
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
    
  )


# save files of interest as png
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/mcf.relapses.png",
       mcf_plot.relapses, units = "mm", width = 300, height = 150 , dpi = 300)
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/mcf.relapses.comparison.png",
       comparison_group, units = "mm", width = 300, height = 150 , dpi = 300)


# ### Number of relpases bar chart ###
# relapse_counts_plot <- merged_data_r_events %>%
#   dplyr::select(USUBJID, MIDS) %>%
#   distinct(USUBJID, MIDS) %>%
#   #mutate(MIDS = ifelse(grepl("MS RELAPSE \\d", MIDS), MIDS, NA)) %>%
#   filter(!is.na(MIDS)) %>%
#   group_by(USUBJID) %>%
#   summarise(Relapses = max(n_distinct(MIDS))) %>%
#   count(Relapses) %>%
#   mutate(Percentage = round((n / sum(n) * 100), digits = 1))
# 
# number_relapses <-
#   ggplot(relapse_counts_plot, aes(x = factor(Relapses), y = Percentage)) +
#   geom_bar(stat = "identity",
#            fill = "skyblue",
#            width = 0.5) +
#   geom_text(aes(label = paste0("(n = ", n, ")")), vjust = -0.5, size = 3) +
#   labs(title = "Percentage and maximum number of relapses per patient",
#        x = "Number of relapses",
#        y = "% patients") +
#   theme_minimal() +
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
#   theme(panel.grid = element_blank())
