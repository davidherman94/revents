# relapses graphs
# source("~/dh_recurrent_events/recurrent_event_project/output/figure/relapses.R")

##################################
# DISABILITY PROGRESSION CHARTS ##
##################################

## JUST ONE PATIENT FOR EXAMPLE EXPLANATION ##
### CDP CHART EXAMPLE ######
cdp_chart_individual <- merged_data_r_events %>%
  filter(USUBJID %in% c("MSOAC/0070")) %>%
  dplyr::select(USUBJID, TIME_EDSS, EDSS) %>%
  filter(TIME_EDSS >= 1 & !is.na(TIME_EDSS),
         !is.na(EDSS))

cdp_chart_individual_plot <-  ggplot(cdp_chart_individual,
                                     aes(x = TIME_EDSS,
                                         y = EDSS,
                                         group = USUBJID,
                                         color = "blue")) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_segment(aes(x = 0, y = 2.5, xend = 260, yend = 2.5), linetype = "dashed", color = "black", size = 0.1) +  # Add dashed line
  geom_text(aes(x = 0, y = 2.5, label = "Ref. Baseline"), hjust = -0.2, vjust = 1.5, color = "black", size = 3) +  # Add label
  geom_text(aes(x = 138, y = 4.5, label = "CDP1"), hjust = 0, vjust = -0.5, color = "black", size = 3.5) +  # Label for 3rd observation
  geom_text(aes(x = 230, y = 4.0, label = "CDP2"), hjust = 0, vjust = -1.2, color = "black", size = 3.5) +
  #geom_segment(aes(x = 260, y = 3.98, xend = 600, yend = 3.98), linetype = "dashed", color = "black", size = 0.1) +  # Add dashed line
  #geom_text(aes(x = 257, y = 4.0, label = "REF. 2"), hjust = -0.5, vjust = 1.5, color = "black", size = 3) +  # Add label
  #geom_text(aes(x = 336, y = 4.0, label = "C2"), hjust = 0, vjust = 2, color = "black", size = 3) +  # Label for 6th observation
  geom_text(aes(x = 390, y = 5.0, label = "CDP3"), hjust = 0, vjust = -0.5, color = "black", size = 3.5) +  # Label for 7th observation
  labs(title = "Disease progression assessed by EDSS in a study participant",
       x = "Time (days)",
       y = "EDDS score") +
  # scale_x_continuous(breaks = seq(0, 600, by = 90), limits = c(0, 600)) +  # Saltos de 90 días en el eje x
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +
  theme_minimal()

##### MEAN CUMULATIVE FUNCTION ##
mcf_plot.cdp <- plot(fit_mcf.cdp, mcf = TRUE, conf.int = FALSE, lty = 1:4,
                     legendName = "Disease course") +
  ggplot2::xlab("Time since study entry (years)") +
  ggplot2::ylab("Mean cumulative function (n° of CDP)") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = element_blank(),
               axis.text = ggplot2::element_text(size = 20),
               axis.title.x = ggplot2::element_text(size = 20),
               axis.title.y = ggplot2::element_text(size = 20),
               legend.position = "bottom", 
               legend.title = ggplot2::element_text(size = 20), 
               legend.text = ggplot2::element_text(size = 20)) +
  ggplot2::theme(panel.grid = element_blank()) + 
  ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 830, by = 182.5), 
                              limits = c(0, 830),
                              labels = seq(0, 830, by = 182.5) / 365) 


# save png
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/cdp_derived.png", 
       cdp_chart_individual_plot, units = "mm", width = 300, height = 150 , dpi = 300)

ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/mcf.dp.png", 
       mcf_plot.cdp, units = "mm", width = 300, height = 150 , dpi = 300)

################################
#### time to event CDP plot ####
################################
filtered_data_cdp <- filter(
  df.model.cdp,
  USUBJID %in% c("MSOAC/1422", "MSOAC/4481", "MSOAC/2082", "MSOAC/5539", "MSOAC/4781",
                 "MSOAC/5527")
)

last_event_0_cdp <- filtered_data_cdp %>%
  group_by(USUBJID) %>%
  filter(last(EVENT) == 0) %>%
  slice_tail(n = 1)

survival_cdp_plot <- ggplot(filtered_data_cdp,
                                aes(x = TSTOP, y = USUBJID, group = USUBJID)) +
  geom_segment(aes(
    x = TSTART,
    xend = TSTOP,
    yend = USUBJID,
    color = EVENT
  ),
  linewidth = 0.3) +
  geom_point(
    data = filter(filtered_data_cdp, EVENT == 1),
    color = "black",
    size = 2
  ) +
  geom_point(data = last_event_0_cdp,
             color = "black",
             size = 2, shape = 21, 
             fill = "white", 
             stroke = 0.5) +  # Puntos blancos con borde negro
  geom_line(aes(group = USUBJID), linewidth = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +  # Vertical line at day 0
  annotate(
    "text",
    x = 0,
    y = max(filtered_data_cdp$USUBJID),
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
    y = max(filtered_data_cdp$USUBJID),
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


# ## OVERALL and IQR by EDSS all patients all observations###
# median_iqr <- spaghetti_chart %>%
#   group_by(QSDY) %>%
#   summarise(
#     median_edss = median(QSORRES, na.rm = TRUE),
#     # Calculate median
#     lower_iqr = quantile(QSORRES, 0.025, na.rm = TRUE),
#     # Calculate lower and upper IQC
#     upper_iqr = quantile(QSORRES, 0.975, na.rm = TRUE)
#   )
# 
# spaghetti_plot_median_iqr <- ggplot(data = median_iqr) +
#   geom_smooth(aes(x = QSDY, y = median_edss),
#               color = "black",
#               linetype = "solid") +
#   geom_ribbon(
#     aes(x = QSDY, ymin = lower_iqr, ymax = upper_iqr),
#     fill = "blue",
#     alpha = 0.2
#   ) +
#   labs(x = "Time of measuring since study starts (days)", y = "EDDS") +
#   scale_x_continuous(breaks = seq(0, 1400, by = 200), limits = c(0, 1420)) +
#   scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
#   theme(legend.position = "none") +
#   theme_minimal()


# ALL PATIENTS ##
### Spaghetti plot EDSS by ID patient ####
# all observations including repetitions of measurements
# spaghetti_chart <- merged_data_r_events %>%
#   dplyr::select(USUBJID, QSDY, QSORRES) %>%
#   distinct(USUBJID,QSDY, QSORRES) %>%
#   filter(QSDY >= 1 & !is.na(QSDY),
#          !is.na(QSORRES))
# 
# spaghetti_plot_all_cases <- ggplot(spaghetti_chart,
#                                    aes(
#                                      x = QSDY,
#                                      y = QSORRES,
#                                      group = USUBJID,
#                                      color = factor(USUBJID)
#                                    )) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Disease progression assesed by EDDS (n= 1622)",
#        x = "Time of measuring since study starts (days)",
#        y = "EDDS") +
#   scale_x_continuous(breaks = seq(0, 1400, by = 200), limits = c(0, 1420)) +
#   scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "none")

## RANDOM SELECTION OF PATIENTS ##
### With 15 randomly selected unics ID to see a smaller example ###
# set.seed(333)
# 
# random_USUBJID <- merged_data_r_events %>%
#   distinct(USUBJID) %>%
#   sample_n(20) %>%
#   pull(USUBJID)
# 
# spaghetti_chart_random <- merged_data_r_events %>%
#   filter(USUBJID %in% random_USUBJID) %>%
#   dplyr::select(USUBJID, QSDY, QSORRES) %>%
#   filter(QSDY >= 1 & !is.na(QSDY),
#          !is.na(QSORRES)) %>%
#   group_by(QSDY_month = ceiling(QSDY / 30.4))


# keeps at the end 9 uniques ID as 5 didn't fullfil the conditions of filtering
# spaghetti_plot_random_selection <- ggplot(spaghetti_chart_random,
#                                           aes(
#                                             x = QSDY_month,
#                                             y = QSORRES,
#                                             group = USUBJID,
#                                             color = factor(USUBJID)
#                                           )) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Disease progression assesed by EDDS (n= 5)",
#        x = "Time of measuring since study starts (months)",
#        y = "EDDS") +
#   scale_x_continuous(breaks = seq(0, 34, by = 1), limits = c(0, 34)) +
#   scale_y_continuous(breaks = seq(0, 8, by = .5), limits = c(0, 8)) +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "none")