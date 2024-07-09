# forest plot for overall estimate
forest_overall <- ggplot(final_results_df.gral, aes(x = Model, y = overall_estimate, ymin = conf_low, ymax = conf_high)) +
  geom_point() +
  geom_errorbar(width = 0.3) +
  facet_grid(paramcd ~ ., scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Model", y = "Overall Estimate*") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, by = .5), lim = c(0, 2)) +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# same but differentiated by type of model (count, time to first event or recurrent)
forest_overall_type <- ggplot(final_results_df.gral, aes(x = Model, y = overall_estimate, ymin = conf_low, ymax = conf_high, color = type_model)) +
  geom_point() +
  geom_errorbar(width = 0.3) + 
  facet_grid(paramcd ~ ., scales = "free_y") + 
  geom_hline(yintercept = 1, linetype = "dashed") +  
  labs(x = "Model", y = "Overall Estimate*", color = "Model Type") +  
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, by = .5), lim = c(0, 2)) +
  theme(
    axis.text.x = element_text(size = 15),  
    axis.text.y = element_text(size = 15),  
    axis.title = element_text(size = 15),   
    strip.text = element_text(size = 15),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom"  
      ) +
  scale_color_manual(values = c("Time to first event" = "blue",
                                "Count-based models" = "red",   
                                "Survival Cox extensions models" = "darkgreen"))

# forest plot for event specific models
forest_ev.sp <- ggplot(final_results_df.event.specific, aes(x = sevent_model, 
                                                            y = overall_estimate,
                                                            ymin = conf_low, 
                                                            ymax = conf_high)) +
  geom_point() +
  geom_errorbar(width = 0.1) + 
  geom_hline(yintercept = 1, linetype = "dashed") +  
  labs(x = "Model", y = "Event Estimate") +  
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 2, by = 0.25)) +
  facet_grid(paramcd ~ ., scales = "free_y") + 
  theme(
    axis.text.x = element_text(size = 15),  
    axis.text.y = element_text(size = 15),  
    axis.title = element_text(size = 15),   
    strip.text = element_text(size = 15)   
  )

# Save png files 
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/forestplot_overall.png",
       forest_overall,units = "mm", width = 300, height = 150 , dpi = 300)
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/forestplot_overall.type.png",
       forest_overall_type,units = "mm", width = 300, height = 150 , dpi = 300)
ggsave("~/dh_recurrent_events/recurrent_event_project/output/figure/forestplot_event.specific.png", 
       forest_ev.sp, units = "mm", width = 300, height = 150 , dpi = 300)
