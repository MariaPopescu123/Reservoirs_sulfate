
# ---- Time-series plot function ----
plot_sulfate_timeseries <- function(df_year, reservoir, site_id = SITE_ID,
                                    fixed_y = c(0, 2750), free_y = FALSE) {
  dfp <- df_year %>%
    filter(Reservoir == reservoir, Site == site_id) %>%
    add_limnion()
  
  p <- ggplot(dfp, aes(Date, SO4_ugL, color = Depth_f)) +
    geom_point() +
    facet_grid(rows = vars(limnion), scales = if (free_y) "free_y" else "fixed") +
    scale_x_date(date_labels = "%b") +
    labs(
      x = "Month",
      y = UNITZ,
      title = paste(reservoir, "Sulfate (Site", site_id, "-", unique(year(dfp$Date)), ")"),
      color = "Depth (m)"
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text(size = TXT$title, face = "bold"),
      axis.title.x = element_text(size = TXT$axis_title),
      axis.title.y = element_text(size = TXT$axis_title),
      axis.text.x  = element_text(size = TXT$axis_text),
      axis.text.y  = element_text(size = TXT$axis_text),
      legend.title = element_text(size = TXT$legend_title),
      legend.text  = element_text(size = TXT$legend_text),
      legend.key.size = unit(TXT$legend_key_cm, "cm"),
      strip.text   = element_text(size = TXT$strip_text),
      strip.text.y = element_text(size = TXT$strip_text),
      panel.spacing = unit(0.8, "lines")
    )
  
  if (!free_y) p <- p + coord_cartesian(ylim = fixed_y)
  p
}

# Example: 2024 time-series (same y-axis across facets)
dt_2024 <- dtNEW_clean %>% filter(year(Date) == 2024)
p_fcr_2024 <- plot_sulfate_timeseries(dt_2024, "FCR", free_y = FALSE)
p_bvr_2024 <- plot_sulfate_timeseries(dt_2024, "BVR", free_y = FALSE)
# print(p_fcr_2024); print(p_bvr_2024)

