#check to make sure these were all at site 50

# Sulfate plots + heatmaps (2016, 2024, 2025) — single dataframe version
# Maria Popescu
# Produces:
#   1) Time-series scatter (by depth, faceted by limnion) for FCR + BVR (site 50)
#   2) 6 heatmaps (FCR/BVR × 2016/2024/2025) + 1 paneled 2×3 figure (one legend)

# ---- Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(akima)
library(patchwork)
library(scales)

# ---- Config ----
OUT_DIR <- "Reservoir monitoring/Figs/heatmaps/sulfate"
SITE_ID <- 50
Z_COL   <- "SO4_ugL"
UNITZ   <- "ug/L"
MAX_LEG <- 2500
my_colors <- c("red", "orange", "yellow", "cyan", "blue")

# ---- Global text sizing (edit once here) ----
TXT <- list(
  title = 18,
  axis_title = 16,
  axis_text = 13,
  legend_title = 16,
  legend_text = 14,
  strip_text = 14,
  legend_key_cm = 0.9
)

#1) Load + standardize BOTH sources into the same schema####

# ---- A) 2024/2025 combined file ----
dtNEW_raw <- read.csv("Reservoir monitoring/sulfate_combined.csv")

prep_dtNEW <- function(df) {
  df %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Depth_m = as.numeric(Depth_m),
      SO4_ugL  = as.numeric(SO4_ugL),
      # keep Site if present; if not, create NA
      Site = if ("Site" %in% names(df)) as.numeric(Site) else NA_real_,
      Reservoir = case_when(
        str_detect(Reservoir, "^\\s*BVR") ~ "BVR",
        str_detect(Reservoir, "^\\s*FCR") ~ "FCR",
        TRUE ~ str_trim(Reservoir)
      )
    ) %>%
    filter(!is.na(Date), !is.na(Depth_m), !is.na(SO4_ugL)) %>%
    select(Date, Reservoir, Site, Depth_m, SO4_ugL) %>%  # <- canonical columns
    mutate(source = "combined_csv")
}

dtNEW_clean <- prep_dtNEW(dtNEW_raw)

# ---- B) 2016 EDI file ----
prep_2016 <- function() {
  dt1 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/607/0/86bc3dc8c1eafe36e6935f8a858a7b27")
  
  dt1 %>%
    mutate(
      Date     = as_date(DateTime),
      Reservoir = as.character(Reservoir),
      Site     = if ("Site" %in% names(dt1)) as.numeric(Site) else NA_real_,
      Depth_m  = as.numeric(Depth_m),
      SO4_ugL  = SO4_mmol_L * 96065
    ) %>%
    filter(!is.na(Date), !is.na(Depth_m), !is.na(SO4_ugL)) %>%
    select(Date, Reservoir, Site, Depth_m, SO4_ugL) %>%  # <- canonical columns
    mutate(source = "edi_2016")
}

dt2016_clean <- prep_2016()

# ---- C) Bind into ONE dataframe ----
dt_all <- bind_rows(dt2016_clean, dtNEW_clean) %>%
  mutate(
    Year = year(Date),
    Reservoir = case_when(
      str_detect(Reservoir, "^\\s*BVR") ~ "BVR",
      str_detect(Reservoir, "^\\s*FCR") ~ "FCR",
      TRUE ~ str_trim(Reservoir)
    )
  ) %>%
  filter(Year %in% c(2016, 2024, 2025)) %>%
  arrange(Reservoir, Date, Depth_m)


#Heatmap function (uses dt_all)####
make_heatmap <- function(df_all, reservoir, yr,
                         z_col = Z_COL, unitz = UNITZ,
                         max_legend_value = MAX_LEG,
                         depth_seq = seq(0.1, 11, by = 0.3),
                         xo_step = 0.5, yo_step = 0.5) {
  
  fp <- df_all %>%
    filter(Reservoir == reservoir, Year == yr) %>%
    transmute(
      Date = as.Date(Date),
      Depth_m = as.numeric(Depth_m),
      value = as.numeric(.data[[z_col]])
    ) %>%
    filter(!is.na(Date), is.finite(Depth_m), is.finite(value))
  
  if (nrow(fp) < 3) {
    return(ggplot() + theme_void() + labs(title = paste(reservoir, yr)) +
             annotate("text", x = 0, y = 0, label = "Not enough data"))
  }
  
  fp_depth <- fp %>%
    group_by(Date) %>%
    group_modify(~{
      df <- .x
      tibble(target_depth = depth_seq) %>%
        mutate(
          idx = map_int(target_depth, ~ which.min(abs(df$Depth_m - .x))),
          Depth_m = df$Depth_m[idx],
          value   = df$value[idx]
        )
    }) %>%
    ungroup() %>%
    mutate(DOY = yday(Date)) %>%
    filter(is.finite(DOY), is.finite(Depth_m), is.finite(value))
  
  interp_obj <- akima::interp(
    x = fp_depth$DOY,
    y = fp_depth$Depth_m,
    z = fp_depth$value,
    xo = seq(min(fp_depth$DOY), max(fp_depth$DOY), by = xo_step),
    yo = seq(min(fp_depth$Depth_m), max(fp_depth$Depth_m), by = yo_step),
    extrap = TRUE, linear = TRUE, duplicate = "strip"
  )
  
  interp_df <- akima::interp2xyz(interp_obj, data.frame = TRUE)
  names(interp_df) <- c("DOY", "Depth_m", "value")
  
  month_seq <- seq.Date(
    as.Date(sprintf("%d-01-01", yr)),
    as.Date(sprintf("%d-12-01", yr)),
    by = "1 month"
  )
  
  ggplot(interp_df, aes(DOY, Depth_m)) +
    geom_raster(aes(fill = value)) +
    geom_point(
      data = fp_depth, aes(DOY, Depth_m),
      inherit.aes = FALSE, shape = 17, color = "black",
      size = 1.6, alpha = 0.8
    ) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = yday(month_seq),
      labels = format(month_seq, "%b")
    ) +
    scale_fill_gradientn(
      colors = rev(my_colors),
      limits = c(0, max_legend_value),
      oob = squish,
      na.value = "gray80"
    ) +
    labs(
      x = "Day of year",
      y = "Depth (m)",
      title = paste(reservoir, yr),
      fill = unitz
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
      strip.text   = element_text(size = TXT$strip_text)
    )
}

#4) Build + save heatmaps (from ONE dataframe)####

make_and_save_heatmaps <- function(df_all,
                                   years = c(2016, 2024, 2025),
                                   reservoirs = c("FCR", "BVR"),
                                   out_dir = OUT_DIR,
                                   width = 5, height = 5, dpi = 500) {
  
  combos <- tidyr::expand_grid(
    Reservoir = reservoirs,
    Year = years
  )
  
  plots <- pmap(combos, function(Reservoir, Year) {
    make_heatmap(df_all, Reservoir, Year,
                 z_col = Z_COL, unitz = UNITZ, max_legend_value = MAX_LEG)
  })
  names(plots) <- paste0(combos$Reservoir, "_", combos$Year)
  
  # Save individual plots
  walk2(names(plots), plots, ~{
    ggsave(
      filename = file.path(out_dir, paste0(.x, "_", Z_COL, "_heatmap.png")),
      plot = .y, width = width, height = height, dpi = dpi
    )
  })
  
  # Paneled 2×3 with one legend (FCR top, BVR bottom; 2016/2024/2025 left->right)
  panel <- (
    (plots[[paste0("FCR_", years[1])]] | plots[[paste0("FCR_", years[2])]] | plots[[paste0("FCR_", years[3])]]) /
      (plots[[paste0("BVR_", years[1])]] | plots[[paste0("BVR_", years[2])]] | plots[[paste0("BVR_", years[3])]])
  ) +
    plot_layout(guides = "collect") +
    plot_annotation(
      theme = theme(
        legend.position = "right",
        plot.title   = element_text(size = TXT$title, face = "bold"),
        axis.title.x = element_text(size = TXT$axis_title),
        axis.title.y = element_text(size = TXT$axis_title),
        axis.text.x  = element_text(size = TXT$axis_text),
        axis.text.y  = element_text(size = TXT$axis_text),
        legend.title = element_text(size = TXT$legend_title),
        legend.text  = element_text(size = TXT$legend_text),
        legend.key.size = unit(TXT$legend_key_cm, "cm"),
        strip.text = element_text(size = TXT$strip_text)
      )
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("PANELED_", Z_COL, "_heatmaps_2016_2024_2025.png")),
    plot = panel, width = 16, height = 11, dpi = dpi
  )
  
  list(plots = plots, panel = panel)
}

# ---- Run: heatmaps ----
res_heat <- make_and_save_heatmaps(dt_all)

# View in RStudio:
# res_heat$panel

# ---- Optional: time-series examples from the ONE dataframe ----
# 2024 (site 50 if available)
p_fcr_2024 <- plot_sulfate_timeseries(dt_all, "FCR", 2024, free_y = FALSE)
p_bvr_2024 <- plot_sulfate_timeseries(dt_all, "BVR", 2024, free_y = FALSE)
# print(p_fcr_2024); print(p_bvr_2024)

write.csv(dt_all, "Reservoir monitoring/Data/sulfate.csv")
