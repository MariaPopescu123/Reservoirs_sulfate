# ============================================================
# FluoroProbe phytoplankton heatmaps (2016, 2024, 2025)
# Produces:
#   • 6 individual heatmaps (FCR/BVR × 2016/2024/2025)
#   • 1 paneled 2×3 figure with ONE legend (FCR top row, BVR bottom row; 2016/2024/2025 left->right)
# Saves to: Reservoir monitoring/Figs/Flora
#
# NOTE:
# - Uses YOUR original color method (no blue2green2red dependency)
# - Removes the error: blue2green2red() not found
# ============================================================

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
OUT_DIR  <- "Reservoir monitoring/Figs/heatmaps/Flora"
SITE_ID  <- 50
YEARS    <- c(2016, 2024, 2025)
RES      <- c("FCR", "BVR")

Z_COL    <- "TotalConc_ugL"
UNITZ    <- "ug/L"

# Bigger titles/axes/legend
TXT <- list(
  title = 18,
  axis_title = 16,
  axis_text = 13,
  legend_title = 16,
  legend_text = 14,
  legend_key_cm = 0.9
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1) Load and combine FluoroProbe data to ONE dataframe
# ============================================================

# 2025 (GitHub raw)
flora_2025 <- read.csv(
  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv"
)

# 2016 + 2024 (EDI 272)
flora_edi <- read.csv(
  "https://pasta.lternet.edu/package/data/eml/edi/272/9/f246b36c591a888cc70ebc87a5abbcb7"
)

prep_flora <- function(df) {
  df %>%
    mutate(
      DateTime = as.POSIXct(DateTime, tz = "UTC"),
      Date = as.Date(DateTime),
      Year = lubridate::year(DateTime),
      Reservoir = case_when(
        str_detect(as.character(Reservoir), "^\\s*FCR") ~ "FCR",
        str_detect(as.character(Reservoir), "^\\s*BVR") ~ "BVR",
        TRUE ~ str_trim(as.character(Reservoir))
      ),
      Site = if ("Site" %in% names(df)) as.numeric(Site) else NA_real_,
      Depth_m = as.numeric(Depth_m),
      TotalConc_ugL = suppressWarnings(as.numeric(TotalConc_ugL))
    ) %>%
    filter(!is.na(DateTime), !is.na(Reservoir), is.finite(Depth_m)) %>%
    # keep ALL original columns; just move the key ones to the front for convenience
    relocate(DateTime, Date, Year, Reservoir, Site, Depth_m, TotalConc_ugL)
}

flora_all <- bind_rows(prep_flora(flora_edi), prep_flora(flora_2025)) %>%
  filter(Year %in% YEARS, Site == SITE_ID) %>%
  arrange(Reservoir, DateTime, Depth_m)

write.csv(flora_all, "Reservoir monitoring/Data/flora_all.csv")

# One shared max legend across all years/reservoirs (comparable panels)
MAX_LEG <- max(flora_all[[Z_COL]], na.rm = TRUE)
# ============================================================
# 2) Heatmap function (YOUR original-ish color approach)
#    (no blue2green2red; uses gradientn colors + rescale values)
# ============================================================
flora_heatmap <- function(fp_data,
                          reservoir,
                          year,
                          site = SITE_ID,
                          z_col = Z_COL,
                          unitz = UNITZ,
                          max_legend_value = MAX_LEG,
                          depth_seq = seq(0.1, 10, by = 0.3),
                          xo_step = 0.1,
                          yo_step = 0.05) {
  
  fp <- fp_data %>%
    filter(Year == year, Reservoir == reservoir, Site == site) %>%
    transmute(
      DateTime,
      Depth_m = as.numeric(Depth_m),
      z = as.numeric(.data[[z_col]])
    ) %>%
    filter(!is.na(DateTime), is.finite(Depth_m), is.finite(z))
  
  if (nrow(fp) < 3) {
    return(
      ggplot() + theme_void() +
        labs(title = paste(reservoir, year)) +
        annotate("text", x = 0, y = 0, label = "Not enough data")
    )
  }
  
  # nearest value at each target depth per DateTime
  fp_depth <- fp %>%
    group_by(DateTime) %>%
    group_modify(~{
      df <- .x
      tibble(target_depth = depth_seq) %>%
        mutate(
          idx = map_int(target_depth, ~ which.min(abs(df$Depth_m - .x))),
          Depth_m = df$Depth_m[idx],
          z       = df$z[idx]
        )
    }) %>%
    ungroup() %>%
    mutate(DOY = yday(DateTime)) %>%
    filter(is.finite(DOY), is.finite(Depth_m), is.finite(z))
  
  # Interpolation (akima warning about collinearity is common; OK)
  interp_obj <- akima::interp(
    x = fp_depth$DOY,
    y = fp_depth$Depth_m,
    z = fp_depth$z,
    xo = seq(min(fp_depth$DOY), max(fp_depth$DOY), by = xo_step),
    yo = seq(min(fp_depth$Depth_m), max(fp_depth$Depth_m), by = yo_step),
    extrap = TRUE,
    linear = TRUE,
    duplicate = "strip"
  )
  
  interp_df <- akima::interp2xyz(interp_obj, data.frame = TRUE)
  names(interp_df) <- c("DOY", "Depth_m", "z")
  
  z_min <- min(interp_df$z, na.rm = TRUE)
  z_max <- max_legend_value
  
  month_seq <- seq.Date(
    as.Date(sprintf("%d-01-01", year)),
    as.Date(sprintf("%d-12-01", year)),
    by = "1 month"
  )
  
  # ---- YOUR color method style (define your own palette + values breakpoints) ----
  # You can change these however you want; this mirrors your earlier "values=rescale(...)" approach.
  flora_colors <- c(
    "blue3",   # low
    "cyan",
    "yellow",
    "orange",
    "red3",    # up to 100
    "black"    # ≥100
  )
  
  flora_values <- scales::rescale(
    c(z_min, 20, 40, 60, 100, z_max),
    from = c(z_min, z_max)
  )
  
  ggplot(interp_df, aes(x = DOY, y = Depth_m)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = yday(month_seq),
      labels = format(month_seq, "%b")
    ) +
    scale_fill_gradientn(
      colors = flora_colors,
      values = flora_values,
      limits = c(z_min, z_max),
      oob = squish,
      name = unitz
    ) +
    labs(
      x = "Day of year",
      y = "Depth (m)",
      title = paste(reservoir, year)
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
      legend.key.size = unit(TXT$legend_key_cm, "cm")
    )
}

# ============================================================
# 3) Build + save 6 individual + 1 paneled 2×3 (one legend)
# ============================================================
make_and_save_flora <- function(df_all,
                                out_dir = OUT_DIR,
                                years = YEARS,
                                reservoirs = RES,
                                width_single = 5,
                                height_single = 5,
                                dpi = 300,
                                panel_width = 16,
                                panel_height = 11) {
  
  combos <- expand_grid(Reservoir = reservoirs, Year = years)
  
  plots <- pmap(combos, function(Reservoir, Year) {
    flora_heatmap(
      fp_data = df_all,
      reservoir = Reservoir,
      year = Year,
      site = SITE_ID,
      z_col = Z_COL,
      unitz = UNITZ,
      max_legend_value = MAX_LEG
    )
  })
  
  names(plots) <- paste0(combos$Reservoir, "_", combos$Year)
  
  # Save individual plots
  walk2(names(plots), plots, ~{
    ggsave(
      filename = file.path(out_dir, paste0("Flora_", Z_COL, "_", .x, ".png")),
      plot = .y,
      width = width_single,
      height = height_single,
      dpi = dpi
    )
  })
  
  # Paneled: FCR top row; BVR bottom row; years left->right
  panel <- (
    (plots[[paste0("FCR_", years[1])]] | plots[[paste0("FCR_", years[2])]] | plots[[paste0("FCR_", years[3])]]) /
      (plots[[paste0("BVR_", years[1])]] | plots[[paste0("BVR_", years[2])]] | plots[[paste0("BVR_", years[3])]])
  ) +
    plot_layout(guides = "collect") +
    plot_annotation(theme = theme(legend.position = "right"))
  
  ggsave(
    filename = file.path(out_dir, paste0("PANELED_Flora_", Z_COL, "_2016_2024_2025.png")),
    plot = panel,
    width = panel_width,
    height = panel_height,
    dpi = dpi
  )
  
  list(plots = plots, panel = panel)
}

# ---- Run ----
res_flora <- make_and_save_flora(flora_all)

# View in RStudio:
# res_flora$panel

