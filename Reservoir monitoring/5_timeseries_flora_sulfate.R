# ============================================================
# DCM multi-var time series + Sulfate (AVG + MAX) time series (Site 50)
# NO joining of dates (keeps "real" sample dates for each dataset)
# X-axis shows ONLY May–Oct for EACH year panel (2016 / 2024 / 2025)
# Outputs (all are 6-panel: FCR top, BVR bottom; 2016, 2024, 2025):
#   A) PHYTO @ daily DCM (multi-variable; ONE point/day/var)
#   B) SO4 daily AVG (red)
#   C) SO4 daily MAX (red)
#   D) COMBINED normalized (PHYTO multi-var + SO4 AVG) — 0–1
#   E) COMBINED normalized (PHYTO multi-var + SO4 MAX) — 0–1
# ============================================================

# ---- Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(scales)

# ---- Config ----
SITE_ID <- 50
YEARS   <- c(2016, 2024, 2025)

FLORA_VARS <- c(
  "TotalConc_ugL",
  "GreenAlgae_ugL",
  "Bluegreens_ugL",
  "BrownAlgae_ugL",
  "MixedAlgae_ugL"
)

OUT_DIR <- "Reservoir monitoring/Figs/time series"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Read ----
flora_all <- read.csv("Reservoir monitoring/Data/flora_all.csv")
sulfate   <- read.csv("Reservoir monitoring/Data/sulfate.csv")

# ---- Helpers ----
std_reservoir <- function(x) {
  x <- as.character(x)
  case_when(
    str_detect(x, "^\\s*FCR") ~ "FCR",
    str_detect(x, "^\\s*BVR") ~ "BVR",
    TRUE ~ str_trim(x)
  )
}

get_date_any <- function(df) {
  if ("Date" %in% names(df)) {
    as.Date(df$Date)
  } else if ("DateTime" %in% names(df)) {
    as.Date(as.POSIXct(df$DateTime, tz = "UTC"))
  } else {
    stop("No Date or DateTime column found in sulfate dataframe.")
  }
}

# ============================================================
# Facet x-limits per year: ONLY May–Oct for that year
# ============================================================
x_limits_df <- tidyr::expand_grid(
  Reservoir = c("FCR", "BVR"),
  Year = YEARS
) %>%
  mutate(
    x_start = as.Date(paste0(Year, "-05-01")),
    x_end   = as.Date(paste0(Year, "-10-31"))
  )

# IMPORTANT FIX: inherit.aes = FALSE so it doesn't look for `variable`
facet_window_blanks <- function(lims_df) {
  list(
    geom_blank(data = lims_df, inherit.aes = FALSE, aes(x = x_start, y = 0)),
    geom_blank(data = lims_df, inherit.aes = FALSE, aes(x = x_end,   y = 0))
  )
}

# ============================================================
# 1) Clean inputs (keep ALL data; don't filter months here)
# ============================================================
flora_all <- flora_all %>%
  mutate(
    Reservoir = std_reservoir(Reservoir),
    DateTime  = as.POSIXct(DateTime, tz = "UTC"),
    Date      = as.Date(DateTime),
    Year      = year(Date),
    Site      = as.numeric(Site),
    Depth_m   = as.numeric(Depth_m)
  ) %>%
  filter(
    Site == SITE_ID,
    Reservoir %in% c("FCR", "BVR"),
    Year %in% YEARS
  )

sulfate <- sulfate %>%
  mutate(
    Reservoir = std_reservoir(Reservoir),
    Date      = get_date_any(sulfate),
    Year      = year(Date),
    Site      = if ("Site" %in% names(sulfate)) as.numeric(Site) else NA_real_,
    Depth_m   = as.numeric(Depth_m),
    SO4_ugL   = as.numeric(SO4_ugL)
  ) %>%
  filter(
    Site == SITE_ID,
    Reservoir %in% c("FCR", "BVR"),
    Year %in% YEARS,
    is.finite(Depth_m),
    is.finite(SO4_ugL)
  )

# ============================================================
# 2) DAILY DCM definition:
#    For each (Reservoir, Year, Date) pick the single row (cast+depth)
#    with the MAX TotalConc_ugL across all depths/casts that day.
#    Then extract each variable at that exact cast, nearest to DCM depth.
# ============================================================
dcm_daily_pick <- flora_all %>%
  filter(is.finite(TotalConc_ugL)) %>%
  group_by(Reservoir, Year, Date) %>%
  slice_max(order_by = TotalConc_ugL, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    Reservoir, Year, Date,
    DCM_DateTime = DateTime,
    DCM_depth    = Depth_m
  )

get_var_at_daily_dcm <- function(var_name) {
  flora_all %>%
    filter(is.finite(.data[[var_name]])) %>%
    inner_join(dcm_daily_pick, by = c("Reservoir", "Year", "Date")) %>%
    filter(DateTime == DCM_DateTime) %>%
    group_by(Reservoir, Year, Date) %>%
    slice(which.min(abs(Depth_m - DCM_depth))) %>%
    ungroup() %>%
    transmute(
      Reservoir, Year, Date,
      variable  = var_name,
      DCM_value = as.numeric(.data[[var_name]])
    )
}

flora_dcm_all <- purrr::map_dfr(FLORA_VARS, get_var_at_daily_dcm) %>%
  filter(is.finite(DCM_value)) %>%
  mutate(variable = factor(variable, levels = FLORA_VARS)) %>%
  left_join(x_limits_df, by = c("Reservoir", "Year")) %>%
  filter(Date >= x_start, Date <= x_end)

# ============================================================
# 3) Sulfate daily AVG + MAX throughout the water column (per day)
# ============================================================
sulfate_daily <- sulfate %>%
  group_by(Reservoir, Year, Date) %>%
  summarise(
    SO4_avg_ugL = mean(SO4_ugL, na.rm = TRUE),
    SO4_max_ugL = max(SO4_ugL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(x_limits_df, by = c("Reservoir", "Year")) %>%
  filter(Date >= x_start, Date <= x_end)

# ============================================================
# 4) Plot styling
# ============================================================
phyto_cols <- c(
  "TotalConc_ugL"     = "black",
  "GreenAlgae_ugL"    = "green",
  "Bluegreens_ugL"    = "blue",
  "BrownAlgae_ugL"    = "brown",
  "MixedAlgae_ugL"    = "purple"
)

base_theme_big <- theme_bw() +
  theme(
    plot.title  = element_text(size = 20, face = "bold"),
    axis.title  = element_text(size = 16),
    axis.text   = element_text(size = 13),
    strip.text  = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title= element_text(size = 13)
  )

x_scale_may_oct <- scale_x_date(
  date_breaks = "1 month",
  date_labels = "%b",
  expand = c(0.01, 0.01)
)

# ============================================================
# A) 6-panel PHYTO @ DAILY DCM (multi-variable; ONE point/day/var)
# ============================================================
p_phyto <- ggplot(flora_dcm_all, aes(Date, DCM_value, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  facet_grid(Reservoir ~ Year, scales = "free_x") +
  scale_color_manual(values = phyto_cols, name = NULL) +
  x_scale_may_oct +
  facet_window_blanks(x_limits_df) +
  labs(
    title = "Phytoplankton @ Daily DCM (Site 50) — May–Oct only",
    x = "Month",
    y = "Concentration at DCM (µg/L)"
  ) +
  base_theme_big +
  theme(legend.position = "top")

ggsave(file.path(OUT_DIR, "PANELED_DCM_multiline_SITE50_MayOct.png"),
       p_phyto, width = 18, height = 8, dpi = 400)

# ============================================================
# B) 6-panel SO4 daily AVG (red)
# ============================================================
p_so4_avg <- ggplot(sulfate_daily, aes(Date, SO4_avg_ugL)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 1.6) +
  facet_grid(Reservoir ~ Year, scales = "free_x") +
  x_scale_may_oct +
  facet_window_blanks(x_limits_df) +
  coord_cartesian(ylim = c(0, 2750)) +
  labs(
    title = "Sulfate (Daily AVG, Site 50) — May–Oct only",
    x = "Month",
    y = "SO₄ avg (µg/L)"
  ) +
  base_theme_big +
  theme(legend.position = "none")

ggsave(file.path(OUT_DIR, "PANELED_SO4_DAILY_AVG_SITE50_MayOct.png"),
       p_so4_avg, width = 18, height = 8, dpi = 400)

# ============================================================
# C) 6-panel SO4 daily MAX (red)
# ============================================================
p_so4_max <- ggplot(sulfate_daily, aes(Date, SO4_max_ugL)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 1.6) +
  facet_grid(Reservoir ~ Year, scales = "free_x") +
  x_scale_may_oct +
  facet_window_blanks(x_limits_df) +
  coord_cartesian(ylim = c(0, 2750)) +
  labs(
    title = "Sulfate (Daily MAX, Site 50) — May–Oct only",
    x = "Month",
    y = "SO₄ max (µg/L)"
  ) +
  base_theme_big +
  theme(legend.position = "none")

ggsave(file.path(OUT_DIR, "PANELED_SO4_DAILY_MAX_SITE50_MayOct.png"),
       p_so4_max, width = 18, height = 8, dpi = 400)

# ============================================================
# D) Combined overlay (normalized 0–1): PHYTO + SO4 AVG (red)
# ============================================================
phyto_norm <- flora_dcm_all %>%
  group_by(Reservoir, Year, variable) %>%
  mutate(phyto_n = rescale(DCM_value)) %>%
  ungroup()

so4_avg_norm <- sulfate_daily %>%
  group_by(Reservoir, Year) %>%
  mutate(so4_n = rescale(SO4_avg_ugL)) %>%
  ungroup()

p_combined_avg <- ggplot() +
  geom_line(
    data = phyto_norm,
    aes(Date, phyto_n, color = variable, group = variable),
    linewidth = 1
  ) +
  geom_point(
    data = phyto_norm,
    aes(Date, phyto_n, color = variable),
    size = 1.3
  ) +
  geom_line(
    data = so4_avg_norm,
    aes(Date, so4_n, group = 1),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    data = so4_avg_norm,
    aes(Date, so4_n),
    color = "red",
    size = 1.3
  ) +
  facet_grid(Reservoir ~ Year, scales = "free_x") +
  scale_color_manual(values = phyto_cols, name = NULL) +
  x_scale_may_oct +
  facet_window_blanks(x_limits_df) +
  labs(
    title = "Phyto @ Daily DCM + SO₄ Daily AVG (red) — normalized — May–Oct only (Site 50)",
    x = "Month",
    y = "Normalized (0–1)"
  ) +
  base_theme_big +
  theme(legend.position = "top")

ggsave(file.path(OUT_DIR, "PANELED_DCM_multiline_PLUS_SO4_DAILY_AVG_NORMALIZED_SITE50_MayOct.png"),
       p_combined_avg, width = 18, height = 8, dpi = 400)

# ============================================================
# E) Combined overlay (normalized 0–1): PHYTO + SO4 MAX (red)
# ============================================================
so4_max_norm <- sulfate_daily %>%
  group_by(Reservoir, Year) %>%
  mutate(so4_n = rescale(SO4_max_ugL)) %>%
  ungroup()

p_combined_max <- ggplot() +
  geom_line(
    data = phyto_norm,
    aes(Date, phyto_n, color = variable, group = variable),
    linewidth = 1
  ) +
  geom_point(
    data = phyto_norm,
    aes(Date, phyto_n, color = variable),
    size = 1.3
  ) +
  geom_line(
    data = so4_max_norm,
    aes(Date, so4_n, group = 1),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    data = so4_max_norm,
    aes(Date, so4_n),
    color = "red",
    size = 1.3
  ) +
  facet_grid(Reservoir ~ Year, scales = "free_x") +
  scale_color_manual(values = phyto_cols, name = NULL) +
  x_scale_may_oct +
  facet_window_blanks(x_limits_df) +
  labs(
    title = "Phyto @ Daily DCM + SO₄ Daily MAX (red) — normalized — May–Oct only (Site 50)",
    x = "Month",
    y = "Normalized (0–1)"
  ) +
  base_theme_big +
  theme(legend.position = "top")

ggsave(file.path(OUT_DIR, "PANELED_DCM_multiline_PLUS_SO4_DAILY_MAX_NORMALIZED_SITE50_MayOct.png"),
       p_combined_max, width = 18, height = 8, dpi = 400)

