# ============================================================
# DO + ORP heatmaps (2016, 2024, 2025) — single dataframe + flags kept
# Maria Popescu
# Produces:
#   1) 6 DO heatmaps + 1 paneled 2×3 (FCR top, BVR bottom; 2016/2024/2025 left->right)
#   2) 6 ORP heatmaps + 1 paneled 2×3 (same layout)
# Notes:
#   - Uses ONE combined CTD dataframe across years (EDI + GitHub raw CTD)
#   - Robust EDI download w/ cache + retries
#   - Keeps Flag_DO_mgL and Flag_ORP_mV (if present) and lets you filter them
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
OUT_DIR_DO  <- "Reservoir monitoring/Figs/heatmaps/DO"
OUT_DIR_ORP <- "Reservoir monitoring/Figs/heatmaps/ORP"
CACHE_DIR   <- "Reservoir monitoring/Data/EDI_cache"

SITE_ID <- 50
YEARS <- c(2016, 2024, 2025)
RESERVOIRS <- c("FCR", "BVR")

# ---- Flag filtering (edit as needed) ----
# Set to TRUE if you want to KEEP ONLY good/unflagged data.
# If FALSE, heatmaps use all data but flags remain in the dataframe.
FILTER_GOOD_FLAGS <- FALSE

# Common patterns (adjust to your lab convention):
# - EDI often uses 0/1 where 0 = good, 1 = suspect/bad
# - Sometimes flags are "A"/"B"/"C" or "OK"/"BAD"
is_good_flag <- function(x) {
  # Treat NA as good unless you want stricter behavior
  if (is.null(x)) return(TRUE)
  if (is.numeric(x)) return(is.na(x) | x == 0)
  x <- toupper(trimws(as.character(x)))
  is.na(x) | x %in% c("0", "OK", "GOOD", "A", "ACCEPT", "ACCEPTED")
}

# ---- Global text sizing ----
TXT <- list(
  title = 18,
  axis_title = 16,
  axis_text = 13,
  legend_title = 16,
  legend_text = 14,
  strip_text = 14,
  legend_key_cm = 0.9
)

# ---- Color scales ----
DO_colors  <- c("red4","red","yellow","cyan","blue","blue3")
ORP_colors <- c("red4","red","yellow","cyan","blue","blue3")

DO_MAX  <- 15
ORP_MIN <- -250
ORP_MAX <- 350

# ============================================================
# 0) Robust download helper (cache + retries)
# ============================================================
download_with_retry <- function(url, dest, timeout_sec = 300, retries = 4, quiet = TRUE) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  options(timeout = max(timeout_sec, getOption("timeout", 60)))
  
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("Using cached file: ", dest)
    return(dest)
  }
  
  for (i in seq_len(retries)) {
    message("Downloading (attempt ", i, "/", retries, "): ", url)
    
    ok <- tryCatch({
      suppressWarnings(utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet))
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) FALSE)
    
    if (isTRUE(ok)) {
      message("Saved to: ", dest)
      return(dest)
    }
    
    Sys.sleep(2 * i)
  }
  
  stop(
    "Failed to download after ", retries, " attempts: ", url,
    "\nTry again later, switch networks, or download manually and place it here:\n",
    dest
  )
}

# ============================================================
# 1) Load + unify CTD data to ONE dataframe (KEEP FLAGS)
# ============================================================

prep_ctd <- function(df, date_col = "DateTime") {
  df %>%
    mutate(
      Date = as_date(.data[[date_col]]),
      Reservoir = case_when(
        str_detect(as.character(Reservoir), "^\\s*BVR") ~ "BVR",
        str_detect(as.character(Reservoir), "^\\s*FCR") ~ "FCR",
        TRUE ~ str_trim(as.character(Reservoir))
      ),
      Site = if ("Site" %in% names(df)) as.numeric(Site) else NA_real_,
      Depth_m = as.numeric(Depth_m),
      
      DO_mgL  = if ("DO_mgL" %in% names(df)) as.numeric(DO_mgL) else NA_real_,
      ORP_mV  = if ("ORP_mV" %in% names(df)) as.numeric(ORP_mV) else NA_real_,
      
      # KEEP FLAGS if present; otherwise create NA columns so schema matches
      Flag_DO_mgL  = if ("Flag_DO_mgL" %in% names(df)) df$Flag_DO_mgL else NA,
      Flag_ORP_mV  = if ("Flag_ORP_mV" %in% names(df)) df$Flag_ORP_mV else NA
    ) %>%
    select(Date, Reservoir, Site, Depth_m, DO_mgL, Flag_DO_mgL, ORP_mV, Flag_ORP_mV) %>%
    filter(!is.na(Date), !is.na(Reservoir), is.finite(Depth_m))
}

# ---- 2016+2024 EDI file (cache locally first) ----
edi_url  <- "https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1"
edi_path <- file.path("Reservoir monitoring", "Data", "EDI_cache", "ctd_L1_edi200_15.csv")

edi_file <- download_with_retry(edi_url, edi_path, timeout_sec = 300, retries = 4)

ctd_edi <- read.csv(edi_file)
ctd_edi_clean <- prep_ctd(ctd_edi, date_col = "DateTime") %>%
  mutate(source = "EDI")

# ---- 2025 GitHub raw CTD ----
ctd_gh <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv")
ctd_gh_clean <- prep_ctd(ctd_gh, date_col = "DateTime") %>%
  mutate(source = "GitHub_raw")

# ---- One combined dataframe ----
ctd_all <- bind_rows(ctd_edi_clean, ctd_gh_clean) %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% YEARS) %>%
  arrange(Reservoir, Date, Depth_m)

# Optional: restrict to Site 50 (keeps everything consistent)
ctd_all_site50 <- ctd_all %>% filter(is.na(Site) | Site == SITE_ID)

# Optional: filter out flagged values (if you want)
# NOTE: this keeps rows where the relevant flag is "good" per is_good_flag()
apply_flag_filter <- function(df, value_col) {
  if (!FILTER_GOOD_FLAGS) return(df)
  
  if (value_col == "DO_mgL") {
    df %>% filter(is_good_flag(Flag_DO_mgL))
  } else if (value_col == "ORP_mV") {
    df %>% filter(is_good_flag(Flag_ORP_mV))
  } else {
    df
  }
}

# ============================================================
# 2) Generic heatmap builder (works for DO or ORP)
# ============================================================

make_interp_heatmap <- function(df_all,
                                reservoir,
                                yr,
                                value_col,
                                legend_name,
                                colors,
                                limits,
                                values = NULL,
                                xo_step = 0.2,
                                yo_step = 0.2,
                                depth_seq = seq(0.1, 11, by = 0.3)) {
  
  fp <- df_all %>%
    filter(Reservoir == reservoir, Year == yr) %>%
    transmute(
      Date = as.Date(Date),
      Depth_m = as.numeric(Depth_m),
      value = as.numeric(.data[[value_col]])
    ) %>%
    filter(!is.na(Date), is.finite(Depth_m), is.finite(value))
  
  if (nrow(fp) < 3) {
    return(
      ggplot() + theme_void() +
        labs(title = paste(reservoir, yr)) +
        annotate("text", x = 0, y = 0, label = "Not enough data")
    )
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
    extrap = TRUE,
    linear = TRUE,
    duplicate = "strip"
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
      data = fp_depth,
      aes(DOY, Depth_m),
      inherit.aes = FALSE,
      shape = 17, color = "black", size = 0.6, alpha = 0.8
    ) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = yday(month_seq),
      labels = format(month_seq, "%b")
    ) +
    scale_fill_gradientn(
      colors = colors,
      limits = limits,
      values = values,
      oob = squish,
      na.value = "grey80",
      name = legend_name
    ) +
    labs(
      x = "Day of year",
      y = "Depth (m)",
      title = paste(reservoir, yr)
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

# ============================================================
# 3) Make + save individual + paneled figures
# ============================================================

make_and_save_panels <- function(df_all,
                                 value_col,
                                 legend_name,
                                 out_dir,
                                 years = YEARS,
                                 reservoirs = RESERVOIRS,
                                 colors,
                                 limits,
                                 values = NULL,
                                 file_prefix,
                                 width_single = 5, height_single = 5,
                                 width_panel = 16, height_panel = 11,
                                 dpi = 500) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  combos <- expand_grid(Reservoir = reservoirs, Year = years)
  
  plots <- pmap(combos, function(Reservoir, Year) {
    make_interp_heatmap(
      df_all = df_all,
      reservoir = Reservoir,
      yr = Year,
      value_col = value_col,
      legend_name = legend_name,
      colors = colors,
      limits = limits,
      values = values
    )
  })
  names(plots) <- paste0(combos$Reservoir, "_", combos$Year)
  
  walk2(names(plots), plots, ~{
    ggsave(
      filename = file.path(out_dir, paste0(file_prefix, "_", .x, ".png")),
      plot = .y,
      width = width_single,
      height = height_single,
      dpi = dpi
    )
  })
  
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
        legend.key.size = unit(TXT$legend_key_cm, "cm")
      )
    )
  
  ggsave(
    filename = file.path(out_dir, paste0(file_prefix, "_PANELED_2016_2024_2025.png")),
    plot = panel,
    width = width_panel,
    height = height_panel,
    dpi = dpi
  )
  
  list(plots = plots, panel = panel)
}

# ============================================================
# 4) DO: run + save (FLAGS KEPT; optional filtering supported)
# ============================================================

DO_values <- rescale(c(0, 1, 2, 4, 6, 10, DO_MAX), from = c(0, DO_MAX))

df_DO <- ctd_all_site50 %>%
  filter(!is.na(DO_mgL)) %>%
  apply_flag_filter("DO_mgL")

res_DO <- make_and_save_panels(
  df_all = df_DO,
  value_col = "DO_mgL",
  legend_name = "DO (mg/L)",
  out_dir = OUT_DIR_DO,
  colors = DO_colors,
  limits = c(0, DO_MAX),
  values = DO_values,
  file_prefix = "DO_heatmap"
)

# View:
# res_DO$panel

# ============================================================
# 5) ORP: run + save (FLAGS KEPT; optional filtering supported)
# ============================================================

ORP_values <- rescale(
  c(ORP_MIN, -250, -100, 0, 100, ORP_MAX),
  from = c(ORP_MIN, ORP_MAX)
)

df_ORP <- ctd_all_site50 %>%
  filter(!is.na(ORP_mV)) %>%
  apply_flag_filter("ORP_mV")

res_ORP <- make_and_save_panels(
  df_all = df_ORP,
  value_col = "ORP_mV",
  legend_name = "ORP (mV)",
  out_dir = OUT_DIR_ORP,
  colors = ORP_colors,
  limits = c(ORP_MIN, ORP_MAX),
  values = ORP_values,
  file_prefix = "ORP_heatmap"
)

# View:
# res_ORP$panel

# ============================================================
# 6) (Optional) Verify flags exist + see counts
# ============================================================
# table(ctd_all_site50$Flag_DO_mgL, useNA = "ifany")
# table(ctd_all_site50$Flag_ORP_mV, useNA = "ifany")
# FILTER_GOOD_FLAGS <- TRUE   # set at top if you want to drop flagged data

