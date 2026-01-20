library(ggplot2)
library(dplyr)
library(lubridate)
library(rlang)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rlang)
library(patchwork)

#I want to see what dates we should have nutrients for FCR and BVR
phytos <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/10/6d7576cc758ca378fe004ad0ac9eed85")

filtered <- phytos|>
  mutate(Date = as.Date(DateTime))|>
  filter(Reservoir == "FCR" | Reservoir == "BVR")|>
  filter(Site == 50, year(Date) == 2025)
  
# Distinct dates for FCR
fcr_dates <- filtered %>%
  filter(Reservoir == "FCR") %>%
  distinct(Date) %>%
  arrange(Date)

# Distinct dates for BVR
bvr_dates <- filtered %>%
  filter(Reservoir == "BVR") %>%
  distinct(Date) %>%
  arrange(Date)

library(ggplot2)
library(dplyr)
library(lubridate)
library(rlang)

library(ggplot2)
library(dplyr)
library(lubridate)
library(rlang)

plot_data_availability_by_depth <- function(data,
                                            var,
                                            reservoir = NULL,
                                            site = NULL,
                                            date_col = "Date",
                                            depth_col = "Depth_m",
                                            x_limits = c(91, 331),
                                            x_breaks_by = 30,
                                            reverse_depth = TRUE,
                                            sparse_depth_min = 1,
                                            sparse_depth_max = 3,
                                            label_sparse_dates = TRUE,
                                            missing_dates = NULL,
                                            out_dir = NULL,
                                            filename = NULL,
                                            width = 12,
                                            height = 8,
                                            dpi = 300) {
  
  stopifnot(is.data.frame(data))
  
  var_sym   <- sym(var)
  date_sym  <- sym(date_col)
  depth_sym <- sym(depth_col)
  
  # --- Filter data by reservoir and site
  df <- data %>%
    { if (!is.null(reservoir)) filter(., Reservoir == reservoir) else . } %>%
    { if (!is.null(site)) filter(., Site == site) else . } %>%
    mutate(
      .date_parsed = suppressWarnings(as.Date(!!date_sym, format = "%m/%d/%Y")),
      Year = year(.date_parsed),
      DayOfYear = yday(.date_parsed)
    ) %>%
    filter(!is.na(.date_parsed)) %>%
    filter(!is.na(!!var_sym)) %>%
    filter(!is.na(!!depth_sym))
  
  if (nrow(df) == 0) stop("No rows left after filtering and removing NA values.")
  
  # --- Sparse-sampling days (optional)
  sparse_days <- df %>%
    group_by(Year, .date_parsed) %>%
    summarise(
      n_depths = n_distinct(!!depth_sym),
      DayOfYear = first(DayOfYear),
      .groups = "drop"
    ) %>%
    filter(n_depths >= sparse_depth_min, n_depths <= sparse_depth_max) %>%
    mutate(date_label = format(.date_parsed, "%m/%d"))
  
  # --- Label placement for sparse days
  year_label_y <- df %>%
    group_by(Year) %>%
    summarise(
      label_y = min(!!depth_sym, na.rm = TRUE),
      .groups = "drop"
    )
  sparse_days <- sparse_days %>% left_join(year_label_y, by = "Year")
  
  # --- Plot title
  title_bits <- c(
    paste0("Data Availability: ", var),
    if (!is.null(reservoir)) paste0("Reservoir = ", reservoir) else NULL,
    if (!is.null(site)) paste0("Site = ", site) else NULL
  )
  plot_title <- paste(title_bits, collapse = " | ")
  
  # --- Base plot
  p <- ggplot(df, aes(x = DayOfYear, y = !!depth_sym)) +
    geom_point(alpha = 0.85, size = 1.4) +
    facet_wrap(~Year, scales = "fixed") +
    theme_bw() +
    labs(title = plot_title, x = "Day of Year", y = "Depth (m)") +
    scale_x_continuous(
      breaks = seq(1, 365, by = x_breaks_by),
      limits = x_limits
    ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_text(face = "bold")
    )
  
  # --- Y-axis: sampled depths
  depth_breaks <- sort(unique(df[[depth_col]]))
  depth_min <- min(df[[depth_col]], na.rm = TRUE)
  depth_max <- max(df[[depth_col]], na.rm = TRUE)
  depth_mid <- (depth_min + depth_max) / 2   # Middle of the sampled depths
  
  if (isTRUE(reverse_depth)) {
    p <- p + scale_y_reverse(breaks = depth_breaks)
  } else {
    p <- p + scale_y_continuous(breaks = depth_breaks)
  }
  
  # --- Sparse sampling dashed lines
  if (isTRUE(label_sparse_dates) && nrow(sparse_days) > 0) {
    p <- p +
      geom_vline(
        data = sparse_days,
        aes(xintercept = DayOfYear),
        linetype = "dashed",
        linewidth = 0.35,
        alpha = 0.8
      ) +
      geom_text(
        data = sparse_days,
        aes(x = DayOfYear, y = label_y, label = date_label),
        angle = 90,
        vjust = -0.2,
        size = 3
      )
  }
  
  # --- Red lines for missing user-specified dates (label in middle of depth range)
  if (!is.null(missing_dates) && nrow(missing_dates) > 0) {
    missing_dates <- missing_dates %>%
      mutate(.date_parsed = suppressWarnings(as.Date(Date, format = "%m/%d/%Y")),
             DayOfYear = yday(.date_parsed)) %>%
      filter(!is.na(.date_parsed))
    
    if (nrow(missing_dates) > 0) {
      p <- p +
        geom_vline(
          data = missing_dates,
          aes(xintercept = DayOfYear),
          color = "red",
          linetype = "solid",
          linewidth = 0.5
        ) +
        geom_text(
          data = missing_dates,
          aes(x = DayOfYear, y = depth_mid,
              label = format(.date_parsed, "%d%b%y")),
          angle = 90, vjust = 0.5, color = "red", size = 3
        )
    }
  }
  
  # --- Save plot if requested
  if (!is.null(out_dir)) {
    if (is.null(filename)) {
      safe_res <- if (!is.null(reservoir)) paste0("_", reservoir) else ""
      safe_site <- if (!is.null(site)) paste0("_site", site) else ""
      filename <- paste0("data_availability_", var, safe_res, safe_site, ".png")
    }
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    ggsave(filename = file.path(out_dir, filename), plot = p, width = width, height = height, dpi = dpi)
  }
  
  return(list(plot = p, data_used = df, sparse_days = sparse_days))
}













df <- read.csv("Reservoir monitoring/sulfate_combined.csv", stringsAsFactors = FALSE)
df <- df %>%
  mutate(.date_parsed = mdy(Date))

bvr_dates <- bvr_dates %>% mutate(Date = as.Date(Date))
fcr_dates <- fcr_dates %>% mutate(Date = as.Date(Date))


res1 <- plot_data_availability_by_depth(
  data = df,
  var = "SO4_ugL",
  reservoir = "BVR",
  site = 50,
  out_dir = "EDI_prep/Figs/DataAvailability",
  sparse_depth_min = 1,
  sparse_depth_max = 3, 
  missing_dates = bvr_dates
)
res1$plot


res2 <- plot_data_availability_by_depth(
  data = df,
  var = "SO4_ugL",
  reservoir = "FCR",
  site = 50,
  out_dir = "EDI_prep/Figs/DataAvailability",
  sparse_depth_min = 1,
  sparse_depth_max = 6, 
  missing_dates = fcr_dates
)
res2$plot

combined_plot <- res1$plot + res2$plot


check <- df %>%
  mutate(Date = lubridate::mdy(Date)) %>%   # convert from "mm/dd/yyyy" format
  filter(
    year(Date) == 2025,
    month(Date) == 5,
    Reservoir == "BVR",
    Site == 50
  )

#I sent a message on July 18th so past that i have consistent samples 
#45 samples total

#for BVR missing:
4/07: 3.0
5/19: 6.0
6/02: 7.0
6/16: 0.1, 3.0
6/23: 0.1, 3.0, 6.0, 7.0 
6/30: 6.0, 7.0
7/7: 0.1, 3.0, 6.0, 7.0
7/14: 0.1, 3.0, 6.0, 7.0
8/11: 5.0

#for FCR missing:
5/12: 8.0
5/19: 1.6
6/02: 0.1, 9.0
6/09:  5.0, 6.2
6/16:  0.1, 1.6, 9.0
6/23:  0.1, 1.6, 3.8, 6.2, 9.0
6/30:  0.1, 3.8, 5.0, 6.2, 8.0
7/7:  0.1, 3.8, 5.0, 6.2, 8.0
7/14: 0.1, 3.8, 5.0, 6.2, 8.0

