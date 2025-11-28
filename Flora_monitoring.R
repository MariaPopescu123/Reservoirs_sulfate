#checking out fluoropobe data to see which reservoir to sample from for sulfate experiment

#flora <- read.csv("Assay Experiment/csvs/fluoroprobe_L1.csv")
flora <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv")

library(dplyr)
library(lubridate)
library(tidyr)


df <- flora|>
  mutate(Date = as_date(DateTime))|>
  filter(Site == 50)|>
  filter(Reservoir == "FCR")

#I ran my assay experiments on 
#June 30 2025
#July 21 2025
#I want to see what those casts look like 

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
#----individual casts----#####
#    NOTE: change the date here if you want a different sampling day
sample_dat <- df %>%
  mutate(Date = date(DateTime)) %>%   # same Date definition as above
  filter(
    Reservoir == "FCR",
    Site == 50,
    Date == as.Date("2025-07-21")     # Assay 1:2025-06-30  #Assay 2: 2025-07-21
  )

# 3) Build plot_dat, keeping only rows whose CastID/Date/Reservoir are in sample_dat
plot_dat <- sample_dat %>%
  mutate(Date = date(DateTime)) %>%        # ensure Date exists
  filter(month(Date) != 1) %>%             # drop January if desired
  group_by(Reservoir, Date, Site, CastID) %>%
  mutate(FacetID = paste(Reservoir, Date, sep = " ")) %>%  # e.g. "BVR 2025-06-30"
  ungroup() %>%
  semi_join(sample_dat, by = c("Reservoir", "Date", "CastID")) %>%  # <- key join
  select(
    CastID, FacetID,
    GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL,
    TotalConc_ugL, Depth_m
    # SampleDepth   # add here later if/when you create it
  ) %>%
  pivot_longer(
    cols = GreenAlgae_ugL:TotalConc_ugL,
    names_to = "var",
    values_to = "ugL"
  )

# 4) Plot profiles for the selected casts
plot_casts <- ggplot(plot_dat, aes(x = ugL, y = Depth_m, group = var)) +
  geom_path(aes(color = var, linewidth = var)) +
  scale_y_reverse() +
  theme_bw() +
  facet_wrap(
    facets = vars(FacetID),
    nrow   = 2,
    ncol   = 5
  ) +
  xlab("micrograms per liter") +
  ylab("Depth (m)") +
  scale_color_manual(
    name = "Variable",
    values = c(
      "GreenAlgae_ugL"   = "green3",
      "Bluegreens_ugL"   = "blue",
      "BrownAlgae_ugL"   = "orange",
      "MixedAlgae_ugL"   = "red",
      "TotalConc_ugL"    = "black"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "GreenAlgae_ugL"   = 0.5,
      "Bluegreens_ugL"   = 0.5,
      "BrownAlgae_ugL"   = 0.5,
      "MixedAlgae_ugL"   = 0.5,
      "TotalConc_ugL"    = 0.8
    ),
    guide = "none"
  )

plot_casts

#----heatmaps----#####
library(dplyr)
library(lubridate)
library(ggplot2)
library(akima)      # for interp / interp2xyz
library(scales)     # for rescale
# library(oce) or your own source for blue2green2red, if needed
library(dplyr)
library(lubridate)
library(ggplot2)
library(akima)      # interp / interp2xyz
library(scales)     # rescale

flora_heatmap <- function(fp_data, year, site = NULL,
                          z = "TotalConc_ugL",
                          unitz = "µg/L",
                          max_legend_value = NA)
{
  z_col <- z  # store column name as string
  
  # ---------------------------
  # 1) Subset to year + site
  # ---------------------------
  fp <- fp_data %>%
    filter(year(DateTime) == year) %>%
    {
      if (!is.null(site) && "Site" %in% names(.)) {
        filter(., Site == site)
      } else {
        .
      }
    } %>%
    select(DateTime, Depth_m, z = .data[[z_col]])
  
  if (nrow(fp) == 0) {
    warning("No data for year = ", year,
            if (!is.null(site)) paste0(", site = ", site) else "")
    return(NULL)
  }
  
  # ---------------------------
  # 2) Slice by target depths
  # ---------------------------
  depths <- seq(0.1, 10, by = 0.3)
  df.final <- data.frame()
  
  for (d in depths) {
    fp_layer <- fp %>%
      group_by(DateTime) %>%
      slice(which.min(abs(as.numeric(Depth_m) - d))) %>%
      ungroup()
    
    df.final <- bind_rows(df.final, fp_layer)
  }
  
  # ---------------------------
  # 3) Wrangle
  # ---------------------------
  fp_new <- df.final %>%
    arrange(DateTime) %>%
    mutate(
      Depth_m = round(as.numeric(Depth_m), digits = 1),
      DOY     = yday(DateTime)
    ) %>%
    filter(
      !is.na(DOY), !is.infinite(DOY),
      !is.na(Depth_m), !is.infinite(Depth_m),
      !is.na(z), !is.infinite(z)
    )
  
  if (nrow(fp_new) == 0) {
    warning("After cleaning, no valid rows remain for year = ", year)
    return(NULL)
  }
  
  # ---------------------------
  # 4) Interpolation
  # ---------------------------
  fig_title <- paste("FCR", year, z_col)
  
  interp <- interp(
    x  = fp_new$DOY,
    y  = fp_new$Depth_m,
    z  = fp_new$z,
    xo = seq(min(fp_new$DOY),   max(fp_new$DOY),   by = 0.1),
    yo = seq(min(fp_new$Depth_m), max(fp_new$Depth_m), by = 0.05),
    extrap   = TRUE,
    linear   = TRUE,
    duplicate = "strip"
  )
  
  interp <- interp2xyz(interp, data.frame = TRUE)
  
  # ---------------------------
  # 5) Legend bounds
  # ---------------------------
  if (is.na(max_legend_value)) {
    max_legend_value <- max(interp$z, na.rm = TRUE)
  }
  
  z_min <- min(interp$z, na.rm = TRUE)
  z_max <- max_legend_value
  
  # ---------------------------
  # 6) Month breaks (no duplicate labels)
  # ---------------------------
  month_seq    <- seq(as.Date(paste0(year, "-01-01")),
                      by = "1 month", length.out = 12)
  month_breaks <- yday(month_seq)
  month_labels <- format(month_seq, "%b")
  
  # ---------------------------
  # 7) DOYs for vertical lines
  # ---------------------------
  date1 <- as.Date(paste0(year, "-06-30"))
  date2 <- as.Date(paste0(year, "-07-21"))
  doy1  <- yday(date1)
  doy2  <- yday(date2)
  
  # ---------------------------
  # 8) Plot
  # ---------------------------
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
    
    # vertical sampling lines
    geom_vline(xintercept = doy1, color = "black",
               linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = doy2, color = "black",
               linewidth = 1, linetype = "dashed") +
    
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = month_breaks,
      labels = month_labels
    ) +
    scale_fill_gradientn(
      colours = c(
        blue2green2red(60),
        rep("black", 10)
      ),
      values = rescale(c(z_min, 40, 80, 110, z_max)),
      limits = c(z_min, z_max),
      oob    = squish
    ) +
    labs(
      x     = "Day of year",
      y     = "Depth (m)",
      title = fig_title,
      fill  = unitz
    ) +
    theme_bw() +
    guides(fill = guide_colorbar(
      barwidth     = 0.6,
      barheight    = 16,
      ticks.colour = "black",
      frame.colour = "black",
      breaks       = c(0, 20, 40, 60, 80, 100, 150, 200, 500, 1000),
      labels       = c("0", "20", "40", "60", "80", "100",
                       "150", "200", "500", "1000")
    )) +
    theme(
      plot.title  = element_text(size = 22, face = "bold"),
      axis.title  = element_text(size = 16),
      axis.text   = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.title= element_text(size = 12),
      legend.key.size = unit(0.5, "cm")
    )
  
  print(p1)
  invisible(p1)
}



#### flora ####

#RAW DATA FCR 2025
b11 <- flora_heatmap(fp_data = df, year = 2025, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))


b11


ggsave("FCR2025heatmap.png", b11, width = 10, height = 10, dpi = 300)
