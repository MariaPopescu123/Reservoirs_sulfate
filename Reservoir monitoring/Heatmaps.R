
####oxygen####

DO_heatmap <- function(fp_data, reservoir, year, z, unitz, chlorophyll_data = NA, max_legend_value = NA) {
  
  library(dplyr)
  library(akima)
  library(purrr)
  library(ggplot2)
  library(viridis)
  library(lubridate)
  
  fp <- fp_data %>%
    filter(year(Date) == year, Reservoir == reservoir) %>%
    select(Date, Reservoir, Depth_m, !!sym(z)) # <- use sym() to turn string into a symbol
  
  depths = seq(0.1, 11, by = 0.3)
  
  df.final <- map_dfr(depths, function(d) {
    fp %>%
      group_by(Date) %>%
      slice(which.min(abs(as.numeric(Depth_m) - d))) %>%
      mutate(target_depth = d)
  })
  
  fp_new <- df.final %>%
    arrange(Date) %>%
    mutate(
      Depth_m = round(as.numeric(Depth_m), 0.5),
      DOY = yday(Date)
    ) %>%
    filter(
      !is.na(DOY), !is.na(Depth_m), !is.infinite(DOY), !is.infinite(Depth_m),
      !is.na(.data[[z]]), !is.infinite(.data[[z]])
    )
  
  fig_title <- paste(reservoir, year, z, sep = " ")
  
  interp <- interp(
    x = fp_new$DOY, 
    y = fp_new$Depth_m, 
    z = fp_new[[z]], # <- use string column access here
    xo = seq(min(fp_new$DOY), max(fp_new$DOY), by = 0.2),
    yo = seq(min(fp_new$Depth_m), max(fp_new$Depth_m), by = 0.2),
    extrap = T, linear = T, duplicate = "strip"
  )
  
  interp <- interp2xyz(interp, data.frame = T)
  
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse(expand = c(0,0)) +
    scale_x_continuous(
      expand = c(0, 0), breaks = seq(1, 366, by = 30),
      labels = function(x) format(as.Date(x - 1, origin = paste0(year, "-01-01")), "%b")
    ) +
    scale_fill_gradientn(
      colors = DO_colors,
      values = scales::rescale(c(0, 1, 2, 4, 6, 10, max_legend_value)),
      limits = c(0, max_legend_value),
      oob = scales::squish,
      na.value = "gray"
    ) +
    labs(x = "Day of year", y = "Depth (m)", title = fig_title, fill = unitz) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm")
    )+
    geom_point(
      data = fp_new, 
      aes(x = DOY, y = Depth_m), 
      shape = 17, color = "black", size = .5
    )
  
  print(p1)
}


ctd <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv")

check <- ctd|>
  mutate(Date = as_date(DateTime))|>
  filter(year(Date) == 2025, Site == 50)|>
  select(Date, DO_mgL, ORP_mV, Depth_m, Reservoir, Site)

####----DO 2016----####
ctd <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1")

check <- ctd|>
  mutate(Date = as_date(DateTime))|>
  filter(!is.na(DO_mgL))|>
  filter(Site == 50)|>
  select(Date, DO_mgL, Depth_m, Reservoir, Site)

FCRoxyheatmap2016 <- DO_heatmap(check, "FCR", 2016, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/FCRoxyheatmap2016.png", 
       plot = FCRoxyheatmap2016, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRoxyheatmap2016<- DO_heatmap(check, "BVR", 2016, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/BVRoxyheatmap2016.png", 
       plot = BVRoxyheatmap2016, 
       width = 5, 
       height = 5, 
       dpi = 500)


####----DO 2024----####
FCRoxyheatmap2024 <- DO_heatmap(check, "FCR", 2024, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/FCRoxyheatmap2024.png", 
       plot = FCRoxyheatmap2024, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRoxyheatmap2024<- DO_heatmap(check, "BVR", 2024, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/BVRoxyheatmap2024.png", 
       plot = BVRoxyheatmap2024, 
       width = 5, 
       height = 5, 
       dpi = 500)

####----DO 2025----####
DO_colors <- c("red4","red","yellow","cyan", "blue","blue3")

FCRoxyheatmap2025 <- DO_heatmap(check, "FCR", 2025, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/FCRoxyheatmap2025.png", 
       plot = FCRoxyheatmap2025, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRoxyheatmap2025 <- DO_heatmap(check, "BVR", 2025, "DO_mgL", "mgL", max_legend_value = 15)
ggsave(filename = "Reservoir monitoring/Figs/BVRoxyheatmap2025.png", 
       plot = BVRoxyheatmap2025, 
       width = 5, 
       height = 5, 
       dpi = 500)



#------ORP-----####
ctd <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv")

check2025 <- ctd|>
  mutate(Date = as_date(DateTime))|>
  filter(year(Date) == 2025, Site == 50)|>
  select(Date, DO_mgL, ORP_mV, Depth_m, Reservoir, Site)


ORP_heatmap <- function(fp_data,
                        reservoir,
                        year,
                        z = "ORP_mV",           # name of ORP column
                        unitz = "ORP (mV)",
                        min_legend_value = -250,
                        max_legend_value = 350) {
  
  library(dplyr)
  library(akima)
  library(purrr)
  library(ggplot2)
  library(viridis)
  library(lubridate)
  
  # subset & pick the ORP column given by `z`
  fp <- fp_data %>%
    filter(year(Date) == year, Reservoir == reservoir) %>%
    select(Date, Reservoir, Depth_m, !!sym(z))
  
  depths <- seq(0.1, 11, by = 0.3)
  
  df.final <- map_dfr(depths, function(d) {
    fp %>%
      group_by(Date) %>%
      slice(which.min(abs(as.numeric(Depth_m) - d))) %>%
      mutate(target_depth = d)
  })
  
  fp_new <- df.final %>%
    arrange(Date) %>%
    mutate(
      Depth_m = round(as.numeric(Depth_m), 0.5),
      DOY = yday(Date)
    ) %>%
    filter(
      !is.na(DOY), !is.na(Depth_m),
      !is.infinite(DOY), !is.infinite(Depth_m),
      !is.na(.data[[z]]), !is.infinite(.data[[z]])
    )
  
  fig_title <- paste(reservoir, year, z, sep = " ")
  
  # interpolation
  interp <- interp(
    x   = fp_new$DOY,
    y   = fp_new$Depth_m,
    z   = fp_new[[z]],
    xo  = seq(min(fp_new$DOY),    max(fp_new$DOY),    by = 0.2),
    yo  = seq(min(fp_new$Depth_m),max(fp_new$Depth_m),by = 0.2),
    extrap  = TRUE,
    linear  = TRUE,
    duplicate = "strip"
  )
  
  interp <- interp2xyz(interp, data.frame = TRUE)
  
  # --- ORP color scale ---
  # ranges chosen to line up with your table:
  #  -250 to -100  ~ sulfide / P release
  #  -100 to  -50  ~ fermentation / transition
  #   -50 to  +50  ~ denitrification
  #   +50 to +100  ~ cBOD
  #  +100 to +350  ~ nitrification
  ORP_colors <- c("red4","red","yellow","cyan", "blue","blue3")
  ORP_values <- scales::rescale(
    c(min_legend_value, -250, -100, 0, 100, max_legend_value),
    from = c(min_legend_value, max_legend_value)
  )
  
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 366, by = 30),
      labels = function(x)
        format(as.Date(x - 1, origin = paste0(year, "-01-01")), "%b")
    ) +
    scale_fill_gradientn(
      colors = ORP_colors,
      values = ORP_values,
      limits = c(min_legend_value, max_legend_value),
      oob    = scales::squish,
      na.value = "grey80",
      name = unitz
    ) +
    labs(
      x = "Day of year",
      y = "Depth (m)",
      title = fig_title
    ) +
    theme_bw() +
    theme(
      legend.text  = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm")
    ) +
    geom_point(
      data = fp_new,
      aes(x = DOY, y = Depth_m),
      shape = 17, color = "black", size = 0.5
    )
  
  print(p1)
}

#----2025----
ORP_heatmap(check2025, "FCR", 2025,
            z = "ORP_mV",
            unitz = "ORP (mV)",
            min_legend_value = -250,
            max_legend_value = 350)

ORP_heatmap(check2025, "BVR", 2025,
            z = "ORP_mV",
            unitz = "ORP (mV)",
            min_legend_value = -250,
            max_legend_value = 350)

#----2024----
ctd2024 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/200/15/9d741c9cced69cfd609c473ada2812b1")

check2024 <- ctd2024|>
  mutate(Date = as_date(DateTime))|>
  filter(!is.na(DO_mgL))|>
  filter(Site == 50)|>
  select(Date, ORP_mV, Depth_m, Reservoir, Site)

ORP_heatmap(check2024, "FCR", 2024,
            z = "ORP_mV",
            unitz = "ORP (mV)",
            min_legend_value = -250,
            max_legend_value = 350)

ORP_heatmap(check2024, "BVR", 2024,
            z = "ORP_mV",
            unitz = "ORP (mV)",
            min_legend_value = -250,
            max_legend_value = 350)

#----2016----
check2016 <- check2024|>
  filter(year(Date)==2016)|>
  filter(!is.na(ORP_mV))

###Phytoplankton function####
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(akima)      # interp / interp2xyz
library(scales)     # rescale

flora_heatmap <- function(fp_data,
                          reservoir,           # "FCR" or "BVR"
                          year,
                          site = NULL,
                          z = "TotalConc_ugL",
                          unitz = "µg/L",
                          max_legend_value = NA) {
  
  z_col <- z  # store column name as string
  
  fp <- fp_data %>%
    filter(year(DateTime) == year,
           Reservoir == reservoir) %>%     # <-- filter by reservoir here
    {
      if (!is.null(site) && "Site" %in% names(.)) {
        filter(., Site == site)
      } else {
        .
      }
    } %>%
    select(DateTime, Depth_m, z = .data[[z_col]])
  
  if (nrow(fp) == 0) {
    warning("No data for reservoir = ", reservoir,
            ", year = ", year,
            if (!is.null(site)) paste0(", site = ", site) else "")
    return(NULL)
  }
  
  depths <- seq(0.1, 10, by = 0.3)
  df.final <- data.frame()
  
  for (d in depths) {
    fp_layer <- fp %>%
      group_by(DateTime) %>%
      slice(which.min(abs(as.numeric(Depth_m) - d))) %>%
      ungroup()
    
    df.final <- bind_rows(df.final, fp_layer)
  }
  
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
    warning("After cleaning, no valid rows remain for reservoir = ",
            reservoir, ", year = ", year)
    return(NULL)
  }
  
  fig_title <- paste(reservoir, year, z_col)   # <-- use reservoir here
  
  interp <- interp(
    x  = fp_new$DOY,
    y  = fp_new$Depth_m,
    z  = fp_new$z,
    xo = seq(min(fp_new$DOY),     max(fp_new$DOY),     by = 0.1),
    yo = seq(min(fp_new$Depth_m), max(fp_new$Depth_m), by = 0.05),
    extrap   = TRUE,
    linear   = TRUE,
    duplicate = "strip"
  )
  
  interp <- interp2xyz(interp, data.frame = TRUE)
  
  if (is.na(max_legend_value)) {
    max_legend_value <- max(interp$z, na.rm = TRUE)
  }
  
  z_min <- min(interp$z, na.rm = TRUE)
  z_max <- max_legend_value
  
  month_seq    <- seq(as.Date(paste0(year, "-01-01")),
                      by = "1 month", length.out = 12)
  month_breaks <- yday(month_seq)
  month_labels <- format(month_seq, "%b")
  
  date1 <- as.Date(paste0(year, "-06-30"))
  date2 <- as.Date(paste0(year, "-07-21"))
  doy1  <- yday(date1)
  doy2  <- yday(date2)
  
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
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



####----2025----####
flora <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv")

df2025 <- flora %>%
  mutate(DateTime = as.POSIXct(DateTime))  # if not already POSIXct

#RAW DATA FCR 2025
b11 <- flora_heatmap(fp_data = df2025, reservoir = "FCR", year = 2025, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/FCR2025phytos.png",
       b11, width = 5, height = 5, dpi = 300)

b12 <- flora_heatmap(fp_data = df2025,reservoir = "BVR", year = 2025, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/BVR2025phytos.png",
       b12, width = 5, height = 5, dpi = 300)


####-----2016----####
current_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/272/9/f246b36c591a888cc70ebc87a5abbcb7")
df <- current_df %>%
  mutate(DateTime = as.POSIXct(DateTime))  

b13 <- flora_heatmap(fp_data = df, reservoir = "FCR", year = 2016, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/FCR2016phytos.png",
       b13, width = 5, height = 5, dpi = 300)

b14 <- flora_heatmap(fp_data = df,reservoir = "BVR", year = 2016, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/BVR2016phytos.png",
       b14, width = 5, height = 5, dpi = 300)


####----2024----####
b15 <- flora_heatmap(fp_data = df, reservoir = "FCR", year = 2024, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/FCR2024phytos.png",
       b15, width = 5, height = 5, dpi = 300)

b16 <- flora_heatmap(fp_data = df,reservoir = "BVR", year = 2024, site = 50, z = "TotalConc_ugL", unitz = "ug/L", max_legend_value = max(df$TotalConc_ugL))
ggsave("Reservoir monitoring/Figs/phytos/BVR2024phytos.png",
       b16
       , width = 5, height = 5, dpi = 300)

