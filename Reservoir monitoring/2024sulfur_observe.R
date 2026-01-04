#2024 sulfate
#Maria Popescu
#Sulfate and Iron concentrations


#you need to run all of these (make sure you have them installed first)
#if you don't have them installed, you'll need to run install.packages("YOUR_PACKAGE_HERE")
#in your terminal

library(dplyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)


#here we go
#change the depth that you want to see
#depths for fcr: 0.1, 5.0, 9.0
#depths for bvr: 0.1, 6.0, 11.0

#library(ggplot2)
#fcr <- filter(dt0, Reservoir == "FCR" & Depth_m == 5.0 & Site == 50)
#bvr <- filter(dt0, Reservoir == "BVR" & Depth_m == 6.0 & Site == 50)

#ggplot()+
# geom_point(data = fcr, aes(x = DateTime, y = SO4_mmol_L, color = "blue"))+
# geom_point(data = bvr, aes(x = DateTime, y = SO4_mmol_L, color = "red"))+
#  labs(title = "SO4 at FCR (5m) and BVR (6m)", x = "Month", y = "SO4 mmol/L")+
#  scale_color_manual(values = c("blue", "red"), labels = c("FCR", "BVR"))+  # Legend with colors and labels
#  theme_classic()

#change the depth and year that you want to see
#depths for fcr: epi: 0.1, 1.6, meta: 3.8, 5.0, hypo: 6.2, 8.0, 9.0

dtNEW <- read.csv("Reservoir monitoring/sulfate_combined.csv")

library(stringr)

###----2024 data----####

dt0 <- dtNEW |>
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    SO4_ugL = as.numeric(SO4_ugL), 
    Depth_m = as.numeric(Depth_m), 
    Reservoir = if_else(str_detect(Reservoir, "^BVR"), "BVR", "FCR") #just in case it's typed weird with spaces 
  ) |>
  filter(!is.na(SO4_ugL))|>
  filter(year(Date) == 2024) #change this here for whichever year you would like to see

#plotting FCR sulfate  
FCR <- dt0|>
  filter(Reservoir == "FCR", Site == 50)%>%
  mutate(limnion = case_when(
    Depth_m <= 1.6 ~ "Epilimnion",
    Depth_m > 1.6 & Depth_m < 6 ~ "Metalimnion",
    Depth_m >= 6 ~ "Hypolimnion"
  )) %>%
  mutate(limnion = factor(limnion, levels = c("Epilimnion", "Metalimnion", "Hypolimnion")))


#plot FCR sulfate concentrations
ggplot(data = FCR, aes(x = Date, y = SO4_ugL, color = as.factor(Depth_m))) +
  geom_point() +
  facet_grid(rows = vars(limnion), scales = "free_y") +
  theme_bw() +
  labs(x = "Month", y = "SO4_ugL", title = "FCR sulfate") +
  scale_color_discrete(name = "Depth (m)") +
  scale_x_date(date_labels = "%b") +  # <--- only shows month letters like Jan, Feb, etc.
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 13),
    strip.text.y = element_text(size = 12)
  )

#plotBVR sulfate
#depths for bvr: epi: 0.1, meta: 3.0 hypo: 6.0, 9.0

BVR <- dt0|>
  filter(Reservoir == "BVR", Site == 50)%>%
  mutate(limnion = case_when(
    Depth_m <= 1.6 ~ "Epilimnion",
    Depth_m > 1.6 & Depth_m < 7 ~ "Metalimnion",
    Depth_m > 6 ~ "Hypolimnion"
  )) %>%
  mutate(limnion = factor(limnion, levels = c("Epilimnion", "Metalimnion", "Hypolimnion")))


#plotFCR sulfate and iron concentrations
ggplot(data = BVR, aes(x = Date, y = SO4_ugL, color = as.factor(Depth_m)))+
  geom_point()+
  facet_grid(rows = vars(limnion), scales = "free_y")+
  theme_bw()+
  labs(x = "Date", y = "SO4_ugL", title = "BVR sulfate")+
  scale_color_discrete(name = "Depth (m)")+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

#now to have the y axis be the same scale

ggplot(data = FCR, aes(x = Date, y = SO4_ugL, color = as.factor(Depth_m))) +
  geom_point() +
  facet_grid(rows = vars(limnion)) +  # no more scales = "free_y"
  ylim(0, 2750) +  # fixed y-axis range
  theme_bw() +
  labs(x = "Date", y = "SO4_ugL", title = "FCR sulfate") +
  scale_color_discrete(name = "Depth (m)") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

ggplot(data = BVR, aes(x = Date, y = SO4_ugL, color = as.factor(Depth_m))) +
  geom_point() +
  facet_grid(rows = vars(limnion)) +  # no more scales = "free_y"
  ylim(0, 2750) +  # fixed y-axis range
  theme_bw() +
  labs(x = "Date", y = "SO4_ugL", title = "BVR sulfate") +
  scale_color_discrete(name = "Depth (m)") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

#----inflow----####

other_sites <- dtNEW|>
  mutate(Date = as.Date(Date))|>
  filter(Site != 50, year(Date) == 2025)


####heatmap function####
library(akima)
my_colors <- c("red", "orange", "yellow","cyan","blue") 

heatmap <- function(fp_data, reservoir, year, z, unitz, chlorophyll_data = NA, max_legend_value = NA) {
  
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
    xo = seq(min(fp_new$DOY), max(fp_new$DOY), by = 0.5),
    yo = seq(min(fp_new$Depth_m), max(fp_new$Depth_m), by = 0.5),
    extrap = T, linear = T, duplicate = "strip"
  )
  
  interp <- interp2xyz(interp, data.frame = T)
  
  month_seq    <- seq(
    as.Date(paste0(year, "-01-01")),
    as.Date(paste0(year, "-12-01")),
    by = "1 month"
  )
  month_breaks <- lubridate::yday(month_seq)
  month_labels <- format(month_seq, "%b")
  
  p1 <- ggplot(interp, aes(x = x, y = y)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse(expand = c(0,0)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = month_breaks,
      labels = month_labels
    ) +
    scale_fill_gradientn(
      colors = rev(my_colors),
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
      shape = 17, color = "black", size = 2
    )
  
  print(p1)
}

####------2024 heatmap----####
FCRSO4heatmap2024 <- heatmap(dt0, "FCR", 2024, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/FCRSO4heatmap2024.png", 
       plot = FCRSO4heatmap2024, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRRSO4heatmap2024 <- heatmap(dt0, "BVR", 2024, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/BVRRSO4heatmap2024.png", 
       plot = BVRRSO4heatmap2024, 
       width = 5, 
       height = 5, 
       dpi = 500)


####----2016 heatmap-----####
dt1 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/607/0/86bc3dc8c1eafe36e6935f8a858a7b27")

dt1<- dt1|>
  mutate(FE2_ugL = (FE2_mmol_L * 55845))|> # Conversion factor based on atomic weight of Fe (55.845 g/mol)
  mutate(SO4_ugL = (SO4_mmol_L * 96065))|> # Conversion factor based on molecular weight of SO4 (96.065 g/mol)
  mutate(Date = as_date(DateTime))

FCRSO4heatmap2016 <- heatmap(dt1, "FCR", 2016, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/FCRSO4heatmap2016.png", 
       plot = FCRSO4heatmap2016, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRSO4heatmap2016 <- heatmap(dt1, "BVR", 2016, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/BVRRSO4heatmap2016.png", 
       plot = BVRSO4heatmap2016, 
       width = 5, 
       height = 5, 
       dpi = 500)

####----2025 heatmap----####
dt0 <- dtNEW |>
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    SO4_ugL = as.numeric(SO4_ugL), 
    Depth_m = as.numeric(Depth_m), 
    Reservoir = if_else(str_detect(Reservoir, "^BVR"), "BVR", "FCR") #just in case it's typed weird with spaces 
  ) |>
  filter(!is.na(SO4_ugL))|>
  filter(year(Date) == 2025)


FCRSO4heatmap2025 <- heatmap(dt0, "FCR", 2025, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/FCRSO4heatmap2025.png", 
       plot = FCRSO4heatmap2025, 
       width = 5, 
       height = 5, 
       dpi = 500)

BVRRSO4heatmap2025 <- heatmap(dt0, "BVR", 2025, "SO4_ugL", "ugL", max_legend_value = 2500)
ggsave(filename = "Reservoir monitoring/Figs/BVRRSO4heatmap2025.png", 
       plot = BVRRSO4heatmap2025, 
       width = 5, 
       height = 5, 
       dpi = 500)


####2024 1.6m all####
pHframe2024 <- ysi_profiles|>
  mutate(Date = as_date(DateTime))|>
  filter(year(Date) == 2024, month(Date) > 03, Site == 50)|>
  select(Date, Reservoir, Site, Depth_m, pH)

phytos2024<- current_df|>
  mutate(Date = as_date(DateTime)) |>
  filter(!is.na(TotalConc_ugL))|>
  filter(year(Date) == 2024, month(Date) > 03, Site == 50)|>
  select(Date, Reservoir, Site, Depth_m, TotalConc_ugL)

source("interpolate_variable.R")
#for just FCR
pHframe2024FCR<- pHframe2024|>
  filter(Reservoir == "FCR")
pHframe2024_interp <- interpolate_variable(pHframe2024FCR, "pH")|>
  filter(!is.na(pH))|>
  filter(Depth_m == 1.6)|>
  select(Depth_m, Date, pH)

O22024FCR <- O22024|>
  filter(Reservoir == "FCR")
O22024_interp <- interpolate_variable(O22024FCR, "DO_mgL")|>
  filter(!is.na(DO_mgL))|>
  filter(Depth_m == 1.6)|>
  select(Depth_m, Date, DO_mgL)

phytos2024FCR<- phytos2024|>
  filter(Reservoir == "FCR")
phytos2024_interp <- interpolate_variable(phytos2024FCR, "TotalConc_ugL")|>
  filter(!is.na(TotalConc_ugL))|>
  filter(Depth_m == 1.6)|>
  select(Depth_m, Date, TotalConc_ugL)

# Combine all dataframes with a full join
sulfate2024epi <- dt0|>
  filter(Depth_m == 1.6, Reservoir == "FCR")
  
dfs <- list(pHframe2024_interp, O22024_interp, phytos2024_interp, sulfate2024epi)
frame2024 <- reduce(dfs, full_join, by = c("Date", "Depth_m"))

#now to plot 2024 depth 1.6
# First, calculate scaling factors
# Define a scale factor between the two variables
max_phyto <- max(frame2024$TotalConc_ugL, na.rm = TRUE)
max_sulfate <- max(frame2024$SO4_ugL, na.rm = TRUE)
scale_factor <- max_phyto / max_sulfate

# Plot with rescaled sulfate

frame2024_sorted <- frame2024 %>% arrange(Date)

frame2024_SO4 <- frame2024 %>%
  filter(!is.na(SO4_ugL)) %>%
  arrange(Date)

frame2024_DO <- frame2024 %>%
  filter(!is.na(DO_mgL)) %>%
  arrange(Date)

frame2024_pH <- frame2024 %>%
  filter(!is.na(pH)) %>%
  arrange(Date)

frame2024_phyto <- frame2024 %>%
  filter(!is.na(TotalConc_ugL)) %>%
  arrange(Date)

max_DO <- 15
max_SO4 <- max(frame2024_SO4$SO4_ugL, na.rm = TRUE)

scale_factor <- max_SO4 / max_DO

ggplot(frame2024_SO4, aes(x = Date)) +
  geom_point(aes(y = SO4_ugL, color = "SO4 (ug/L)")) +  # Added color aesthetic
  geom_line(aes(y = SO4_ugL, color = "SO4 (ug/L)"), group = 1) +  # Added color aesthetic
  geom_line(
    data = frame2024_DO,
    aes(x = Date, y = DO_mgL * scale_factor, color = "DO (mg/L)")  # Added color aesthetic for DO
  ) +
  geom_line(
    data = frame2024_pH,  # Reusing frame2024_DO for pH
    aes(x = Date, y = pH * scale_factor, color = "pH (scaled)")  # Added pH using the same scale factor
  ) +
  scale_y_continuous(
    name = "SO4 (ug/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "DO and pH (scaled)", breaks = seq(0, 15, 3))
  ) +
  labs(x = "Date", title = "SO4, DO, and pH Time Series", color = "Variable") +  # Added color legend label
  theme_minimal() +
  scale_color_manual(values = c("SO4 (ug/L)" = "orange", "DO (mg/L)" = "blue", "pH (scaled)" = "green"))  # Custom color scale

max_phyto <- 70
max_SO4 <- max(frame2024_SO4$SO4_ugL, na.rm = TRUE)

scale_factor <- max_SO4 / max_phyto

ggplot(frame2024_SO4, aes(x = Date)) +
  # SO4 data points and line
  geom_point(aes(y = SO4_ugL, color = "SO4 (ug/L)")) +  
  geom_line(aes(y = SO4_ugL, color = "SO4 (ug/L)"), group = 1) +  
  # Phytoplankton data points and line (scaled by scale_factor)
  geom_line(
    data = frame2024_phyto,
    aes(x = Date, y = TotalConc_ugL * scale_factor, color = "Phytos (ug/L)")  # Line for Phytos scaled
  ) +
  geom_point(
    data = frame2024_phyto,
    aes(x = Date, y = TotalConc_ugL * scale_factor, color = "Phytos (ug/L)")  # Points for Phytos scaled
  ) +
  scale_y_continuous(
    name = "SO4 (ug/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Phytos (ug/L)", breaks = seq(0, 80, by = 10))  # Dynamic breaks based on data
  ) +
  labs(x = "Date", title = "SO4 and Phytos Time Series", color = "Variable") +  
  theme_minimal() +
  scale_color_manual(values = c("SO4 (ug/L)" = "orange", "Phytos (ug/L)" = "black"))  # Custom color scale

####2024 3.8m all####
# Filter pH data
pHframe2024FCR <- pHframe2024 |> filter(Reservoir == "FCR")
pHframe2024_interp <- interpolate_variable(pHframe2024FCR, "pH") |>
  filter(!is.na(pH)) |>
  select(Depth_m, Date, pH)

# Filter DO data
O22024FCR <- O22024 |> filter(Reservoir == "FCR")
O22024_interp <- interpolate_variable(O22024FCR, "DO_mgL") |>
  filter(!is.na(DO_mgL)) |>
  select(Depth_m, Date, DO_mgL)

# Filter phytoplankton data
phytos2024FCR <- phytos2024 |> filter(Reservoir == "FCR")
phytos2024_interp <- interpolate_variable(phytos2024FCR, "TotalConc_ugL") |>
  filter(!is.na(TotalConc_ugL)) |>
  select(Depth_m, Date, TotalConc_ugL)

# Filter sulfate data
sulfate2024epi <- dt0 |>
  filter(Reservoir == "FCR", Depth_m == 3.8)

# Join all data frames
dfs <- list(pHframe2024_interp, O22024_interp, phytos2024_interp, sulfate2024epi)
frame2024 <- reduce(dfs, full_join, by = c("Date", "Depth_m"))
frame2024 <- frame2024|>
  filter(abs(Depth_m - 3.8) < 0.05)


#now to plot 2024 depth 1.6
# First, calculate scaling factors
# Define a scale factor between the two variables
max_phyto <- max(frame2024$TotalConc_ugL, na.rm = TRUE)
max_sulfate <- max(frame2024$SO4_ugL, na.rm = TRUE)
scale_factor <- max_phyto / max_sulfate

# Plot with rescaled sulfate

frame2024_sorted <- frame2024 %>% arrange(Date)

frame2024_SO4 <- frame2024 %>%
  filter(!is.na(SO4_ugL)) %>%
  arrange(Date)

frame2024_DO <- frame2024 %>%
  filter(!is.na(DO_mgL)) %>%
  arrange(Date)

frame2024_pH <- frame2024 %>%
  filter(!is.na(pH)) %>%
  arrange(Date)

frame2024_phyto <- frame2024 %>%
  filter(!is.na(TotalConc_ugL)) %>%
  arrange(Date)

max_DO <- 15
max_SO4 <- max(frame2024_SO4$SO4_ugL, na.rm = TRUE)

scale_factor <- max_SO4 / max_DO

ggplot(frame2024_SO4, aes(x = Date)) +
  geom_point(aes(y = SO4_ugL, color = "SO4 (ug/L)")) +  # Added color aesthetic
  geom_line(aes(y = SO4_ugL, color = "SO4 (ug/L)"), group = 1) +  # Added color aesthetic
  geom_line(
    data = frame2024_DO,
    aes(x = Date, y = DO_mgL * scale_factor, color = "DO (mg/L)")  # Added color aesthetic for DO
  ) +
  geom_line(
    data = frame2024_pH,  # Reusing frame2024_DO for pH
    aes(x = Date, y = pH * scale_factor, color = "pH (scaled)")  # Added pH using the same scale factor
  ) +
  scale_y_continuous(
    name = "SO4 (ug/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "DO and pH (scaled)", breaks = seq(0, 15, 3))
  ) +
  labs(x = "Date", title = "SO4, DO, and pH Time Series", color = "Variable") +  # Added color legend label
  theme_minimal() +
  scale_color_manual(values = c("SO4 (ug/L)" = "orange", "DO (mg/L)" = "blue", "pH (scaled)" = "green"))  # Custom color scale

max_phyto <- 70
max_SO4 <- max(frame2024_SO4$SO4_ugL, na.rm = TRUE)

scale_factor <- max_SO4 / max_phyto

ggplot(frame2024_SO4, aes(x = Date)) +
  # SO4 data points and line
  geom_point(aes(y = SO4_ugL, color = "SO4 (ug/L)")) +  
  geom_line(aes(y = SO4_ugL, color = "SO4 (ug/L)"), group = 1) +  
  # Phytoplankton data points and line (scaled by scale_factor)
  geom_line(
    data = frame2024_phyto,
    aes(x = Date, y = TotalConc_ugL * scale_factor, color = "Phytos (ug/L)")  # Line for Phytos scaled
  ) +
  geom_point(
    data = frame2024_phyto,
    aes(x = Date, y = TotalConc_ugL * scale_factor, color = "Phytos (ug/L)")  # Points for Phytos scaled
  ) +
  scale_y_continuous(
    name = "SO4 (ug/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Phytos (ug/L)", breaks = seq(0, 300, by = 50))  # Dynamic breaks based on data
  ) +
  labs(x = "Date", title = "SO4 and Phytos Time Series", color = "Variable") +  
  theme_minimal() +
  scale_color_manual(values = c("SO4 (ug/L)" = "orange", "Phytos (ug/L)" = "black"))  # Custom color scale


#----iron----

metals <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5") 

metals2<- metals |>
  mutate(FeIII = TFe_mgL - SFe_mgL)|>
  mutate(Date = as.Date(DateTime))

this <- heatmap(metals2, "FCR", 2016, "FeIII", "mgL" , chlorophyll_data = NA, max_legend_value = NA) 
ggsave(filename = "Reservoir monitoring/Figs/FCR_FeIIIheatmap2016.png", 
       plot = this, 
       width = 5, 
       height = 5, 
       dpi = 500)

this <- heatmap(metals2, "FCR", 2024, "FeIII", "mgL" , chlorophyll_data = NA, max_legend_value = NA) 
ggsave(filename = "Reservoir monitoring/Figs/FCR_FeIIIheatmap2024.png", 
       plot = this, 
       width = 5, 
       height = 5, 
       dpi = 500)

this <- heatmap(metals2, "BVR", 2016, "FeIII", "mgL" , chlorophyll_data = NA, max_legend_value = NA) 
ggsave(filename = "Reservoir monitoring/Figs/BVR_FeIIIheatmap2016.png", 
       plot = this, 
       width = 5, 
       height = 5, 
       dpi = 500)

this <- heatmap(metals2, "BVR", 2024, "FeIII", "mgL" , chlorophyll_data = NA, max_legend_value = NA) 
ggsave(filename = "Reservoir monitoring/Figs/BVR_FeIIIheatmap2024.png", 
       plot = this, 
       width = 5, 
       height = 5, 
       dpi = 500)
