#analyzing sulfate data 

pacman::p_load(tidyverse, patchwork, lubridate, akima, reshape2, pracma,
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes, splines, readr, ggbeeswarm)

assay_sulfate <- read_excel("Assay Experiment/assay_sulfate_data.xlsx")



assay_sulfate1 <- assay_sulfate %>%
  mutate(SO4 = as.numeric(SO4)) %>%
  filter(!is.na(SO4)) %>%
  mutate(SO4 = round(SO4, 0))%>%
  mutate(Dose = ifelse(Flask %in% c("init1", "init2"), "init", Dose))|>
  filter(Round == 1)|>
  mutate(Day = as.factor(Day))

assay_sulfate2 <- assay_sulfate %>%
  mutate(SO4 = as.numeric(SO4)) %>%
  filter(!is.na(SO4)) %>%
  mutate(SO4 = round(SO4, 0))%>%
  mutate(Dose = ifelse(Flask %in% c("init1", "init2"), "init", Dose))|>
  filter(Round == 2)|>
  mutate(Day = as.factor(Day))


library(ggplot2)

####visualize sulfate####

#------Assay 1------####
# Prepare data
boxplot_data <- assay_sulfate1 %>%
  filter(!is.na(Dose)) %>%
  mutate(Dose = as.factor(Dose))

# Calculate mean and SD for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_SO4 = mean(SO4, na.rm = TRUE),
    sd_SO4 = sd(SO4, na.rm = TRUE),
    label = paste0(round(mean_SO4, 1), " ± ", round(sd_SO4, 1)),
    y_pos = max(SO4, na.rm = TRUE) + 1  # position label slightly above the highest point
  )


Assay1sulfateplot <- ggplot(boxplot_data, aes(x = Dose, y = SO4)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +
  geom_jitter(aes(color = Day),  # map color to Day
              width = 0.2, size = 2, alpha = 0.6) +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Assay 1 Sulfate Concentrations by Dose",
    x = "Dose",
    y = "Sulfate Concentration (SO4)",
    color = "Day"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/Assay1sulfateplot.png",
  plot = Assay1sulfateplot,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)



#-----Assay 2----####
# Prepare data
boxplot_data <- assay_sulfate2 %>%
  filter(!is.na(Dose)) %>%
  mutate(Dose = as.factor(Dose))

# Calculate mean and SD for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_SO4 = mean(SO4, na.rm = TRUE),
    sd_SO4 = sd(SO4, na.rm = TRUE),
    label = paste0(round(mean_SO4, 1), " ± ", round(sd_SO4, 1)),
    y_pos = max(SO4, na.rm = TRUE) + 1  # position label slightly above the highest point
  )

Assay1sulfateplot <- ggplot(boxplot_data, aes(x = Dose, y = SO4, fill = Day)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_jitter(aes(color = Day),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              size = 2, alpha = 0.6) +
  labs(
    title = "Assay 2 Sulfate Concentrations by Dose and Day",
    x = "Dose",
    y = "Sulfate Concentration (SO4)",
    fill = "Day",
    color = "Day"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/Assay2sulfateplot.png",
  plot = Assay1sulfateplot,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)



####having the days seperate####
# Make sure Day is a factor
boxplot_data <- assay_sulfate2 %>%
  filter(!is.na(Dose)) %>%
  mutate(
    Dose = as.factor(Dose),
    Day = as.factor(Day)
  )

# Calculate mean and SD for each Dose-Day group
stats_labels <- boxplot_data %>%
  group_by(Dose, Day) %>%
  summarise(
    mean_SO4 = mean(SO4, na.rm = TRUE),
    sd_SO4 = sd(SO4, na.rm = TRUE),
    label = paste0(round(mean_SO4, 1), " ± ", round(sd_SO4, 1)),
    y_pos = max(SO4, na.rm = TRUE) + 1,
    .groups = "drop"
  )

# Plot with grouped boxplots and summary labels
assay2sulfatewlabels <- ggplot(boxplot_data, aes(x = Dose, y = SO4, fill = Day)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_jitter(
    aes(color = Day),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
    size = 2,
    alpha = 0.6
  ) +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label, group = Day),
    position = position_dodge(width = 0.75),
    vjust = 0,
    fontface = "bold",
    size = 3.5,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Assay 2 Sulfate Concentrations by Dose and Day",
    x = "Dose",
    y = "Sulfate Concentration (SO4)",
    fill = "Day",
    color = "Day"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/assay2sulfatewlabels.png",
  plot = assay2sulfatewlabels,
  width = 15,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)


