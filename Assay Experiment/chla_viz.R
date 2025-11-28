#visualizing chlorophyll

df <- read.csv("Assay Experiment/csvs/All_Assays_chla.csv")


#Pheo is a degradation product of chlorophyll
df1 <- df |>
  select(Flask, Dose, Chla_ugL, Pheo_ugL, Assay_num) |>
  mutate(Dose = ifelse(Flask %in% c("init1", "init2"), "init", Dose)) |>
  filter(Assay_num == 1) |>
  mutate(Flask = as.numeric(Flask))

df2 <- df|>
  select(Flask, Dose, Chla_ugL, Pheo_ugL, Assay_num) |>
  mutate(Dose = ifelse(Flask %in% c("init1", "init2"), "init", Dose)) |>
  filter(Assay_num == 2) |>
  mutate(Flask = as.numeric(Flask))

#-----Assay 1----####

# Prepare data
boxplot_data <- df1 %>%
  mutate(Dose = as.factor(Dose))

# Calculate mean and SD for Chla_ugL for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_Chla_ugL = mean(Chla_ugL, na.rm = TRUE),
    sd_Chla_ugL = sd(Chla_ugL, na.rm = TRUE),
    label = paste0(round(mean_Chla_ugL, 1), " ± ", round(sd_Chla_ugL, 1)),
    y_pos = max(Chla_ugL, na.rm = TRUE) + 1  # label placed above max point
  )

# Plot Chla_ugL boxplot with text labels
chla_assay1 <- ggplot(boxplot_data, aes(x = Dose, y = Chla_ugL)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "darkgreen") +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Assay 1 Chlorophyll-a Concentrations by Dose",
    x = "Dose",
    y = "Chlorophyll-a (µg/L)"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/chla_assay1_boxplot.png",
  plot = chla_assay1,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)


# Calculate mean and SD for Pheo_ugL for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_Pheo_ugL = mean(Pheo_ugL, na.rm = TRUE),
    sd_Pheo_ugL = sd(Pheo_ugL, na.rm = TRUE),
    label = paste0(round(mean_Pheo_ugL, 1), " ± ", round(sd_Pheo_ugL, 1)),
    y_pos = max(Pheo_ugL, na.rm = TRUE) + 1  # label placed above max point
  )

# Plot Pheo_ugL boxplot with text labels
PheophytinAssay1 <- ggplot(boxplot_data, aes(x = Dose, y = Pheo_ugL)) +
  geom_boxplot(fill = "plum", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "purple4") +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Assay1 Pheophytin Concentrations by Dose",
    x = "Dose",
    y = "Pheophytin (µg/L)"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/PheophytinAssay1.png",
  plot = PheophytinAssay1,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)

#-----Assay 2-----####
# Prepare data
boxplot_data <- df2 %>%
  mutate(Dose = as.factor(Dose))

# Calculate mean and SD for Chla_ugL for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_Chla_ugL = mean(Chla_ugL, na.rm = TRUE),
    sd_Chla_ugL = sd(Chla_ugL, na.rm = TRUE),
    label = paste0(round(mean_Chla_ugL, 1), " ± ", round(sd_Chla_ugL, 1)),
    y_pos = max(Chla_ugL, na.rm = TRUE) + 1  # label placed above max point
  )

# Plot Chla_ugL boxplot with text labels
chla_assay2 <- ggplot(boxplot_data, aes(x = Dose, y = Chla_ugL)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "darkgreen") +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Assay 2 Chlorophyll-a Concentrations by Dose",
    x = "Dose",
    y = "Chlorophyll-a (µg/L)"
  ) +
  theme_minimal()

ggsave(
  filename = "Assay Experiment/Figs/chla_assay2_boxplot.png",
  plot = chla_assay2,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)


# Calculate mean and SD for Pheo_ugL for labels
stats_labels <- boxplot_data %>%
  group_by(Dose) %>%
  summarise(
    mean_Pheo_ugL = mean(Pheo_ugL, na.rm = TRUE),
    sd_Pheo_ugL = sd(Pheo_ugL, na.rm = TRUE),
    label = paste0(round(mean_Pheo_ugL, 1), " ± ", round(sd_Pheo_ugL, 1)),
    y_pos = max(Pheo_ugL, na.rm = TRUE) + 1  # label placed above max point
  )

# Plot Pheo_ugL boxplot with text labels
PheophytinAssay2 <- ggplot(boxplot_data, aes(x = Dose, y = Pheo_ugL)) +
  geom_boxplot(fill = "plum", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "purple4") +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Assay 2 Pheophytin Concentrations by Dose",
    x = "Dose",
    y = "Pheophytin (µg/L)"
  ) +
  theme_minimal()


ggsave(
  filename = "Assay Experiment/Figs/PheophytinAssay2.png",
  plot = PheophytinAssay2,
  width = 6,      # inches
  height = 6,      # inches
  dpi = 400,       # publication-quality
  bg = "white"
)

