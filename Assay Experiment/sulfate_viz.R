#analyzing sulfate data 

library(readxl)
library(dplyr)

assay_sulfate <- read_excel("Assay Experiment/Round1_assay_sulfate_data.xlsx")

assay_sulfate <- assay_sulfate %>%
  mutate(SO4 = as.numeric(SO4)) %>%
  filter(!is.na(SO4)) %>%
  mutate(SO4 = round(SO4, 0))%>%
  mutate(Dose = ifelse(Flask %in% c("init1", "init2"), "init", Dose))



library(ggplot2)

####visualize sulfate####

# Prepare data
boxplot_data <- assay_sulfate %>%
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

# Plot with label text
ggplot(boxplot_data, aes(x = Dose, y = SO4)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6, color = "darkblue") +
  geom_text(
    data = stats_labels,
    aes(x = Dose, y = y_pos, label = label),
    inherit.aes = FALSE,
    vjust = 0,
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Sulfate Concentrations by Dose",
    x = "Dose",
    y = "Sulfate Concentration (SO4)"
  ) +
  theme_minimal()
