# Dot Heatmap of Ecological Distribution
#
# Purpose: Generate a dot-based heatmap to visualize the ecological distribution of different genotypes across host sources. Circle color represents percentage values, while circle size is kept constant for clarity.
#
# Dependencies:
#   - tidyverse
#   - ggplot2
#   - viridis
#   - RColorBrewer
#   - scales
#
# Input:
#   - "Heatmap_data.csv" with columns:
#       Type        (genotype / clone category)
#       Source      (host or ecological source)
#       Percentage  (percentage value, 0–100)
#
# =========================================================
# 1. Load required packages
# =========================================================
required_packages <- c("tidyverse", "ggplot2", "viridis", "RColorBrewer", "scales")
new_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(new_packages)) {
  install.packages(new_packages)
}

library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)

# =========================================================
# 2. Data preparation
# =========================================================
# Read input data
data <- read.csv("Heatmap_data.csv", stringsAsFactors = FALSE)

# Process data (retain Total category)
heatmap_data <- data %>%
  mutate(
    # Percentage column is already scaled from 0 to 100
    Percent = Percentage,
    
    # Define genotype order (Total placed first)
    Type = factor(
      Type,
      levels = c(
        "Total", "A/H53", "B1/H24", "B2/H24R",
        "B3/H24Rx", "B4/H24RxC", "B5/H24RxC",
        "B3/H24Rx_1", "B3/H24Rx_2"
      )
    ),
    
    # Convert source to factor (ordering defined below)
    Source = factor(Source)
  )

# =========================================================
# 3. Reorder sources based on Total percentage
# =========================================================
# Determine source order by descending Total percentage
total_order <- heatmap_data %>%
  filter(Type == "Total") %>%
  arrange(desc(Percent)) %>%
  pull(Source)

# Apply source ordering
heatmap_data$Source <- factor(heatmap_data$Source, levels = total_order)

# Inspect data range and structure
cat("Data dimensions:", dim(heatmap_data), "\n")
cat("Genotype levels:", levels(heatmap_data$Type), "\n")
cat("Number of sources:", length(levels(heatmap_data$Source)), "\n")
cat(
  "Percentage range:",
  sprintf(
    "%.2f%% to %.2f%%",
    min(heatmap_data$Percent, na.rm = TRUE),
    max(heatmap_data$Percent, na.rm = TRUE)
  ),
  "\n\n"
)

# =========================================================
# 4. Create dot heatmap
# =========================================================
p1 <- ggplot(heatmap_data, aes(x = Type, y = Source)) +
  # Draw circles with uniform size; color encodes percentage
  geom_point(
    aes(color = Percent),
    size = 9.5,
    shape = 19,
    alpha = 0.85,
    stroke = 0
  ) +
  
  # Add numeric labels inside circles
  geom_text(
    aes(
      label = ifelse(
        Percent >= 1,
        sprintf("%.1f", Percent),
        ifelse(
          Percent >= 0.1,
          sprintf("%.1f", Percent),
          ifelse(Percent > 0, "<0.1", "0.0")
        )
      )
    ),
    color = "black",
    size = 3.5
  ) +
  
  # Color scale using viridis (magma palette)
  scale_color_viridis(
    option = "magma",
    name = "(%)",
    limits = c(0, 100),
    direction = -1,   # High values brighter, low values darker
    begin = 0.1,
    end = 0.95,
    na.value = "black"
  ) +
  
  # Theme and layout
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      face = "bold",
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 11,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      color = "black"
    ),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.8, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 11, vjust = 0.8),
    legend.text = element_text(size = 10, color = "black"),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "black",
      margin = margin(b = 10)
    ),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Labels
  labs(
    title = "Ecological distribution"
  )

# Display plot
print(p1)
