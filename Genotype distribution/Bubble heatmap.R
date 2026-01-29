# Bubble Heatmap of Relative Abundance
#
# Dependencies:
#   - ggplot2
#   - dplyr
#
# Input:
#   - "Temporal_data.csv" with columns:
#       Type                (genotype)
#       Year                (time point)
#       Relative_abundance  (numeric, > 0)
#
##########################################################################
library(ggplot2)
library(dplyr)

# =========================================================
# 1. Read data
# =========================================================
# Disable automatic conversion of strings to factors
df <- read.csv("Temporal_data.csv", stringsAsFactors = FALSE)

# Remove zero-abundance entries
df2 <- df %>%
  filter(Relative_abundance > 0)

# Preserve original ordering of Type and Year
df2$Type <- factor(df2$Type, levels = unique(df2$Type))
df2$Year <- factor(df2$Year, levels = unique(df$Year))

# =========================================================
# 2. Create background bands for row separation
# =========================================================
# Generate alternating background rectangles for each Type
bg <- data.frame(
  Type = levels(df2$Type),
  y_min = seq_along(levels(df2$Type)) - 0.5,
  y_max = seq_along(levels(df2$Type)) + 0.5
)

# Assign background color to every second row
bg <- bg %>%
  mutate(fill_color = ifelse(row_number() %% 2 == 0, "grey95", NA))

# =========================================================
# 3. Build bubble heatmap
# =========================================================
ggplot() +
  # Alternating background stripes
  geom_rect(
    data = bg %>% filter(!is.na(fill_color)),
    aes(xmin = -Inf, xmax = Inf, ymin = y_min, ymax = y_max),
    fill = "grey95",
    inherit.aes = FALSE
  ) +
  
  # Bubble points encoding relative abundance
  geom_point(
    data = df2,
    aes(
      x = Year,
      y = Type,
      size = Relative_abundance,
      fill = Relative_abundance   # ★ key: color encodes abundance
    ),
    shape = 21,
    color = "black",
    alpha = 0.85
  ) +
  
  # Size scale for abundance
  scale_size_continuous(
    limits = c(0, max(df2$Relative_abundance)),
    breaks = seq(0.1, max(df2$Relative_abundance), by = 0.2),
    range  = c(1.5, 10)
  ) +
  
  # Fill color gradient (red scale)
  # Alternative blue scale is provided in comments
  scale_fill_gradient(
    low  = "#FFF5F5",   # light red (low values)
    high = "#C53030",   # dark red (high values)
    name = "Relative abundance"
  ) +
  
  # Place x-axis at the top
  scale_x_discrete(position = "top") +
  
  # Theme customization
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(
      face = "bold",
      angle = 90,
      hjust = 0,
      vjust = 0.5,
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(color = "black")
  ) +
  
  # Labels
  labs(
    x = "Year",
    y = NULL,
    size = "Relative abundance"
  )
