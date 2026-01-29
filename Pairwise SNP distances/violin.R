# SNP Distance Distribution among Subclones
#
# Purpose: Visualize the distribution of pairwise SNP distances between B5/H24RxC and other subclones using violin plots combined with boxplots, jittered points, and annotated medians.
#
# Dependencies:
#   - ggplot2
#   - gcookbook
#
# Input:
#   - "A.txt": tab-delimited file with columns:
#       Subclones  (categorical)
#       SNPs       (numeric, pairwise SNP distances)
#

library(gcookbook)
library(ggplot2)

# =========================================================
# 1. Read and preprocess data
# =========================================================
# Load SNP distance data
df <- read.table(
  "A.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Ensure SNP distances are numeric (critical step)
df$SNPs <- as.numeric(df$SNPs)

# Remove rows with missing SNP values
df <- df[!is.na(df$SNPs), ]

# =========================================================
# 2. Define color palette
# =========================================================
# Custom color palette for subclones
colors <- c(
  "#9B7FCB", "#95765C", "#B2BC8A",
  "#FFE699", "#FECC66", "#76D5FF", "#FF7E79"
)

# =========================================================
# 3. Build the plot
# =========================================================
p <- ggplot(df, aes(x = Subclones, y = SNPs))

p +
  # Violin plot showing full distribution
  geom_violin(
    trim = TRUE,          # Trim tails beyond data range
    adjust = 2,           # Bandwidth adjustment for smoothing
    aes(fill = Subclones),
    alpha = 0.5
  ) +
  
  # Boxplot summarizing quartiles and median
  geom_boxplot(
    width = 0.1,
    fill = "grey",
    outlier.shape = NA    # Suppress outliers (shown as jittered points)
  ) +
  
  # Jittered points representing individual observations
  geom_jitter(
    aes(color = Subclones),
    width = 0.3,
    size = 0.8,
    alpha = 0.5
  ) +
  
  # Median point (white-filled circle)
  stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 2,
    fill = "white"
  ) +
  
  # Annotated median values (rounded to integers)
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.0f", after_stat(y))),
    vjust = -0.8,
    size = 4,
    color = "black"
  ) +
  
  # Manual color scales
  scale_fill_manual(
    values = colors[1:length(unique(df$Subclones))]
  ) +
  scale_color_manual(
    values = colors[1:length(unique(df$Subclones))]
  ) +
  
  # Y-axis scaling
  scale_y_continuous(
    breaks = seq(0, 500, by = 100),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  # Axis labels and title
  labs(
    x = NULL,
    y = "Pairwise SNPs",
    title = "SNP distance between A/H53 and other subclones"
  ) +
  
  # Theme customization
  theme_bw() +
  theme(
    legend.position = "none",
    aspect.ratio = 0.6,
    axis.text.x = element_text(
      size = 14,
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      color = "black"
    )
  )
