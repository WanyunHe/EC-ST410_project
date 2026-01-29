# Linear Regression Analysis of Temporal Trends
#
# Purpose: Perform linear regression analysis to assess temporal trends in relative abundance across different types. The script normalizes abundance values (0–1), fits per-type linear models, computes statistical metrics, and visualizes results with faceted plots.
#
# Dependencies:
#   - tidyverse
#   - ggpubr
#
# Input:
#   - "Linear_data.csv" with columns:
#       Type                (categorical variable)
#       Year                (numeric or character)
#       Relative_abundance  (numeric)
#
# Output:
#   - Faceted regression plot (optional export)
#   - "Statistical_results.csv"
#   - "Cleaned_normalized_data.csv"
#
# =========================================================
# 1. Load required packages
# =========================================================
library(tidyverse)
library(ggpubr)

# =========================================================
# 2. Read and clean data
# =========================================================
# Read raw data
data_raw <- read.csv("Linear_data.csv", header = TRUE)

# Data cleaning pipeline
data_clean <- data_raw %>%
  # Keep only the first three columns
  select(1:3) %>%
  # Rename columns for clarity
  rename(
    Type = 1,
    Year = 2,
    Relative_abundance = 3
  ) %>%
  # Remove rows with missing key values
  filter(!is.na(Type) & !is.na(Year)) %>%
  # Convert data types
  mutate(
    Year = as.numeric(Year),
    Relative_abundance = as.numeric(Relative_abundance)
  ) %>%
  # Remove rows with missing abundance values
  filter(!is.na(Relative_abundance))

# Inspect cleaned data
cat("Data cleaning completed.\n")
cat(paste("Number of rows:", nrow(data_clean), "\n"))
cat(paste("Number of types:", length(unique(data_clean$Type)), "\n"))
cat("Preview of cleaned data:\n")
print(head(data_clean, 10))

# Preserve the original order of Type as it appears in the data
type_order <- unique(data_clean$Type)

data_clean <- data_clean %>%
  mutate(Type = factor(Type, levels = type_order))

# =========================================================
# 3. Normalize abundance values (0–1 scaling)
# =========================================================
# Normalize relative abundance within each type
data_norm <- data_clean %>%
  group_by(Type) %>%
  mutate(
    # Normalization formula: (x - min) / (max - min)
    Abundance_norm = (Relative_abundance - min(Relative_abundance)) /
      (max(Relative_abundance) - min(Relative_abundance))
  ) %>%
  ungroup()

# Verify normalization results
cat("\nChecking normalization results:\n")
for (tp in unique(data_norm$Type)) {
  subset_data <- data_norm[data_norm$Type == tp, ]
  cat(
    paste(
      tp, ": original range [",
      round(min(subset_data$Relative_abundance), 3), ", ",
      round(max(subset_data$Relative_abundance), 3),
      "], normalized range [",
      round(min(subset_data$Abundance_norm), 3), ", ",
      round(max(subset_data$Abundance_norm), 3), "]\n",
      sep = ""
    )
  )
}

# =========================================================
# 4. Calculate statistical metrics
# =========================================================
stats <- data_norm %>%
  group_by(Type) %>%
  summarise(
    # Basic statistics
    n_points = n(),
    mean_abundance = mean(Relative_abundance),
    
    # Linear regression on normalized abundance
    slope = lm(Abundance_norm ~ Year)$coefficients[2],
    p_value = summary(lm(Abundance_norm ~ Year))$coefficients[2, 4],
    R2 = summary(lm(Abundance_norm ~ Year))$r.squared,
    
    # Pearson correlation
    r = cor(Abundance_norm, Year, method = "pearson"),
    
    # Label positioning for plots
    label_x = median(Year),
    label_y = max(Abundance_norm) * 0.85,
    
    .groups = "drop"
  ) %>%
  # Format statistical labels
  mutate(
    p_label = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01 ~ "p < 0.01",
      p_value < 0.05 ~ sprintf("p < %.3f", p_value),
      TRUE ~ sprintf("p = %.3f", p_value)
    ),
    R2_label = sprintf("R² = %.3f", R2),
    r_label = sprintf("r = %.3f", r),
    stat_label = paste(p_label, R2_label, r_label, sep = "\n")
  )

# Display statistical summary
print(stats[, c("Type", "n_points", "p_label", "R2_label", "r_label")])

# =========================================================
# 5. Visualization
# =========================================================
p <- ggplot(data_norm, aes(x = Year, y = Abundance_norm)) +
  # Scatter points
  geom_point(color = "#377EB8", size = 3, alpha = 0.7) +
  
  # Linear regression line with confidence interval
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "#E41A1C",
    fill = "#FF9999",
    alpha = 0.3,
    linewidth = 1.0,
    se = TRUE
  ) +
  
  # Facet by type
  facet_wrap(~ Type, ncol = 3, scales = "free_y") +
  
  # Add statistical annotations
  geom_text(
    data = stats,
    aes(x = label_x, y = label_y, label = stat_label),
    size = 3.5,
    hjust = 1,
    vjust = 0.5,
    color = "black"
  ) +
  
  # Theme customization
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(color = "black", linewidth = 0.5),
    strip.background = element_rect(fill = "gray95", color = "gray50"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  ) +
  
  # Labels
  labs(
    x = "Year",
    y = "Relative abundance (normalized)",
    title = "Temporal trends",
    subtitle = "Linear regression with Pearson correlation"
  ) +
  
  # Axis settings
  scale_x_continuous(
    breaks = seq(2012, 2024, by = 4),
    limits = c(2011.5, 2024.5)
  ) +
  ylim(0, 1)

# Display plot
print(p)

# =========================================================
# 6. Save results
# =========================================================
# Save statistical results
write.csv(stats, "Statistical_results.csv", row.names = FALSE)

# Save cleaned and normalized data
write.csv(data_norm, "Cleaned_normalized_data.csv", row.names = FALSE)
