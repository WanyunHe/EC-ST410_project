# Stream Plot for ST410 Subclones
#
# Purpose: Generate a temporal stream plot showing relative prevalence of ST410 subclones over years using ggstream.
#
# Dependencies:
#   - ggplot2
#   - ggstream
#   - dplyr
#   - ggrepel
#   - readr
#   - scales
#
# Input:
#   - "ST410_subclones.csv": CSV file containing columns:
#       Year       (numeric or factor)
#       Clone      (subclone identifier)
#       Prevalence (numeric, relative abundance)
#
# Load libraries
library(ggplot2)
library(ggstream)
library(dplyr)
library(ggrepel)
library(readr)
library(scales)

# Load subclone data
df <- read_csv("ST410_subclones.csv")

# =====================================================
# 1. Prepare sorting variable to ensure correct stacking
# =====================================================

# Define fixed clone order (can be customized)
clone_order <- c("A/H53", "B1/H24", "B2/H24R", "B3/H24Rx_1", 
                 "B3/H24Rx_2", "B4/H24RxC", "B5/H24RxC")

# Assign numeric sort value to each clone
clone_sort_values <- setNames(1:length(clone_order), clone_order)

# Prepare dataframe with fixed ordering
df_prepared <- df %>%
  mutate(
    Year = factor(Year, levels = unique(Year)),      # Keep year order
    Clone = factor(Clone, levels = clone_order),    # Set clone factor order
    sort_value = clone_sort_values[as.character(Clone)]  # Sorting variable
  ) %>%
  arrange(Year, sort_value)  # Arrange by year and clone order

# =====================================================
# 2. Calculate cumulative prevalence for stacking
# =====================================================
df_final <- df_prepared %>%
  group_by(Year) %>%
  arrange(sort_value) %>%
  mutate(
    cumsum_prevalence = cumsum(Prevalence),  # cumulative sum per year
    y_start = lag(cumsum_prevalence, default = 0)  # start point for each stream
  ) %>%
  ungroup()

# =====================================================
# 3. Prepare labels for the last occurrence of each clone
# =====================================================
label_df <- df_final %>%
  group_by(Clone) %>%
  slice_tail(n = 1)  # take last year for labeling

# =====================================================
# 4. Define color palette
# =====================================================
clone_cols <- c(
  "A/H53"      = "#9B7FCB",
  "B1/H24"     = "#95765C",
  "B2/H24R"    = "#B2BC8A",
  "B3/H24Rx_1" = "#ffe699",
  "B3/H24Rx_2" = "#fecc66",
  "B4/H24RxC"  = "#76D5FF",
  "B5/H24RxC"  = "#FF7E79"
)

# =====================================================
# 5. Generate the stream plot
# =====================================================
try({
  p <- ggplot(df_final, aes(x = Year, y = Prevalence, fill = Clone)) +
    geom_stream(
      type = "proportional",   # proportional stacking
      bw = 0.65,               # smoothness parameter
      extra_span = 0.15,       # additional smoothing
      color = "white",          # border color for streams
      linewidth = 0.15,         # stream border width
      sorting = "onset"         # sorting by first appearance
    ) +
    scale_fill_manual(values = clone_cols) +
    scale_y_continuous(labels = percent) +
    labs(
      x = "Year",
      y = "Relative prevalence",
      title = "Temporal dynamics of ST410 subclones"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_text_repel(
      data = label_df,
      aes(label = Clone),
      nudge_x = 0.5,
      direction = "y",
      size = 4,
      segment.color = NA
    )
  
  print(p)
})
