# Population Structure Inference using hierBAPS
#
# Purpose: Infer hierarchical population structure from a core genome alignment using the hierBAPS algorithm. This script loads a FASTA alignment, runs hierBAPS clustering, and exports cluster assignments.
#
# Dependencies:
#   - rhierbaps
#
# Input:
#   - "clean.core.aln": FASTA-format core genome alignment
#
# Output:
#   - "hierbaps_partition.csv": cluster assignments for each sequence
#

library(rhierbaps)

# =========================================================
# 1. Load sequence alignment
# =========================================================
# Path to FASTA alignment file
fasta.file.name <- "clean.core.aln"

# Load FASTA file and convert to SNP matrix
# Each row represents a sequence; columns represent SNP positions
snp.matrix <- load_fasta(fasta.file.name)

# =========================================================
# 2. Benchmark runtime (optional)
# =========================================================
# Measure how long hierBAPS takes to run on this dataset
# This step is useful for estimating computational cost
system.time(
  hierBAPS(
    snp.matrix,
    max.depth = 2,     # Number of hierarchical levels
    n.pops = 20,       # Upper bound on number of populations
    quiet = TRUE       # Suppress console output
  )
)

# =========================================================
# 3. Run hierBAPS clustering
# =========================================================
# Option 1: Standard run (fixed number of iterations)
# hb.results <- hierBAPS(
#   snp.matrix,
#   max.depth = 2,
#   n.pops = 20,
#   quiet = TRUE
# )

# Option 2: Run until convergence to a local optimum
# n.extra.rounds = Inf allows additional optimization rounds
hb.results <- hierBAPS(
  snp.matrix,
  max.depth = 2,          # Number of hierarchical levels
  n.pops = 20,            # Maximum number of clusters
  n.extra.rounds = Inf,   # Continue until convergence
  quiet = TRUE
)

# =========================================================
# 4. Inspect and export results
# =========================================================
# Preview cluster assignments
head(hb.results$partition.df)

# Save partition results to CSV
# The output contains sequence IDs and their assigned clusters
write.csv(
  hb.results$partition.df,
  file = file.path("hierbaps_partition.csv"),
  col.names = TRUE,
  row.names = FALSE
)
