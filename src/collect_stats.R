#!/usr/bin/env Rscript
source("utils.R")

N_BINS <- 3

discretize <- function(df, n_bins) {
  # Discretize numerical columns in df to n_bins of equal size

  # Args:
  #   df: the data.frame to discretize
  #   n_bins: number of bins for each numeric column

  # Return:
  #   data.frame with all categorical columns
  numeric_cols <- names(df[, unlist(lapply(df, is.numeric))])

  if (length(numeric_cols) > 0) {  # Non-zero numeric columns
    disc_df <- df %>%
      mutate_(.dots = setNames(paste("dplyr::ntile(", numeric_cols, ",",
                                     n_bins, ")"),
                               numeric_cols))

    for (cn in numeric_cols) {
      disc_df[[cn]] <- as.factor(disc_df[[cn]])
    }
  } else {
    disc_df <- df
  }

  disc_df
}

# Initial setup (load list of data directories)
data_dirs <- list.dirs(DATA_DIR, full.names = FALSE, recursive = FALSE)
data_dirs <- data_dirs[!data_dirs %in% skip_dirs]

index <- 1
n_feats <- c()
for (dataname in data_dirs) {

  filename <- paste0(dataname, ".csv.gz")
  rawdata <-
    file.path(DATA_DIR, "raw", filename) %>%
    read.csv(strip.white=TRUE, header=FALSE)

  clean_data <-
    getCleanDataPath(dataname) %>%
    read_rds()

  uci_rows <- dim(rawdata)[1]
  uci_cols <- dim(rawdata)[2] - 1 # remove label column

  srr_df <- clean_data %>%
    select(-starts_with("fold_idx__"), -example_id) %>%
    discretize(N_BINS)

  clean_mm <- model.matrix(label ~ ., srr_df)
  heur_rows <- dim(clean_mm)[1]
  heur_cols <- dim(clean_mm)[2]

  cont_cols <- clean_data %>%
    select(-starts_with("fold_idx__"), -example_id) %>%
    select_if(is.numeric) %>%
    ncol()

  cat(  # dataname & uci_rows & uci_cols & nrows & cont_cols & nvariables & p_pos \\
      sprintf('%d.\\ %s & %s & %s & %s & %s & %s & %.0f \\\\\n',
              index, dataname, uci_rows, uci_cols, heur_rows, cont_cols,
              heur_cols, 100*mean(clean_data$label)))

    index <- index + 1
    n_feats <- c(n_feats, uci_cols)
}

message(sprintf("Number of features in [%d, %d] with average %.2f",
                min(n_feats), max(n_feats), mean(n_feats)))
