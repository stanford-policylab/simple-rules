#!/usr/bin/env Rscript
source("utils.R")

library(ROCR)
library(zoo)

set.seed(666)  # set random seed for reproducibility
registerDoMC(cores = 22)

# Initial setup ----------------------------------------------------------
data_dirs <- list.dirs(DATA_DIR, full.names = FALSE, recursive = FALSE)
data_dirs <- data_dirs[!data_dirs %in% skip_dirs]

MAX_K <- 15

skip_existing <- FALSE

# Helper functions --------------------------------------------------------
compute_perf <- function(df){
  # Compute AUC
  pred <- prediction(df$pred, df$labels)
  auc <- performance(pred, "auc")
  auc <- unlist(slot(auc, "y.values"))

  # Compute accuracy
  labels <- df$labels
  if (is.factor(labels)) {
    labels <- as.numeric(levels(labels)[as.numeric(labels)])
  }
  acc <- mean((df$pred > 0.5) == labels)

  tibble(auc = auc,
         acc = acc,
         min_nz = min(df$nzero),
         mean_nz = mean(df$nzero),
         med_nz = median(df$nzero),
         max_nz = max(df$nzero))
}

fill_numfeats <- function(df, max_numfeats) {
  # Fill in missing num_feats
  # take a group of heuristic auc with same M but different nzero,
  # and return a new data.frame where nzero is filled from 1:max_numfeats,
  # with missing values pulled from the maximum num_feats lower than that
  # of the missing nzero
  tibble(num_feats = 1:max_numfeats) %>%
    left_join(df, by = "num_feats") %>%
    select(num_feats, nzero, auc, acc)  %>%
    # na.locf from zoo library; fills leading NAs with most "recent" values
    do(na.locf(.))
}

# Loop datasets -----------------------------------------------------------
preds_all <- foreach(dataname = data_dirs,
                     .combine = bind_rows,
                     .multicombine = TRUE) %dopar% {
  # Aggregate predictions across all datasets
  # While the predictions are loaded, dataset-specific performance measures are
  # computed as a side-effect
  cat(sprintf("[%s] start eval (%s)\n",
              dataname, format(Sys.time(), "%H:%M:%S")))
  ptm <- proc.time()["elapsed"]  # Time each dataset

  pred_filename <- getPredPath(dataname)
  # This is what's going to be returned and aggregated at the foreach loop
  pred_df <- as_tibble(read_from_path(pred_filename, "predictions"))

  # ... but since we already have the data loaded in memory, we might as well
  #     compute and save performance metrics
  perf_df <- pred_df %>%
    group_by(dataset, model_type, round_lvl, num_feats) %>%
    do(compute_perf(.)) %>%
    # nzero for a fold is defined as the max nonzero used within each fold
    mutate(nzero = ifelse(num_feats == Inf, Inf, max_nz)) %>%
    ungroup()

  simple_df <- perf_df %>%
    filter(num_feats != Inf) %>%
    group_by(dataset, model_type, round_lvl) %>%
    do(fill_numfeats(., MAX_K)) %>%
    ungroup()

  complex_df <- perf_df %>%
    filter(num_feats == Inf) %>%
    select(!!colnames(simple_df))

  perf_full <- bind_rows(simple_df, complex_df)

  etm <- as.numeric(proc.time()["elapsed"] - ptm)
  cat(sprintf("finish [%s] %0.1fs\n", dataname, etm))

  perf_filename = getPerfPath(dataname)
  write_to_path(perf_full, perf_filename, "performance metrics")

  pred_df
}  # END: foreach(data_dirs) %:% foreach(run) %dopar%

write_to_path(preds_all, PREDS_PATH, "aggregated predictions")

perf_all <- foreach (dataname = data_dirs,
                     .combine = bind_rows,
                     .multicombine = TRUE) %dopar% {
  # Aggregate performance metrics across datasets
  filename = getPerfPath(dataname)
  read_from_path(filename, "local perf metrics")
}

write_to_path(perf_all, PERFS_PATH, "aggregated perf metrics")
