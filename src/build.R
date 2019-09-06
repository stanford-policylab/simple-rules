#!/usr/bin/env Rscript
# Build models and generate predictions
# Each model is saved, and can be loaded (without re-training) by setting
# use_saved_models = TRUE (default)
source("build_deps.R")  # loads all build dependencies and subroutines

# Initial setup (load list of data directories and set params) ------------
data_dirs <- list.dirs(DATA_DIR, full.names = FALSE, recursive = FALSE)
data_dirs <- data_dirs[!data_dirs %in% skip_dirs]

# Hyper-parameters
MAX_K <- 10
Ms <- c(1:5, 10)  # coefs rounded to use M integers
N_BINS <- 3  # Number of equal-sized bins for discretization

# Loop datasets -----------------------------------------------------------
# Run models in two phases:
#   Phase 1: Fit (and save) models and generate predictions on clean data
#   Phase 2: Use saved models to generate predictions for perturbed data
for (dataname in data_dirs) {  # Phase 1: Fit and save clean models/preds
  cat(sprintf("Fit models for [%s] at %s\n", dataname,
              format(Sys.time(), "%H:%M")))
  sptm <- proc.time()["elapsed"]  # Time each dataset

  clean_data <- read_rds(getCleanDataPath(dataname))

  fold_id <- "fold_idx__"
  nfolds <- max(clean_data[fold_id])

  scores <- clean_data %>%
    select_(.dots = c("label", fold_id, "example_id"))
  data <- clean_data %>%
    select(-starts_with("fold_idx"), -example_id)

  numeric_cn <- names(data[, unlist(lapply(data, is.numeric))])
  ordinal_cn <- names(data[, unlist(lapply(data, is.ordered))])

  # Add non-ordered version of ordered columns
  data[, paste0(ordinal_cn, "_ordered__")] <- data[, ordinal_cn]
  data <-
    data %>%
    mutate_at(ordinal_cn, factor, ordered=FALSE)

  # Fit and save models for each fold
  pred_df <- foreach(fold = 1:nfolds,
                     .combine = bind_rows,
                     .multicombine = TRUE) %dopar% {
    cat(sprintf("Fit models for fold %d/%d [%s]\n", fold, nfolds, dataname))

    features <- colnames(data %>% select(-label))
    model_formula <- reformulate(features, response = "label")

    nonord_features <- colnames(data %>%
                                select(-label, -ends_with("_ordered__")))
    nonord_model_formula <- reformulate(nonord_features, response = "label")

    train_ind <- scores[fold_id] != fold
    test_ind <- scores[fold_id] == fold

    train_df <- data[train_ind, ]
    train_labels <- train_df$label

    test_df <- data[test_ind, ]
    test_labels <- test_df$label
    test_id <- scores$example_id[test_ind]

    # Standardize numeric columns with train statistics
    if (length(numeric_cn) > 0) {  # Non-zero numeric columns
      trainStd <- scale(train_df[, numeric_cn])
      trainMeans <- attr(trainStd, "scaled:center")
      trainSD <- attr(trainStd, "scaled:scale")
      if (length(numeric_cn) == 1) {
        trainStd <- as.numeric(trainStd)
      }
      train_df[, numeric_cn] <- trainStd
      if (length(trainSD) > 1) {  # Index trainSD
        for (col in numeric_cn) {
          if (trainSD[col] == 0)  {  # Happens in ecoli data ...
            train_df[, col] <- 0
            test_df[, col] <- test_df[, col] - trainMeans[col]
          } else {
            test_df[, col] <-
              (test_df[, col] - trainMeans[col]) / trainSD[col]
          }
        }
      } else {  # There's only one trainSD/trainMeans
        if (trainSD == 0)  {  # Happens in ecoli data ...
          train_df[, numeric_cn] <- 0
          test_df[, numeric_cn] <- test_df[, numeric_cn] - trainMeans
        } else {
          test_df[, numeric_cn] <-
            (test_df[, numeric_cn] - trainMeans) / trainSD
        }
      }  # END: if length(numeric_cn > 1)
    }  # END: if length(numeric_cn > 0)

    # Build model matrix with standardized data
    train_mm <- model.matrix(model_formula, data = train_df)
    test_mm <- model.matrix(model_formula, data = test_df)

    pred_glm_df <- fit_glm(model_formula, train_df, fold, k = Inf) %>%
      pred_glm(test_df, test_labels, test_id, dataname, fold, k = Inf)

    pred_lasso_df <- fit_lasso(train_mm, train_labels, fold, k = Inf) %>%
      pred_lasso(test_mm, test_labels, test_id, dataname, fold, k = Inf)

    pred_rf_df <- fit_rf(train_mm, train_labels, fold, k = Inf) %>%
      pred_rf(test_mm, test_labels, test_id, dataname, fold, k = Inf)

    # Cast ordinal columns as regular factors for simple rules
    train_nonord_df <-
      train_df %>%
      select(-ends_with("_ordered__"))

    test_nonord_df <-
      test_df %>%
      select(-ends_with("_ordered__"))

    # Build model matrix with standardized data
    train_nonord_mm <- model.matrix(nonord_model_formula,
                                    data = train_nonord_df)
    test_nonord_mm <- model.matrix(nonord_model_formula,
                                   data = test_nonord_df)

    # Models using k features (as-is/not discretized)
    k_params <- get_subsets(train_nonord_mm, train_labels)
    subsets <- k_params$subsets
    mm_assign <- k_params$ass
    k_pred_df <- fit_k_features(mm_assign, subsets, features,
                                train_nonord_df, test_nonord_df,
                                train_labels, test_labels,
                                test_id, fold, MAX_K)

    # Models using k, all discretize, features
    disc_train_df <- discretize(train_nonord_df, N_BINS)
    disc_test_df <- discretize(test_nonord_df, N_BINS)
    disc_feats <- colnames(disc_train_df %>% select(-label))
    disc_formula <- make_formula("label", disc_feats)
    disc_mm <- model.matrix(disc_formula, data = disc_train_df)

    disc_params <- get_subsets(disc_mm, train_labels)
    disc_subsets <- disc_params$subsets
    disc_assign <- disc_params$ass
    k_disc_pred_df <- fit_k_features(disc_assign, disc_subsets, disc_feats,
                                     disc_train_df, disc_test_df,
                                     train_labels, test_labels, test_id,
                                     fold, MAX_K, type = "disc")

    bind_rows(pred_glm_df, pred_lasso_df, pred_rf_df, k_pred_df, k_disc_pred_df)
  }  # END: foreach(fold = 1:nfolds)

  # Write prediction scores to file
  write_rds(pred_df, path = getPredPath(dataname))

  etm <- as.numeric(proc.time()["elapsed"] - sptm)
  cat(sprintf("finish [%s] (%0.1fs)\n", dataname, etm))
}
