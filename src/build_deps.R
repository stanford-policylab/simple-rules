#!/usr/bin/env Rscript
# Build models and generate predictions
# Each model is saved, and can be loaded (without re-training) by setting
# use_saved_models to TRUE in utils.R
source("utils.R")  # loads plyr, tidyverse, and doMC

library(leaps, warn.conflicts = FALSE, quietly = TRUE)
library(glmnet, warn.conflicts = FALSE, quietly = TRUE)
library(randomForest, warn.conflicts = FALSE, quietly = TRUE)

set.seed(666)  # set random seed for reproducibility

# Subroutines as functions ------------------------------------------------
fit_glm <- function (f, df, fold, k, type = NULL, use_saved = use_saved_models) {
  modelname <- getModelName("glm", type)
  model_path <- getModelPath(dataname, fold, k = k, modelname = modelname)

  if (use_saved) {
    if (file.exists(model_path)) {
      glm_model <- read_model(model_path)
    }
  }

  if (!exists("glm_model")) {
    glm_model <-
      glm(f, family = "binomial", data = df)
      # glmnet(X, y, family = "binomial", lambda = 0)
    write_model(glm_model, model_path)
  }

  glm_model
}
pred_glm <- function (model, df_new, y_new, ids, dataname,
                      fold, k, type = NULL) {
  modelname <- getModelName("glm", type)

  pred <- predict(model, df_new, type = "response")
  nz <- model$rank
  setPredDf(pred, y_new, dataname, modelname, ids, fold, nz = nz,
            numfeats = k)
}

fit_lasso <- function (X, y, fold, k, type = NULL,
                       use_saved = use_saved_models) {
  modelname <- getModelName("lasso", type)
  model_path <- getModelPath(dataname, fold, k = k, modelname = modelname)

  lambda <- 1000

  if (use_saved) {
    if (file.exists(model_path)) {
      lasso_model <- read_model(model_path)
    }
  }

  if (!exists("lasso_model")) {
    lasso_model <- cv.glmnet(X, y, type.measure = "auc", alpha = 1,
                             nlambda = lambda, family = "binomial")
    write_model(lasso_model, model_path)
  }

  lasso_model
}
pred_lasso <- function (model, X_new, y_new, ids, dataname, fold, k,
                        type = NULL) {
  modelname <- getModelName("lasso", type)

  pred <- predict(model, X_new, type = "response", s = "lambda.1se")
  lambda_ind <- model$lambda == model$lambda.1se
  nz <- model$nzero[lambda_ind]
  setPredDf(pred, y_new, dataname, modelname, ids, fold, nz, numfeats = k)
}

fit_rf <- function (X, y, fold, k, type = NULL, use_saved = use_saved_models) {
  modelname <- getModelName("rf", type)
  model_path <- getModelPath(dataname, fold, k = k, modelname = modelname)

  ntrees <- 1000

  if (use_saved) {
    if (file.exists(model_path)) {
      rf_model <- read_model(model_path)
    }
  }

  if (!exists("rf_model")) {
    rf_model <- randomForest(X, factor(y), ntree = ntrees)
    write_model(rf_model, model_path)
  }
  rf_model
}
pred_rf <- function (model, X_new, y_new, ids, dataname, fold, k,
                     type = NULL) {
  modelname <- getModelName("rf", type)

  pred <- predict(model, X_new, type = "prob")[, 2]
  setPredDf(pred, y_new, dataname, modelname, ids, fold, nz = k, numfeats = k)
}

get_subsets <- function (mm, y) {
  # Given a model.matrix and labels y, return a logical matrix of subsets

  # Args:
  #   mm: model.matrix
  #   y: corresponding labels

  # Returns:
  #   list of (subsets, assign)
  real_mm_cols <- colnames(mm) != "(Intercept)"
  ass <- attr(mm, "assign")
  mm <- mm[, real_mm_cols]
  ass <- ass[real_mm_cols]

  maxfeats <- dim(mm)[2]  # number of total columns

  sink("/dev/null")  # Redirect output from leaps
  search <- regsubsets(mm, y, method = "forward", intercept = TRUE,
                       really.big = TRUE, nvmax = maxfeats)
  sink()  # END: redirect output from leaps
  subsets <- summary(search)$which

  non_intercept_cols <- colnames(subsets) != "(Intercept)"
  subsets <- subsets[, non_intercept_cols]  # Remove (Intercept) term

  e <- assert_that(dim(subsets)[2] == length(ass),
                   msg = "subsets and assign have incompatible dimensions!")

  list(subsets = subsets, ass = ass)
}

get_formulas <- function(ass, subsets, features) {
  # Get forula from subsets that use exactly n original features

  # Args:
  #   ass: the "assign" attribute from a full model.matrix
  #   subsets: a summary.regsubsets object
  #   features: character vector of original feature columns

  # Returns:
  #   data.frame with columns k and corresponding formula
  ret <- bind_rows(apply(subsets, 1, function(s) {
    # Extract original index of variables that are included
    v <- unique(ass[s])
    f <- features[v]
    nv <- length(f)
    tibble(n = nv, feats = list(f))
  })) %>%
  distinct(n, .keep_all = TRUE) %>%
  mutate(formula = map(feats, ~ make_formula("label", .x)))

  ret
}

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

fit_k_features <- function(ass, subsets, features, train_df, test_df,
                           train_labels, test_labels, ids, fold, maxk,
                           type = NULL) {
  # Get formula from subsets that use exactly n original features

  # Args:
  #   ass: the "assign" attribute from a full model.matrix
  #   subsets: a summary.regsubsets object
  #   features: character vector of original feature columns
  #   type: model type (prepended to directory) when saving model
  #   train_df, test_df: original train/test data frames
  #   train_labels, test_labels: original train/test labels
  #   maxk: maximum number of k; will use min(length(features), maxk)
  #
  # Note: Intermediate models are saved as a side effect
  #
  # Returns:
  #   data.frame of all predictions over k models
  heur_base <- getModelName("lasso", type)
  formulas <- get_formulas(ass, subsets, features)

  maxk <- min(max(formulas$n), maxk)

  foreach (k = 1:maxk, .combine = bind_rows, .multicombine = TRUE) %do% {
    ss_formula <- formulas %>%
      filter(n == k) %>%
      pull(formula) %>%
      first()

    # Update the selected subset
    train_ssmm <- model.matrix(ss_formula, data = train_df)
    test_ssmm <- model.matrix(ss_formula, data = test_df)

    # Predictions for glm/rf models using only k features
    k_pred_glm_df <- fit_glm(ss_formula, train_df, fold, k, type) %>%
      pred_glm(test_df, test_labels, ids, dataname, fold, k, type)

    k_pred_rf_df <- fit_rf(train_ssmm, train_labels, fold, k, type) %>%
      pred_rf(test_ssmm, test_labels, ids, dataname, fold, k, type)

    # Lasso model is used for SRR, so we keep a copy
    m_lasso_k <- fit_lasso(train_ssmm, train_labels, fold, k, type)
    k_pred_lasso_df <-
      pred_lasso(m_lasso_k, test_ssmm, test_labels, ids, dataname,
                 fold, k, type)

    # Use trained model to generate heuristics
    roundM_model <- as.matrix(coef(m_lasso_k, s = "lambda.min"))
    ind_non_intercept <- rownames(roundM_model) != "(Intercept)"
    nfeat <- dim(roundM_model)[1]
    roundM_model <- array(roundM_model, c(nfeat, length(Ms)))
    max_coef <- max(abs(roundM_model[ind_non_intercept, ]))
    if (max_coef > 0) {  # Sometimes all coefs are 0 (e.g., haberman)
        tmp.rounds <- roundM_model[ind_non_intercept, ]
        tmp.rounds <- (tmp.rounds/max_coef) %*% diag(Ms + 0.499)
        tmp.rounds <- round(tmp.rounds)
        tmp.rounds <- (tmp.rounds * max_coef) %*% diag(1/(Ms + 0.499))
        roundM_model[ind_non_intercept, ] <- tmp.rounds
    }


    roundM_pred <- predict3D(roundM_model, test_ssmm, type = "response")
    colnames(roundM_model) <- Ms
    colnames(roundM_pred) <- Ms

    k_pred_srr_df <- bind_rows(lapply(colnames(roundM_pred), function(x) {
      srr_nz = sum(roundM_model[ind_non_intercept, x] != 0)
      setPredDf(roundM_pred[, x], test_labels, dataname, heur_base, ids,
                fold, nz = srr_nz, round = as.numeric(x), numfeats = k)
    }))

    bind_rows(k_pred_glm_df, k_pred_lasso_df,  # k_pred_rf_df,
              k_pred_srr_df)
  }  # next k in 1:maxk
}
