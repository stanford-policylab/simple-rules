library(leaps)
library(glmnet)
library(randomForest)

fit_glm <- function (f, df, fold, k, type = NULL, use_saved = TRUE) {
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
                       use_saved = FALSE) {
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

fit_rf <- function (X, y, fold, k, type = NULL) {
  modelname <- getModelName("rf", type)
  model_path <- getModelPath(dataname, fold, k = k, modelname = modelname)

  ntrees <- 1000

  randomForest(X, factor(y), ntree = ntrees)
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

discretize <- function(d_train, d_test, n_bins) {
  # Discretize numerical columns in train/test data to n_bins of equal size,
  # based on cutoffs derived from the training data

  # Args:
  #   d_train, d_test: data frames for train and test
  #   n_bins: number of bins for each numeric column

  # Return:
  #   list of two data frames, $train and $test
  numeric_cols <- names(d_train[, unlist(lapply(d_train, is.numeric))])
  qs <- seq(1/n_bins, 1, 1/n_bins)

  disc_train <- d_train
  disc_test <- d_test

  if (length(numeric_cols) > 0) {  # Non-zero numeric columns
    for (cn in numeric_cols) {
      col_train <- d_train[[cn]]
      col_test <- d_test[[cn]]

      breaks <- unique(quantile(col_train, qs))
      breaks[n_bins] <- Inf
      disc_train[[cn]] <- as.factor(cut(col_train, c(-Inf, breaks)))
      disc_test[[cn]] <- as.factor(cut(col_test, c(-Inf, breaks)))
    }
  }

  list(train = disc_train, test = disc_test)
}

get_k_feat_model <- function(ass, subsets, features,
                             train_df, train_labels, k){
  # Get forula from subsets that use exactly n original features

  # Args:
  #   ass: the "assign" attribute from a full model.matrix
  #   subsets: a summary.regsubsets object
  #   features: character vector of original feature columns
  #   train_df, test_df: original train/test data frames
  #   train_labels, test_labels: original train/test labels
  #   k: parameter k to get model for
  #
  # Note: Intermediate models are saved as a side effect
  #
  # Returns:
  #   data.frame of all predictions over k models
  formulas <- get_formulas(ass, subsets, features)

  if (k > max(formulas$n)) {
    stop("k=", k," is too large; max is ", max(formulas$n))
  }

  ss_formula <- formulas %>%
    filter(n == k) %>%
    pull(formula) %>%
    first()

  get_k_feat_model_from_formula(ss_formula, train_df, train_labels, k)

}

get_k_feat_model_from_formula <- function(formula, train_df, train_labels, k){
  # Get SRR model for k given formula

  # Args:
  #   formula: model formula
  #   train_df, test_df: original train/test data frames
  #   train_labels, test_labels: original train/test labels
  #   k: parameter k to get model for

  # Update the selected subset
  train_ssmm <- model.matrix(formula, data = train_df)

  # Lasso model is used for SRR, so we keep a copy
  m_lasso_k <- fit_lasso(train_ssmm, train_labels, fold = 5, k)

  # Use trained model to generate heuristics
  roundM_model <- as.matrix(coef(m_lasso_k, s = "lambda.min"))
  inf_model <- as.matrix(coef(m_lasso_k, s = "lambda.min"))
  coef_names <- rownames(roundM_model)
  ind_non_intercept <- coef_names != "(Intercept)"
  nfeat <- dim(roundM_model)[1]
  roundM_model <- array(roundM_model, c(nfeat, length(Ms)))
  max_coef <- max(abs(roundM_model[ind_non_intercept, ]))
  tmp.rounds <- roundM_model[ind_non_intercept, ]
  tmp.rounds <- (tmp.rounds/max_coef) %*% diag(Ms + 0.499)
  tmp.rounds <- round(tmp.rounds)
  roundM_model[ind_non_intercept, ] <- tmp.rounds

  rownames(roundM_model) <- coef_names
  colnames(roundM_model) <- Ms

  list(model = m_lasso_k, round = roundM_model[ind_non_intercept,])
}

