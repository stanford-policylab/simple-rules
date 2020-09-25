# Common utilities to use accros different scripts
library(assertthat)
library(plyr)
library(tidyverse)
library(doMC)
library(optparse)
library(ROCR)

OPT_LIST <- list(
  make_option(c("-v", "--version"), default = "v3",
              help = "Version string to keep track of runs"),
  make_option(c("-c", "--use_saved_models"), action="store_true",
              default=TRUE,
              help=paste("Store models as .rds, and load",
                         "(instead of fitting) if a saved model",
                         "already exists [default]")),
  make_option(c("-f", "--force_fit"), action="store_false",
              dest="use_saved_models",
              help="Ignore saved models and fit all models from scratch")
)

OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

use_saved_models <- opt$use_saved_models

DATA_DIR <- normalizePath("../data", mustWork = TRUE)
PLOT_PATH <- normalizePath("../figs", mustWork = TRUE)

# Where "final" version-specific aggregate predictions/performance metrics
PREDS_PATH <- file.path(DATA_DIR, sprintf("predictions.%s.rds", opt$version))
PERFS_PATH <- file.path(DATA_DIR, sprintf("performance.%s.rds", opt$version))

nproc <- detectCores()
maxcores <- 10  # Use at most maxcores cores
nproc <- min(maxcores, nproc)
registerDoMC(cores = nproc)

# Helper functions and path builders --------------------------------------
make_formula <- function(y, vars) {
  as.formula(paste(y, "~", paste(vars, collapse = "+")))
}


getCleanDataPath <- function(dataname) {
  file.path(DATA_DIR, dataname, "data.rds")
}

getPredPath <- function(dataname) {
  file.path(DATA_DIR, dataname, "pred.rds")
}

getPerfPath <- function(dataname) {
  file.path(DATA_DIR, dataname, "perf.rds")
}

getModelName <- function(model, type) {
  if (is.null(type)) {
    return(model)
  } else {
    return(paste(type, model, sep = "_"))
  }
}

getModelPath <- function(dataname, fold, k=Inf, modelname="srr") {
  if (k <= 0) {
    stop(sprintf("Non-positive k for [%s] model:%s k:%s, fold:%s",
                 dataname, modelname, k, fold))
  } else if (k < Inf) { # heuristic model with k
    subdir <- sprintf("k%03d", k)
  } else if (k == Inf) { # benchmark models
    subdir <- "kInf"
  } else {
    stop(sprintf("Unspecified k for [%s] model:%s k:%s, fold:%s",
                 dataname, modelname, k, fold))
  }

  datapath <- file.path(DATA_DIR, dataname, modelname, subdir)

  if (!dir.exists(datapath)) {dir.create(datapath, recursive=TRUE)}

  file.path(datapath, sprintf("fold%02d.rds", fold))
}

write_to_path <- function(obj, path, name = NULL) {
  cat(sprintf("Writing %s to %s\n", name, path))
  write_rds(obj, path = path)
}
write_model <- function(model, path) {
  write_to_path(model, path, "model")
}

read_from_path <- function(path, name = NULL) {
  cat(sprintf("Reading %s from %s\n", name, path))
  read_rds(path = path)
}
read_model <- function(path) {
  read_from_path(path, "model")
}

# Directories (UCI datasets) to skip when building etc. ########################
gt10feats <- list(
  "adult",
  "annealing",
  "audiology-std",
  "bank",
  "bankruptcy",
  "car",
  "chess-krvk",
  "chess-krvkp",
  "congress-voting",
  "contrac",
  "credit-approval",
  "ctg",
  "cylinder-bands",
  "dermatology",
  "german_credit",
  "heart-cleveland",
  "ilpd",
  "mammo",
  "mushroom",
  "wine",
  "wine_qual"
  )
skip_dirs <- c("raw", "NON_UCI")

setPredDf <- function(
  pred, labels, dataname, type, ids, fold, nz=Inf, round=Inf, numfeats=Inf) {
  tmp_df <- data.frame(pred, labels, dataname, type, ids, fold, round,
                       nz, numfeats)
  names(tmp_df) <- c(
    "pred", "labels", "dataset", "model_type", "example_id", "fold_id",
    "round_lvl", "nzero", "num_feats"
    )
  rownames(tmp_df) <- NULL
  tmp_df
}

initPredDf <- function() {
  tmp_df <- data.frame(pred=double(), labels=character(), dataset=character(),
                       model_type=character(), example_id=integer(),
                       fold_id=integer(), round_lvl=double(), nz=integer(),
                       num_feats=integer())
  names(tmp_df) <- c("pred", "labels", "dataset", "model_type", "example_id",
                     "fold_id", "round_lvl", "nzero", "num_feats")
  return(tmp_df)
}

setCoefDf <- function(coefs, features, dataname, type, fold, round=Inf,
                      numfeats=Inf) {
  tmp_df <- data.frame(coefs, features, dataname, type, fold, round, numfeats)
  names(tmp_df) <- c("value", "feature", "dataset", "model_type", "fold_id",
                     "round_lvl", "num_feats")
  rownames(tmp_df) <- NULL
  tmp_df
}

# Some helper functions for heuristics #########################################
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

predict3D <- function(model, newdata, type='score') {
  # Generate manual predictions for 3D-array models (e.g., roundM)
  pred <- as.matrix(cbind(1, newdata)) %*% as.matrix(model)
  if (type == 'response') pred <- binomial()$linkinv(pred)
  pred
}
# Combine methods
collect <- function(accum, ...) {
  args <- list(...)
  preds <- lapply(args, function(x) x[['pred']])
  coefs <- lapply(args, function(x) x[['coef']])
  pred.df <- bind_rows(preds)
  coef.df <- bind_rows(coefs)
  if (is.list(accum)) {
    pred.df <- rbind(pred.df, accum[['pred']])
    coef.df <- rbind(coef.df, accum[['coef']])
  }
  list(pred=pred.df, coef=coef.df)
}
collectHeur <- function(accum, ...) {
  args <- list(...)
  preds <- lapply(args, function(x) x[['pred']])
  coefs <- lapply(args, function(x) x[['coef']])
  order <- lapply(args, function(x) x[['order']])
  pred.df <- bind_rows(preds)
  coef.df <- bind_rows(coefs)
  order.df <- bind_rows(order)
  if (is.list(accum)) {
    pred.df <- rbind(pred.df, accum[['pred']])
    coef.df <- rbind(coef.df, accum[['coef']])
    order.df <- rbind(order.df, accum[['order']])
  }
  list(pred=pred.df, coef=coef.df, order=order.df)
}
combineEval <- function(accum, ...) {
  args <- list(...)
  auc <- lapply(args, function(x) x[['auc']])
  topk <- lapply(args, function(x) x[['topk']])
  nfeat <- lapply(args, function(x) x[['nfeat']])
  pred <- lapply(args, function(x) x[['pred']])

  auc.df <- bind_rows(auc)
  topk.df <- bind_rows(topk)
  nfeat.df <- bind_rows(nfeat)
  pred.df <- bind_rows(pred)
  if (is.list(accum)) {
    auc.df <- rbind(auc.df, accum[['auc']])
    topk.df <- rbind(topk.df, accum[['topk']])
    nfeat.df <- rbind(nfeat.df, accum[['nfeat']])
    pred.df <- rbind(pred.df, accum[['pred']])
  }
  list(auc=auc.df, topk=topk.df, nfeat=nfeat.df, pred=pred.df)
}
normal_prob_area_plot <- function(
  lb, ub, mean = 0, sd = 1, limits = c(mean - 3 * sd, mean + 3 * sd)) {
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- tibble(x = areax, ymin = 0, ymax = dnorm(areax, mean = mean, sd = sd))
  (ggplot()
   + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax))
   + scale_x_continuous(limits = limits))
}

auc <- function(labels, pred) {
  pred.df <- prediction(pred, labels)
  res <- performance(pred.df, "auc")
  res <- unlist(slot(res, "y.values"))
  return(res)
}
