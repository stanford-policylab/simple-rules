#!/usr/bin/env Rscript
# Case study results for single data set
source("utils.R")  # loads plyr, tidyverse, and doMC
source("subfuncs.R")

theme_set(theme_bw())
set.seed(94305)

data_dirs <- list.dirs(DATA_DIR, full.names = FALSE, recursive = FALSE)

# Set the specific data parameters here
test_fold <- 1
dataname <- "german_credit"

# Initial setup (load data and set hyper parameters -----------------------

# Hyper-parameters
MAX_K <- 10
Ms <- c(1:5, 10)  # coefs rounded to use M integers
N_BINS <- 3  # Number of equal-sized bins for discretization

cat(sprintf("Fit models for [%s] at %s\n", dataname,
            format(Sys.time(), "%H:%M")))
sptm <- proc.time()["elapsed"]  # Time each dataset

clean_data <- dataname %>%
  getCleanDataPath() %>%
  read_rds()

fold_id <- "fold_idx__"
nfolds <- max(clean_data[fold_id])

scores <- clean_data %>%
  select_(.dots = c("label", fold_id, "example_id"))

data <- clean_data %>%
  select(-starts_with("fold_idx"), -example_id) %>%
  mutate(
         Credit_history = relevel(Credit_history, "A30"),
         Purpose = relevel(Purpose, "A40"),
         Savings_account = relevel(Savings_account, "A61")
  )

features <- colnames(data %>% select(-label))
model_formula <- make_formula("label", features)

train_ind <- scores[fold_id] != test_fold
test_ind <- !train_ind

train_df <- data[train_ind, ]
train_labels <- train_df$label

test_df <- data[test_ind, ]
test_labels <- test_df$label
test_id <- scores$example_id[test_ind]

# Build model matrix with standardized data
train_mm <- model.matrix(model_formula, data = train_df)
test_mm <- model.matrix(model_formula, data = test_df)

# Fit complex models ------------------------------------------------------
m_lasso <- fit_lasso(train_mm, train_labels, fold = 5, k = Inf)
pred_lasso_df <- m_lasso %>%
  pred_lasso(test_mm, test_labels, test_id, dataname, fold = 5, k = Inf)

m_rf <- fit_rf(train_mm, train_labels, fold = 5, k = Inf)
pred_rf_df <- m_rf %>%
  pred_rf(test_mm, test_labels, test_id, dataname, fold = 5, k = Inf)

# SRR ---------------------------------------------------------------------
# Models using k, all discretize, features
disc_dfs <- discretize(train_df, test_df, N_BINS)

disc_train_df <- disc_dfs$train
disc_test_df <- disc_dfs$test
disc_feats <- colnames(disc_train_df %>% select(-label))
disc_formula <- make_formula("label", disc_feats)
disc_mm <- model.matrix(disc_formula, data = disc_train_df)

disc_params <- get_subsets(disc_mm, train_labels)
disc_subsets <- disc_params$subsets
disc_assign <- disc_params$ass

formulas <- get_formulas(disc_assign, disc_subsets, disc_feats)

formulas %>%
  filter(n <= 5) %>%
  unnest(feats) %>%
  distinct(feats, .keep_all = TRUE)

srr_formula <- formulas %>%
  filter(n == 5) %>%
  pull(formula) %>%
  first()

k_model <- get_k_feat_model_from_formula(srr_formula,
                                         disc_train_df, train_labels, 5)

cat(sprintf("Using lambdas in range 1e-4 to %.3f, with best %.3f\n",
            range(k_model$model$lambda)[2],
            k_model$model$lambda.min))

# Print model as LaTeX table
cat(paste(rownames(k_model$round),
      format(coef(k_model$model, s = "lambda.min")[c(-1, -2),], digits = 2),
      k_model$round[, 3],
      sep = " & ",
      collapse = " \\\\ \n"), "\\\\ \n")

etm <- as.numeric(proc.time()["elapsed"] - sptm)
cat(sprintf("finish [%s] (%0.1fs)\n", dataname, etm))

# PLOT: Discretization of duration in months ------------------------------
dim_qs <- quantile(train_df$Duration_in_months, c(1/3, 2/3))

p_dim_dist <- ggplot(train_df, aes(x = Duration_in_months)) +
  geom_histogram(binwidth = 6) +
  geom_vline(xintercept=dim_qs + 3, color = "red", linetype = "dashed") +
  scale_x_continuous("\nDuration in months") +
  scale_y_continuous(element_blank()) +
  theme(
    plot.margin = unit(c(0, 1, 0, 0), "cm"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    )
ggsave(filename = file.path(PLOT_PATH, "C_dim_dist.png"),
       plot = p_dim_dist, width = 5, height = 3)

# Performance comparison --------------------------------------------------
evaluate <- function(d, method) {
  d %>%
    arrange(desc(!!rlang::sym(method))) %>%
    mutate(method = method,
           pos = 1,
           n_pos = cumsum(pos),
           n_neg = n() - n_pos,
           n_true = sum(labels),
           n_false = n() - n_true,
           n_tp = cumsum(labels),
           n_fp = cumsum(pos != labels),
           tpr = n_tp / n_true,
           fpr = n_fp / n_false,
           prec = n_tp / n_pos,
           p_lend = 1 - (n_pos / n()),
           p_def = (n_true - n_tp)/n(),
           cost = (n_true - n_tp) * 5 + (n_pos - n_tp) * 2
           )
}

disc_test_mm <- model.matrix(srr_formula, data = disc_test_df)

srr_pred <- tibble(
  example_id = test_id,
  srr = c(disc_test_mm[, rownames(k_model$round)] %*% k_model$round[, 3])
)

preds <- tibble(labels = test_labels == 1, example_id = test_id) %>%
  left_join(pred_lasso_df %>% select(lasso = pred, example_id),
            by = "example_id") %>%
  left_join(pred_rf_df %>% select(rf = pred, example_id),
            by = "example_id") %>%
  left_join(srr_pred, by = "example_id")

preds %>%
  gather(method, pred, lasso, rf, srr) %>%
  group_by(method) %>%
  summarize(auc = auc(labels, pred))

cost_lasso <- preds %>%
  evaluate("lasso")

cost_rf <- preds %>%
  evaluate("rf")

cost_srr <- preds %>%
  group_by(srr) %>%
  summarize(pos = n(),
            tp = sum(labels),
            fp = pos - tp) %>%
  ungroup() %>%
  arrange(desc(srr)) %>%
  mutate(method = "srr",
         n_pos = cumsum(pos),
         n_neg = sum(pos) - n_pos,
         n_true = sum(tp),
         n_false = sum(fp),
         n_tp = cumsum(tp),
         n_fp = cumsum(fp),
         tpr = n_tp / n_true,
         fpr = n_fp / n_false,
         prec = n_tp / n_pos,
         p_lend = 1 - (n_pos / sum(pos)),
         p_def = (n_true - n_tp)/sum(pos),
         cost = (n_true - n_tp) * 5 + (n_pos - n_tp) * 2)

costs_complex <- cost_rf

# PLOT: Performance plots -------------------------------------------------
lend_curve <- ggplot(costs_complex, aes(x = p_lend, y = p_def)) +
  geom_line(aes(color = method)) +
  geom_point(aes(color = method), data = cost_srr, size = 5) +
  scale_color_manual(breaks = c("lasso", "srr"), values = c("black", "red"),
                     guide = FALSE) +
  scale_x_continuous("\n% approved",
                     limits = c(0, 1),
                     expand = c(0, 0), labels = scales::percent) +
  scale_y_continuous("% bad credit approvals\n",
                     breaks = c(.2, .4, .6, .8), limits = c(0, .7),
                     expand = c(0, 0), labels = scales::percent) +
  theme(
    plot.margin = unit(c(0, 1, 0, 0), "cm"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    )
ggsave(filename = file.path(PLOT_PATH, "C_lending_defaults.png"),
       plot = lend_curve, width = 5, height = 5)

roc_curve <- ggplot(costs_complex, aes(x = fpr, y = tpr)) +
  geom_abline(color = "black", linetype = "dotted", intercept = 0, slope = 1) +
  geom_line(aes(color = method)) +
  geom_line(aes(color = method), linetype = "dashed", data = cost_srr) +
  geom_point(aes(color = method), data = cost_srr, size = 5) +
  scale_color_manual(breaks = c("lasso", "srr"), values = c("blue", "red"),
                     guide = FALSE) +
  scale_x_continuous("\nFalse positive rate",
                     expand = c(0, 0), labels = scales::percent) +
  scale_y_continuous("True positive rate\n",
                     expand = c(0, 0), labels = scales::percent) +
  coord_fixed(ratio = 1) +
  theme(
    plot.margin = unit(c(0, 1, 0, 0), "cm"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    )
ggsave(filename = file.path(PLOT_PATH, "C_lending_roc.png"),
       plot = roc_curve, width = 5, height = 5)
