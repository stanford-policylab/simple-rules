#!/usr/bin/env Rscript
source("utils.R")

theme_set(theme_bw())

bench_models <- c(
  "glm",
  "lasso",
  "rf"
  )

# SRR settings
srr_base_model <-  "disc_lasso"
srr_k <- 5
srr_M <- 3

target_datasets <- unlist(gt10feats)

# Setup paths and global variables ----------------------------------------
publish_prefix <- PLOT_PATH

# Load performance data ---------------------------------------------------
perf_all <- read_from_path(PERFS_PATH, "aggregate performance")
ndata <- length(unique(perf_all$dataset))

# Helper functions --------------------------------------------------------
gammaHat <- function(df, base.df) {
  pos <- filter(base.df, labels == 1)
  sigma_p <- sd(pos$base_pred)
  neg <- filter(base.df, labels == 0)
  sigma_n <- sd(neg$base_pred)
  agg.df <- left_join(df, base.df, by = c("dataset", "example_id"))
  diff <- agg.df$base_pred - agg.df$pred
  sigma <- sd(diff)
  gamma <- sigma^2/((sigma_p^2+sigma_n^2)/2)
  data.frame(gamma_hat = gamma)
}

# Global plot attributes --------------------------------------------------
# Plot labelling/coloring functions/vectors
rounding_color <- c(
  "1" = "#E41A1C",
  "2" = "#377EB8",
  "3" = "#4DAF4A",
  "4" = "deepskyblue4",
  "5" = "deeppink3",
  "Inf" = "black"
)

translate_labels <- function(s) {  # Convert label text to meaningful text
  sapply(s, function(x) switch(as.character(x),
    glm = "Logit",
    lasso = "Lasso",
    rf = "Random Forest",
    disc_glm = "Logit (discretized)",
    disc_lasso = "Lasso (discretized)",
    disc_rf = "Random Forest (discretized)",
    dummy = NA,
    # Default to original value
    s)
  )
}

translate_labels_short <- function(s) {  # Convert label text to meaningful text
  sapply(s, function(x) switch(as.character(x),
    glm = "Logit",
    lasso = "L1",
    rf = "RF",
    disc_glm = "Logit (discretized)",
    disc_lasso = "L1 (discretized)",
    disc_rf = "RF (discretized)",
    # Default to its own value
    s))
}

legend_name <- "Coefficient range"

# AUC plot params
str_y_label = "AUC\n"

str_x_label = "\nNumber of features"
x_limits <- c(1, 10)
x_breaks = c(1, seq(5,10,5))

concat_feats <- 10

# PLOT: Scatter AUC for complex v. simple ---------------------------------
scatter_pd <- perf_all %>%
  ungroup() %>%
  # Filter to target experiments (either benchmark complex models or SRR)
  filter(dataset %in% target_datasets) %>%
  filter(nzero == Inf |
         (num_feats == srr_k &
          round_lvl == srr_M &
          model_type == srr_base_model)) %>%
  filter(model_type %in% c(bench_models, srr_base_model)) %>%
  mutate(type = ifelse(round_lvl == Inf, as.character(model_type), "srr")) %>%
  # filter(dataset == "chess-krvkp")
  select(dataset, type, auc)  %>%
  tidyr::spread(type, auc) %>%
  tidyr::gather(type, complex, !!!bench_models)

p_scatter <- ggplot(scatter_pd, aes(y = srr, x = complex)) +
  facet_grid(. ~ type, labeller = labeller(
    type = c("glm" = "Logistic", "lasso" = "Lasso", "rf" = "Random forest")
    )) +
  geom_point(shape = 1, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous("AUC for SRR\n",
                     labels = scales::percent_format(1),
                     limits = c(.5, 1)) +
  scale_x_continuous("\nAUC for \"complex\" model",
                     labels = scales::percent_format(1),
                     limits = c(.5, 1)) +
  theme(plot.margin = unit(rep(.6, 4), "cm"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(publish_prefix, "scatter.pdf"),
       plot = p_scatter, width = 12, height = 4)

# PLOT: AUC facet for each of the 21 datasets -----------------------------
FACET_ALLFEATS <- 11
max_round <- 3

facet_auc_pd <- perf_all %>%
  filter(dataset %in% target_datasets) %>%
  mutate(perf = auc)

complex_fauc_pd <- facet_auc_pd %>%
  ungroup() %>%
  filter(num_feats == Inf, model_type %in% bench_models)

srr_fauc_pd <- facet_auc_pd %>%
  filter(model_type == srr_base_model) %>%
  mutate(base_model = model_type)

noround_fauc_pd <- srr_fauc_pd %>%
  filter(num_feats != Inf, round_lvl == Inf, num_feats <= concat_feats)

rounded_fauc_pd <- srr_fauc_pd %>%
  filter(num_feats != Inf, round_lvl <= max_round, num_feats <= concat_feats)

# Arbitrary large number to break plot
complex_fauc_pd$num_feats <- FACET_ALLFEATS

p_fauc <- ggplot(rounded_fauc_pd, aes(x = num_feats, y = perf)) +
  facet_wrap(~ dataset, ncol = 4) +
  geom_point(data = complex_fauc_pd, aes(shape = model_type), size = 3) +
  scale_shape("Model type",
              limits = c("glm", "lasso", "rf"),
              labels = c("Logistic", "Lasso", "Random forest")) +
  scale_y_continuous(str_y_label, labels = scales::percent_format(1)) +
  scale_x_continuous(str_x_label, labels = c("1", "5", "10", "All"),
                     limits = c(1, 11),
                     breaks = c(1, 5, 10, FACET_ALLFEATS)) +
  geom_line(data = noround_fauc_pd, aes(color = factor(round_lvl))) +
  geom_line(aes(color = factor(round_lvl), group = round_lvl)) +
  scale_color_manual(
    legend_name,
    limits = c(unique(rounded_fauc_pd$round_lvl), Inf),
    values = rounding_color,
    breaks = c(unique(rounded_fauc_pd$round_lvl), Inf),
    label = c(sprintf("[-%1$s, %1$s]", unique(rounded_fauc_pd$round_lvl)),
              "No rounding")) +
  coord_cartesian(ylim = c(.5, 1)) +
  theme(legend.position = "top",
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))
ggsave(filename = file.path(publish_prefix, 'facet_auc.pdf'),
       plot = p_fauc, width = 15, height = 15)

# PLOT: AUC facet for each of the 21 datasets with binarized features -----
max_round <- 3

facet_auc_pd <- perf_all %>%
  filter(dataset %in% target_datasets) %>%
  mutate(perf = auc)

srr_fauc_pd <- facet_auc_pd %>%
  filter(model_type == srr_base_model,
         num_feats != Inf,
         num_feats <= concat_feats,
         round_lvl == Inf | round_lvl < max_round) %>%
  mutate(base_model = model_type,
         round_lvl = factor(round_lvl)) %>%
  group_by(dataset, model_type, round_lvl, nzero) %>%
  summarize(mean_perf = mean(perf), min_perf = min(perf), max_perf = max(perf))

srr_fauc_pd %>%
  filter(dataset == "bankruptcy")

p_fauc <- ggplot(srr_fauc_pd, aes(x = nzero, y = mean_perf)) +
  facet_wrap(~ dataset, ncol = 4, scales = "free_x") +
  scale_y_continuous(str_y_label, labels = scales::percent_format(1)) +
  scale_x_continuous("\nNumber of non-zero coefficients",
                     labels = round) +
  geom_line(aes(color = round_lvl, group = round_lvl)) +
  scale_fill_manual(
    legend_name,
    limits = c(unique(rounded_fauc_pd$round_lvl), Inf),
    values = rounding_color,
    breaks = c(unique(rounded_fauc_pd$round_lvl), Inf),
    label = c(sprintf("[-%1$s, %1$s]", unique(rounded_fauc_pd$round_lvl)),
              "No rounding")) +
  scale_color_manual(
    legend_name,
    limits = c(unique(rounded_fauc_pd$round_lvl), Inf),
    values = rounding_color,
    breaks = c(unique(rounded_fauc_pd$round_lvl), Inf),
    label = c(sprintf("[-%1$s, %1$s]", unique(rounded_fauc_pd$round_lvl)),
              "No rounding")) +
  theme(legend.position = "top",
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))
ggsave(filename = file.path(publish_prefix, 'facet_auc_nzero.pdf'),
       plot = p_fauc, width = 15, height = 15)

# PLOT: CV Mean AUC over target datasets ----------------------------------
ALLFEATS <- 9999
plot_width <- 6
plot_height <- 5

max_round <- 3

mean_auc_pd <- perf_all %>%
  filter(dataset %in% target_datasets) %>%
  group_by(model_type, num_feats, round_lvl) %>%
  summarize(perf = mean(auc)) %>%
  ungroup()

complex_mauc_pd <- mean_auc_pd %>%
  filter(num_feats == Inf, model_type %in% bench_models) %>%
  mutate(num_feats = ALLFEATS) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS - 1,
          round_lvl = Inf, perf = 0) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS,
          round_lvl = Inf, perf = 0) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS + 4,
          round_lvl = Inf, perf = 0) %>%
  # "z" to make sure the ordering is last
  mutate(xsplit = "z_complex") %>%
  bind_rows(tibble(model_type = "dummy",
                   num_feats = seq(1:10),
                   round_lvl = Inf,
                   perf = 0,
                   xsplit = "0_heuristic"))

srr_mauc_pd <- mean_auc_pd %>%
  filter(model_type == srr_base_model) %>%
  rename(base_model = model_type) %>%
  # "0" for ordering first
  mutate(xsplit = "0_heuristic")

noround_mauc_pd <- srr_mauc_pd %>%
  filter(num_feats != Inf, round_lvl == Inf, num_feats <= concat_feats)

kcomplex_mauc_pd <- mean_auc_pd %>%
  filter(num_feats != Inf, round_lvl == Inf, num_feats <= concat_feats,
         model_type %in% bench_models, model_type != srr_base_model) %>%
  mutate(xsplit = "0_heuristic")

rounded_mauc_pd <- srr_mauc_pd %>%
  filter(num_feats != Inf, round_lvl <= max_round, num_feats <= concat_feats)

split_x_breaks <- c(x_breaks, ALLFEATS)
label_x_breaks <- function(variable, value) {
  if (value == ALLFEATS) {
    return("All")
  } else {
    return(x)
  }
}

p_mauc <- ggplot(rounded_mauc_pd, aes(x = num_feats, y = perf)) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  facet_grid(. ~ xsplit, scales = "free_x", space = "free_x") +
  geom_point(aes(shape = model_type), data = complex_mauc_pd, na.rm = TRUE) +
  geom_label(data = complex_mauc_pd,
             aes(label = translate_labels(model_type)),
             hjust = 0,
             vjust = c(rep(0, 10),  # first 10 dummy
                       1,           # Logit
                       .0,          # Lasso
                       .5,          # Rf
                       rep(0, 3)),  # last three dummy
             nudge_x = .2,
             na.rm = TRUE)

# Legends and Scales
p_mauc <- p_mauc +
  scale_shape(element_blank(), guide = FALSE) +
  scale_y_continuous(sprintf(str_y_label, ndata),
                     labels = scales::percent_format(1),
                     limits = c(.6, 1), expand = c(0, 0)) +
  scale_x_continuous(str_x_label, labels = c("1", "5", "10", "All"),
                     breaks = split_x_breaks,
                     expand = c(0, 0)) +
  scale_color_manual(element_blank(), breaks = c(Inf), values = c("black"),
                     label = c("No rounding")) +
  theme(legend.position = c(.65, 0),
        legend.background = element_blank(),
        legend.justification = c(1, 0),
        plot.margin = unit(rep(.5, 4), "cm"))

q_mauc <- p_mauc +
  geom_line(data = noround_mauc_pd,
            aes(color = factor(round_lvl))) +
  scale_color_manual(
    legend_name,
    limits = c(unique(rounded_mauc_pd$round_lvl), Inf),
    values = rounding_color,
    breaks = c(unique(rounded_mauc_pd$round_lvl), Inf),
    label = c(sprintf("[-%1$s, %1$s]", unique(rounded_mauc_pd$round_lvl)),
              "No rounding")
    )

r_mauc <- p_mauc +
  geom_line(aes(color = factor(round_lvl))) +
  geom_line(data = noround_mauc_pd, aes(color = factor(round_lvl))) +
  scale_color_manual(
    legend_name,
    limits = c(unique(rounded_mauc_pd$round_lvl), Inf),
    values = rounding_color,
    breaks = c(unique(rounded_mauc_pd$round_lvl), Inf),
    label = c(sprintf("[-%1$s, %1$s]", unique(rounded_mauc_pd$round_lvl)),
              "No rounding")
    ) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))
ggsave(filename = file.path(publish_prefix, "Fig1_color_auc.pdf"),
       plot = r_mauc, width = plot_width, height = plot_height)

s_mauc <- r_mauc +
  coord_cartesian(ylim = c(.7, 1)) +
  theme(legend.position = "none")
ggsave(filename = file.path(publish_prefix, "Fig1_color_auc_no_legend.pdf"),
       plot = s_mauc, width=plot_width, height=plot_height)

# PLOT: Extra plot for revision response ----------------------------------
ALLFEATS <- 9999
plot_width <- 6
plot_height <- 5

mean_auc_pd <- perf_all %>%
  filter(dataset %in% target_datasets) %>%
  group_by(model_type, num_feats, round_lvl) %>%
  summarize(perf = mean(auc)) %>%
  ungroup()

complex_mauc_pd <- mean_auc_pd %>%
  filter(num_feats == Inf, model_type %in% bench_models) %>%
  mutate(num_feats = ALLFEATS) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS - 1,
          round_lvl = Inf, perf = 0) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS,
          round_lvl = Inf, perf = 0) %>%
  add_row(model_type = "dummy", num_feats = ALLFEATS + 4,
          round_lvl = Inf, perf = 0) %>%
  # "z" to make sure the ordering is last
  mutate(xsplit = "z_complex") %>%
  bind_rows(tibble(model_type = "dummy",
                   num_feats = seq(1:10),
                   round_lvl = Inf,
                   perf = 0,
                   xsplit = "0_heuristic"))

kcomplex_mauc_pd <- mean_auc_pd %>%
  filter(num_feats != Inf, round_lvl == Inf, num_feats <= concat_feats,
         model_type %in% bench_models, model_type != srr_base_model) %>%
  mutate(xsplit = "0_heuristic")

split_x_breaks <- c(x_breaks, ALLFEATS)
label_x_breaks <- function(variable, value) {
  if (value == ALLFEATS) {
    return("All")
  } else {
    return(x)
  }
}

v2_auc <- ggplot(kcomplex_mauc_pd, aes(x = num_feats, y = perf)) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  facet_grid(. ~ xsplit, scales = "free_x", space = "free_x") +
  geom_point(aes(shape = model_type, color = model_type), data = complex_mauc_pd, na.rm = TRUE) +
  geom_label(data = complex_mauc_pd,
             aes(label = translate_labels(model_type)),
             hjust = 0,
             vjust = c(rep(0, 10),  # first 10 dummy
                       1,           # Logit
                       .0,          # Lasso
                       .5,          # Rf
                       rep(0, 3)),  # last three dummy
             nudge_x = .2,
             na.rm = TRUE) +
  scale_shape(element_blank(), guide = FALSE) +
  scale_color_discrete(element_blank(), guide = FALSE) +
  scale_y_continuous(sprintf(str_y_label, ndata),
                     labels = scales::percent_format(1),
                     limits = c(.6, 1), expand = c(0, 0)) +
  scale_x_continuous(str_x_label, labels = c("1", "5", "10", "All"),
                     breaks = split_x_breaks,
                     expand = c(0, 0))  +
  theme(legend.position = c(.65, 0),
        legend.background = element_blank(),
        legend.justification = c(1, 0),
        plot.margin = unit(rep(.5, 4), "cm"))

v2_mauc <- v2_auc +
  geom_line(data = kcomplex_mauc_pd, aes(color = model_type)) +
  coord_cartesian(ylim = c(.7, 1)) +
  theme(legend.position = "top",
        legend.just = "left",
        legend.box.just = "left",
        legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.spacing = unit(0, "pt"),
        legend.box = "vertical")

ggsave(filename = file.path(publish_prefix, "Fig1_with_selection.png"),
       plot = v2_mauc, width=plot_width, height=plot_height)

# PLOT: Class distribution (heart-cleveland) ------------------------------
pred_df <- read_from_path(getPredPath("heart-cleveland"), "specific preds")

# Lasso model predictions
dist_pd <- pred_df %>%
  filter(model_type == "lasso", num_feats == Inf, round_lvl == Inf) %>%
  mutate(logit = binomial()$linkfun(pred))

lab <- dist_pd %>%
  group_by(labels) %>%
  summarize(x = mean(logit)) %>%
  mutate(y = .1,
  str_labels = ifelse(labels == 1,
                      "Positive\ninstances",
                      "Negative\ninstances"))

p_dist <- ggplot(data = dist_pd, aes(x = logit)) +
  geom_density(aes(y = ..density.., linetype = factor(labels))) +
  geom_text(data = lab, aes(x = x, y = y, label = str_labels),
            nudge_x = c(0, 0), nudge_y = c(.1, 0), size = 7) +
  scale_x_continuous(paste0("Model score"), limits = c(-4, 4),
                     expand = c(0,0)) +
  scale_y_continuous(element_blank(), breaks = NULL, expand = c(0,0.01)) +
  scale_linetype(guide = FALSE) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_blank()
    )
ggsave(filename = file.path(publish_prefix, "FigA1_lasso_distribution.pdf"),
       plot = p_dist, width = 5, height = 5)

# PLOT: Empirical distribution of SRR error (vs. full) --------------------
pred_df <- read_from_path(getPredPath("heart-cleveland"), "specific preds")

error_pd <- pred_df %>%
  filter((model_type == "lasso" &
          num_feats == Inf & round_lvl == Inf) |
         (model_type == srr_base_model &
          num_feats == srr_k & round_lvl == srr_M)) %>%
  mutate(logit = binomial()$linkfun(pred),
         type = ifelse(round_lvl == Inf, "lasso", "srr")) %>%
  select(example_id, type, logit) %>%
  spread(type, logit) %>%
  mutate(eps = lasso - srr)

p_error <- ggplot(error_pd, aes(x = eps)) +
  geom_density() +
  scale_x_continuous(expression(epsilon), limits=c(-4, 4),
                     expand=c(0, 0)) +
  scale_y_continuous(element_blank(), breaks=NULL, expand=c(0, 0.012)) +
  scale_linetype(guide=FALSE) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_blank()
    )
ggsave(
  filename = file.path(publish_prefix, "FigA3_error_distribution.pdf"),
  plot = p_error, width = 5, height = 5)

# PLOT: Theoretical gamma effects on AUC ----------------------------------
sigmas <- seq(0.01, 1, 0.01)
gammas <- seq(0, 2, 0.1)

gamma_pd <- foreach (start_auc = seq(0.6, .9, 0.1),
                     .combine = bind_rows) %do% {
    auc <- pnorm(qnorm(start_auc)/(sqrt(1+gammas)))
    tmp_df <- tibble(gammas = gammas, auc = auc, start_auc = start_auc)
    tmp_df
}

gamma_labels <- tibble(x = 0,
                       y = seq(.6, .9, .1),
                       lab = paste0("AUC[Y]==", seq(60, 90, 10)))

p_gamma <- ggplot(data = gamma_pd, aes(x = gammas, y = auc)) +
  geom_line(aes(color = factor(start_auc))) +
  scale_x_continuous(
    expression(gamma),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expression(AUC[hat(Y)], "\n"),
    expand = c(0, 0),
    breaks = seq(.6, .9, .1),
    labels = scales::percent_format(1),
    limits = c(.55, .95)
  ) +
  scale_color_manual(guide = FALSE, values = rep("black", 10)) +
  geom_text(
    data = gamma_labels,
    aes(x = x, y = y, label = lab, vjust = 0, hjust = .2),
    parse = TRUE, nudge_x = .15, nudge_y = .004, size = 5
  )  +
  geom_text(
    data = gamma_labels,
    aes(x = x, y = y, label = "%", vjust = 0, hjust = .2),
    nudge_x = .54, nudge_y = .007, size = 5
  ) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 18),
        plot.margin = unit(c(0.2, .5, .2, 0), "cm"))
ggsave(filename = file.path(publish_prefix, "FigA2_general_auc_gamma.pdf"),
       plot = p_gamma, width = 5, height = 5)
ggsave(filename = file.path(publish_prefix, "FigA2_general_auc_gamma.png"),
       plot = p_gamma, width = 5, height = 5)

# PLOT: Gamma_hat distribution --------------------------------------------
preds_all <- read_from_path(PREDS_PATH, "predictions") %>%
  mutate(pred = ifelse(pred > .99, .99, pred),
         pred = ifelse(pred < .01, .01, pred),
         prob = pred,
         pred = binomial()$linkfun(pred))

base_preds <- preds_all %>%
  filter(model_type == "lasso", num_feats == Inf) %>%
  select(base_pred = pred, dataset, example_id, labels)

srr_preds <- preds_all %>%
  filter(model_type == srr_base_model,
         num_feats == srr_k,
         round_lvl == srr_M)

gamma_hat_pd <- srr_preds %>%
  group_by(dataset, model_type, num_feats, round_lvl) %>%
  do(gammaHat(., base_preds))

p_gh_dist <- ggplot(gamma_hat_pd, aes(x = gamma_hat)) +
  geom_histogram(binwidth = 0.3) +
  scale_y_continuous(element_blank()) +
  scale_x_continuous(expression(hat(gamma))) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 28),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(.2, .4, 0, 0), "cm")
    )
ggsave(filename = file.path(publish_prefix, "gamma_dist.pdf"),
       plot = p_gh_dist, width = 5, height = 5)
ggsave(filename = file.path(publish_prefix, "gamma_dist.png"),
       plot = p_gh_dist, width = 5, height = 5)
