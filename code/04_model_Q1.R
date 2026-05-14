# =========================================================
# Q1: predict match winner
# =========================================================


# load packages
library(tidyverse)
library(tidymodels)

set.seed(123)
# ---------------------------------------------------------
# read data
# ---------------------------------------------------------
# please change the file path
df <- read_csv("df_partB_ready.csv", show_col_types = FALSE)
"set1_win" %in% names(df)
table(df$set1_win, useNA = "ifany")
# ---------------------------------------------------------
# prepare modelling data
# ---------------------------------------------------------
df_model <- df %>%
  transmute(
    tourney_date  = as.integer(tourney_date),
    surface       = factor(surface, levels = c("Hard", "Clay", "Grass")),
    tourney_level = factor(tourney_level, levels = c("G", "M", "A", "F")),
    
    # outcomes
    y        = factor(y, levels = c(0, 1), labels = c("0", "1")),
   
    
    # pre-match features
    elo_diff,
    rank_points_diff,
    age_diff,
    
    # Set 1 features
    set1_winner_benchmark = factor(set1_win, levels = c(0, 1), labels = c("0", "1")),
    s1_score_diff,
    s1_score_sum,
    s1_margin_abs = abs(s1_score_diff)
  ) %>%
  drop_na()

# ---------------------------------------------------------
# train / test split
# ---------------------------------------------------------
train_data <- df_model %>% filter(tourney_date < 20190101)
test_data  <- df_model %>% filter(tourney_date >= 20190101)

cat("Train rows:", nrow(train_data), "\n")
cat("Test rows :", nrow(test_data), "\n")


# ---------------------------------------------------------
# check Q1 class distribution
# ---------------------------------------------------------
cat("\n===== Q1 target y distribution =====\n")

cat("\nFull modelling dataset:\n")
print(table(df_model$y, useNA = "ifany"))
print(round(prop.table(table(df_model$y, useNA = "ifany")), 4))

cat("\nTrain set:\n")
print(table(train_data$y, useNA = "ifany"))
print(round(prop.table(table(train_data$y, useNA = "ifany")), 4))

cat("\nTest set:\n")
print(table(test_data$y, useNA = "ifany"))
print(round(prop.table(table(test_data$y, useNA = "ifany")), 4))

# ---------------------------------------------------------
# Create a preprocessing recipe.
# ---------------------------------------------------------
make_recipe <- function(formula_obj, train_data, normalize = TRUE) {
  # Inputs:
  #   formula_obj : model formula
  #   train_data  : training dataset
  #   normalize   : whether to normalise numeric predictors
  # Output:
  #   A tidymodels recipe
  rec <- recipe(formula_obj, data = train_data) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_zv(all_predictors())
  
  if (normalize) {
    rec <- rec %>% step_normalize(all_numeric_predictors())
  }
  
  rec
}

# ---------------------------------------------------------
# Calculate test-set performance metrics.
# ---------------------------------------------------------
get_metrics <- function(fit_obj, test_data, outcome_name, model_name) {
  # Inputs:
  #   fit_obj      : fitted workflow object
  #   test_data    : held-out 2019 test dataset
  #   outcome_name : name of the outcome variable
  #   model_name   : model label used in output table
  # Output:
  #   A metric table
  
  # predicted probabilities
  pred_prob <- predict(fit_obj, new_data = test_data, type = "prob")
  
  # predicted class using default threshold = 0.5
  pred_class <- predict(fit_obj, new_data = test_data, type = "class")
  
  res <- bind_cols(
    test_data %>% select(all_of(outcome_name)),
    pred_prob,
    pred_class
  )
  
  names(res)[1] <- "truth"
  
  tibble(
    Model    = model_name,
    Accuracy = accuracy(res, truth = truth, estimate = .pred_class)$.estimate,
    ROC_AUC  = roc_auc(res, truth = truth, .pred_1, event_level = "second")$.estimate,
    F1_Score = f_meas(res, truth = truth, estimate = .pred_class, event_level = "second")$.estimate,
    LogLoss  = mn_log_loss(res, truth = truth, .pred_1, event_level = "second")$.estimate
  )
}

# ---------------------------------------------------------
# logistic regression
# ---------------------------------------------------------

# Fit and evaluate a logistic regression model.
fit_eval_glm <- function(formula_obj, outcome_name, model_name, train_data, test_data) {
  
  rec <- make_recipe(formula_obj, train_data, normalize = TRUE)
  
  mod <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(mod)
  
  fit_obj <- fit(wf, data = train_data)
  
  get_metrics(fit_obj, test_data, outcome_name, model_name)
}

# ---------------------------------------------------------
# random forest
# ---------------------------------------------------------

# Fit and evaluate a random forest model.
fit_eval_rf <- function(formula_obj, outcome_name, model_name, train_data, test_data) {
  
  rec <- make_recipe(formula_obj, train_data, normalize = FALSE)
  
  mod <- rand_forest(
    trees = 500,
    mtry  = 3,
    min_n = 5
  ) %>%
    set_engine("ranger") %>%
    set_mode("classification")
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(mod)
  
  fit_obj <- fit(wf, data = train_data)
  
  get_metrics(fit_obj, test_data, outcome_name, model_name)
}

# =========================================================
# Q1: predict match winner
# ---------------------------------------------------------
# Q1_M0: y ~ prematch
# Q1_M1: y ~ prematch + s1_score_diff
# Q1_M2: y ~ prematch + s1_score_diff + s1_score_sum
# =========================================================

q1_form_m0 <- y ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level

q1_form_m1 <- y ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level +
  s1_score_diff

q1_form_m2 <- y ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level +
  s1_score_diff + s1_score_sum

q1_set1_baseline_res <- test_data %>%
  transmute(
    truth = y,
    .pred_class = set1_winner_benchmark
  )

q1_set1_benchmark_tbl <- tibble(
  Model = "Q1 Baseline: Set 1 winner",
  Accuracy = accuracy(q1_set1_baseline_res, truth, .pred_class)$.estimate,
  ROC_AUC = NA_real_,
  F1_Score = f_meas(q1_set1_baseline_res, truth, .pred_class, event_level = "second")$.estimate,
  LogLoss = NA_real_
)

set.seed(123)


q1_results <- bind_rows(
  q1_set1_benchmark_tbl,
  
  fit_eval_glm(q1_form_m0, "y", "Q1 GLM M0: prematch", train_data, test_data),
  fit_eval_glm(q1_form_m1, "y", "Q1 GLM M1: + s1_score_diff", train_data, test_data),
  fit_eval_glm(q1_form_m2, "y", "Q1 GLM M2: + s1_score_diff + s1_score_sum", train_data, test_data),
  
  fit_eval_rf(q1_form_m0, "y", "Q1 RF  M0: prematch", train_data, test_data),
  fit_eval_rf(q1_form_m1, "y", "Q1 RF  M1: + s1_score_diff", train_data, test_data),
  fit_eval_rf(q1_form_m2, "y", "Q1 RF  M2: + s1_score_diff + s1_score_sum", train_data, test_data)
)

cat("\n===== Q1: Predict Match Winner =====\n")
print(q1_results)

# =========================================================
# Plot changes in Q1 performance relative to pre-match baseline
# =========================================================

q1_plot_data <- q1_results %>%
  mutate(
    model_type = case_when(
      str_detect(Model, "GLM") ~ "GLM",
      str_detect(Model, "RF")  ~ "RF",
      TRUE ~ NA_character_
    ),
    feature_set = case_when(
      str_detect(Model, "M0") ~ "Pre-match",
      str_detect(Model, "M1") ~ "Pre-match + S1 game diff",
      str_detect(Model, "M2") ~ "Pre-match + S1 game diff + S1 total games",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_type), !is.na(feature_set)) %>%
  pivot_longer(
    cols = c(Accuracy, ROC_AUC, F1_Score, LogLoss),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      Accuracy = "Accuracy",
      ROC_AUC = "ROC-AUC",
      F1_Score = "F1-score",
      LogLoss = "Log-loss reduction"
    )
  ) %>%
  group_by(model_type, metric) %>%
  mutate(
    baseline = value[feature_set == "Pre-match"],
    change = if_else(
      metric == "Log-loss reduction",
      baseline - value,
      value - baseline
    )
  ) %>%
  ungroup() %>%
  filter(feature_set != "Pre-match")

q1_delta_plot <- ggplot(
  q1_plot_data,
  aes(x = model_type, y = change, fill = feature_set)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.65,
    colour = "black",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Pre-match + S1 game diff" = "grey30",
      "Pre-match + S1 game diff + S1 total games" = "grey75"
    )
  ) +
  labs(
    title = "Change in Q1 performance relative to the pre-match baseline",
    x = NULL,
    y = "Change from M0",
    fill = "Added features"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

q1_delta_plot

ggsave(
  "q1_delta.png",
  q1_delta_plot,
  width = 8,
  height = 4.5,
  dpi = 400
)


