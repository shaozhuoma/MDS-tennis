
# =========================================================
# 1. Package loading
# =========================================================

library(tidyverse)
library(tidymodels)
library(themis)


# =========================================================
# 2. Load prepared Part B dataset
# =========================================================

# set seed 
set.seed(123)

# load prepared data set, please change the file path
df <- read_csv("df_partB_ready.csv", show_col_types = FALSE)

# If need_s3 is not already included, construct it
if (!"need_s3" %in% names(df)) {
  df <- df %>%
    mutate(need_s3 = as.integer(set1_win != set2_win))
}

# =========================================================
# 3. Basic data checks
# =========================================================

cat("\n===== Basic check =====\n")

cat("Rows:", nrow(df), "\n")
cat("Cols:", ncol(df), "\n")

# need_s3 overall
print(table(df$need_s3, useNA = "ifany"))
print(prop.table(table(df$need_s3, useNA = "ifany")))

# set1_win x set2_win
print(table(df$set1_win, df$set2_win, useNA = "ifany"))

# s1_score_diff summary
print(summary(df$s1_score_diff))

# Check the train/test class distribution before modelling.
df_check <- df %>%
  mutate(
    tourney_date = as.integer(tourney_date),
    need_s3 = factor(need_s3, levels = c(0, 1), labels = c("0", "1"))
  )

train_check <- df_check %>% filter(tourney_date < 20190101)
test_check  <- df_check %>% filter(tourney_date >= 20190101)


cat("\n===== Train/Test distribution =====\n")
cat("Train rows:", nrow(train_check), "\n")
cat("Test rows :", nrow(test_check), "\n")

# Train need_s3
print(table(train_check$need_s3, useNA = "ifany"))
print(prop.table(table(train_check$need_s3, useNA = "ifany")))

# Test need_s3
print(table(test_check$need_s3, useNA = "ifany"))
print(prop.table(table(test_check$need_s3, useNA = "ifany")))

# Majority-class baseline accuracy:
# this is the accuracy obtained by always predicting the larger class.
train_tab <- table(train_check$need_s3)
test_tab  <- table(test_check$need_s3)

train_baseline <- max(train_tab) / sum(train_tab)
test_baseline  <- max(test_tab) / sum(test_tab)

# Train baseline accuracy
round(train_baseline, 4)

# Test baseline accuracy
round(test_baseline, 4)

# plot: check of class proportions in the training and test sets.
bind_rows(
  train_check %>% mutate(split = "Train"),
  test_check  %>% mutate(split = "Test")
) %>%
  ggplot(aes(x = split, fill = need_s3)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of need_s3 in Train and Test",
    x = NULL,
    y = "Proportion"
  ) +
  theme_minimal()

# =========================================================
# 4. Construct Q2 modelling dataset
# =========================================================

# Select variables used for Q2 modelling.
# All difference variables are measured from the P1 perspective.

df_model <- df %>%
  transmute(
    tourney_date  = as.integer(tourney_date),
    
    need_s3       = factor(need_s3, levels = c(0, 1), labels = c("0", "1")),
    
    surface       = factor(surface, levels = c("Hard", "Clay", "Grass")),
    tourney_level = factor(tourney_level, levels = c("G", "M", "A", "F")),
    
    elo_diff,
    rank_points_diff,
    age_diff,
    
    set1_win      = factor(set1_win, levels = c(0, 1), labels = c("0", "1")),
    s1_score_diff,
    s1_margin_abs = abs(s1_score_diff),
    s1_score_sum
  ) %>%
  drop_na()


# Quick check 
head(df_model)
glimpse(df_model)
summary(df_model$s1_score_diff)
table(df_model$need_s3, useNA = "ifany")


# train/test split:
# matches before 2019 are used for training;
# 2019 matches are held out for final testing.
train_data <- df_model %>% filter(tourney_date < 20190101)
test_data  <- df_model %>% filter(tourney_date >= 20190101)


cat("\n===== Modelling dataset size =====\n")
cat("Full modelling rows:", nrow(df_model), "\n")
cat("Train rows:", nrow(train_data), "\n")
cat("Test rows :", nrow(test_data), "\n")

cat("\n===== Q2 target need_s3 distribution =====\n")

cat("\nFull modelling dataset:\n")
print(table(df_model$need_s3, useNA = "ifany"))
print(round(prop.table(table(df_model$need_s3, useNA = "ifany")), 4))

cat("\nTrain set:\n")
print(table(train_data$need_s3, useNA = "ifany"))
print(round(prop.table(table(train_data$need_s3, useNA = "ifany")), 4))

cat("\nTest set:\n")
print(table(test_data$need_s3, useNA = "ifany"))
print(round(prop.table(table(test_data$need_s3, useNA = "ifany")), 4))



train_tab <- table(train_data$need_s3)
test_tab  <- table(test_data$need_s3)
cat("\n===== Majority-class baseline accuracy =====\n")
cat("Train baseline accuracy:",
    round(max(train_tab) / sum(train_tab), 4), "\n")
cat("Test baseline accuracy :",
    round(max(test_tab) / sum(test_tab), 4), "\n")





# =========================================================
# 5. Cross-validation setup
# =========================================================

set.seed(123) #seed

# Use stratified 5-fold cross-validation 
# so that each fold keeps a similar need_s3 class distribution.

cv_folds <- vfold_cv(
  train_data,
  v = 5,
  strata = need_s3
)

cv_folds 



# =========================================================
# 6. Recipes
# =========================================================


# M0: pre-match baseline model.
rec_m0 <- recipe(
  need_s3 ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level,
  data = train_data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# M1: pre-match features + Set 1 information
rec_m1 <- recipe(
  need_s3 ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level +
    set1_win + s1_score_diff + s1_margin_abs + s1_score_sum,
  data = train_data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# M1_down: same predictors as M1, with down-sampling 
rec_m1_down <- recipe(
  need_s3 ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level +
    set1_win + s1_score_diff + s1_margin_abs + s1_score_sum,
  data = train_data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_downsample(need_s3)


# =========================================================
# 7. Model specifications
# =========================================================

# Logistic regression model.
glm_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Random forest model with fixed hyperparameters.
rf_spec_fixed <- rand_forest(
  mtry = 3,
  min_n = 10,
  trees = 300
) %>%
  set_engine("ranger",seed=123) %>%
  set_mode("classification")

# =========================================================
# 8. Workflows
# =========================================================

# GLM M0: logistic regression with pre-match features
wf_glm_m0 <- workflow() %>%
  add_recipe(rec_m0) %>%
  add_model(glm_spec)

# GLM M1: logistic regression with pre-match and Set 1 features
wf_glm_m1 <- workflow() %>%
  add_recipe(rec_m1) %>%
  add_model(glm_spec)

# RF M0: random forest with pre-match features
wf_rf_m0_fixed <- workflow() %>%
  add_recipe(rec_m0) %>%
  add_model(rf_spec_fixed)

# RF M1: random forest with pre-match and Set 1 features
wf_rf_m1_fixed <- workflow() %>%
  add_recipe(rec_m1) %>%
  add_model(rf_spec_fixed)

# RF M1_down: random forest with Set 1 features and down-sampling
wf_rf_m1_down_fixed <- workflow() %>%
  add_recipe(rec_m1_down) %>%
  add_model(rf_spec_fixed)

# =========================================================
# 9. Cross-validation model comparison
# =========================================================

# CV comparison uses accuracy, balanced accuracy, and ROC-AUC.

# metric_set
metric_set_q2 <- metric_set(roc_auc, accuracy, bal_accuracy)

# GLM
# Fit GLM M0: pre-match baseline.
glm_m0_rs <- fit_resamples(
  wf_glm_m0,
  resamples = cv_folds,
  metrics = metric_set_q2,
  control = control_resamples(save_pred = TRUE)
)
# Fit GLM M1: pre-match + Set 1 features.
glm_m1_rs <- fit_resamples(
  wf_glm_m1,
  resamples = cv_folds,
  metrics = metric_set_q2,
  control = control_resamples(save_pred = TRUE)
)

# Print cross-validation results.
collect_metrics(glm_m0_rs)
collect_metrics(glm_m1_rs)


# random forest
# RF M0: pre-match baseline.
rf_m0_rs <- fit_resamples(
  wf_rf_m0_fixed,
  resamples = cv_folds,
  metrics = metric_set_q2,
  control = control_resamples(save_pred = TRUE)
)
# RF M1: pre-match + Set 1 features.
rf_m1_rs <- fit_resamples(
  wf_rf_m1_fixed,
  resamples = cv_folds,
  metrics = metric_set_q2,
  control = control_resamples(save_pred = TRUE)
  
)
# RF M1_down: pre-match + Set 1 features with down-sampling.
rf_m1_down_rs <- fit_resamples(
  wf_rf_m1_down_fixed,
  resamples = cv_folds,
  metrics = metric_set_q2,
  control = control_resamples(save_pred = TRUE)
)

# Print cross-validation results for the random forest models.
collect_metrics(rf_m0_rs)
collect_metrics(rf_m1_rs)
collect_metrics(rf_m1_down_rs)

# summary table used in the report.
cv_tbl <- bind_rows(
  collect_metrics(glm_m0_rs) %>% mutate(Model = "GLM M0", Feature = "Pre-match"),
  collect_metrics(glm_m1_rs) %>% mutate(Model = "GLM M1", Feature = "Pre-match + Set 1"),
  collect_metrics(rf_m0_rs)  %>% mutate(Model = "RF M0",  Feature = "Pre-match"),
  collect_metrics(rf_m1_rs)  %>% mutate(Model = "RF M1",  Feature = "Pre-match + Set 1"),
  collect_metrics(rf_m1_down_rs) %>% mutate(Model = "RF M1_down", Feature = "Pre-match + Set 1 + down-sampling")
) %>% 
  select(Model, Feature, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  relocate(Model, Feature, accuracy, bal_accuracy, roc_auc) %>%
  mutate(across(where(is.numeric), round, 3))

print(cv_tbl)




# =========================================================
# 10.Final model training
# =========================================================
set.seed(123)

# final fit on full training set
final_rf_m1    <- fit(wf_rf_m1_fixed,    data = train_data)
final_rf_m1_down <- fit(wf_rf_m1_down_fixed, data = train_data)

# =========================================================
# 11. Test-set evaluation with default cutoff
# =========================================================

# Evaluate a fitted model using the default cutoff of 0.5.
evaluate_test_default <- function(final_fit, test_data, model_name) {
  #   Inputs:
  #     final_fit  : a fitted workflow object
  #     test_data  : the held-out 2019 test dataset
  #     model_name : model label used in the output table
  #   Output:
  #     A list containing the prediction results, 
  #     metric table, and confusion matrix.  
  test_res <- bind_cols(
    test_data %>% select(need_s3),
    predict(final_fit, new_data = test_data, type = "prob"),
    predict(final_fit, new_data = test_data, type = "class")
  ) %>%
    rename(truth = need_s3)
  
  metric_tbl <- tibble(
    Model = model_name,
    Threshold = 0.5,
    Accuracy = accuracy(test_res, truth, .pred_class)$.estimate,
    Balanced_Accuracy = bal_accuracy(test_res, truth, .pred_class)$.estimate,
    Recall_1 = sens(test_res, truth, .pred_class, event_level = "second")$.estimate,
    Precision_1 = ppv(test_res, truth, .pred_class, event_level = "second")$.estimate,
    Specificity = spec(test_res, truth, .pred_class, event_level = "second")$.estimate,
    F1_Score = f_meas(test_res, truth, .pred_class, event_level = "second")$.estimate,
    ROC_AUC = roc_auc(test_res, truth, .pred_1, event_level = "second")$.estimate
  ) %>%
    mutate(across(where(is.numeric), round, 3))
  
  cm <- conf_mat(test_res, truth = truth, estimate = .pred_class)
  
  list(
    results = test_res,
    metrics = metric_tbl,
    confusion_matrix = cm
  )
}

# Evaluate RF M1 with default cutoff.
eval_m1_default <- evaluate_test_default(
  final_fit = final_rf_m1,
  test_data = test_data,
  model_name = "RF_M1 default"
)
# Evaluate RF M1_down with default cutoff.
eval_m1_down_default <- evaluate_test_default(
  final_fit = final_rf_m1_down,
  test_data = test_data,
  model_name = "RF_M1_down default"
)

# Combine default-cutoff test results
default_test_tbl <- bind_rows(
  eval_m1_default$metrics,
  eval_m1_down_default$metrics
)

cat("\n===== Test-set results with default threshold 0.5 =====\n")
print(default_test_tbl)

cat("\n===== Confusion matrix: RF_M1 default =====\n")
eval_m1_default$confusion_matrix

cat("\n===== Confusion matrix: RF_M1_down default =====\n")
eval_m1_down_default$confusion_matrix

# =========================================================
# 12. Threshold selection from CV predictions
# =========================================================

# Select the cutoff that maximises Youden's index using CV predictions.
get_cv_best_thr <- function(resample_obj) {
  # Input:
  #   resample_obj : fitted resampling object from fit_resamples()
  # Output:
  #   One numeric probability threshold
  cv_pred <- collect_predictions(resample_obj)
  
  roc_df <- roc_curve(
    cv_pred,
    truth = need_s3,
    .pred_1,
    event_level = "second"
  )
  
  best_thr <- roc_df %>%
    filter(is.finite(.threshold)) %>%
    mutate(youden = sensitivity + specificity - 1) %>%
    slice_max(youden, n = 1, with_ties = FALSE) %>%
    pull(.threshold)
  
  return(best_thr)
}

# Select thresholds for RF M1 and RF M1_down.
thr_m1_cv    <- get_cv_best_thr(rf_m1_rs)
thr_m1_down_cv <- get_cv_best_thr(rf_m1_down_rs)

cat("\nCV-selected threshold for RF_M1    :", round(thr_m1_cv, 3), "\n")
cat("CV-selected threshold for RF_M1_down :", round(thr_m1_down_cv, 3), "\n")

# =========================================================
# 13. Test-set evaluation using CV-selected thresholds
# =========================================================

# Evaluate a fitted model (selected probability cutoff)
evaluate_test_with_thr <- function(final_fit, test_data, threshold, model_name) {
  # Inputs:
  #   final_fit  : fitted workflow object
  #   test_data  : held-out 2019 test dataset
  #   threshold  : CV-selected probability cutoff
  #   model_name : model label used in output table
  # Output:
  #   A list containing predictions, metrics, and confusion matrix
  test_res <- bind_cols(
    test_data %>% select(need_s3),
    predict(final_fit, new_data = test_data, type = "prob")
  ) %>%
    rename(truth = need_s3) %>%
    mutate(
      pred_thr = factor(
        if_else(.pred_1 >= threshold, "1", "0"),
        levels = c("0", "1")
      )
    )
  
  metric_tbl <- tibble(
    Model = model_name,
    Threshold = threshold,
    Accuracy = accuracy(test_res, truth, pred_thr)$.estimate,
    Balanced_Accuracy = bal_accuracy(test_res, truth, pred_thr)$.estimate,
    Recall_1 = sens(test_res, truth, pred_thr, event_level = "second")$.estimate,
    Precision_1 = ppv(test_res, truth, pred_thr, event_level = "second")$.estimate,
    Specificity = spec(test_res, truth, pred_thr, event_level = "second")$.estimate,
    F1_Score = f_meas(test_res, truth, pred_thr, event_level = "second")$.estimate,
    ROC_AUC = roc_auc(test_res, truth, .pred_1, event_level = "second")$.estimate
  ) %>%
    mutate(across(where(is.numeric), round, 3))
  
  cm <- conf_mat(test_res, truth = truth, estimate = pred_thr)
  
  list(
    results = test_res,
    metrics = metric_tbl,
    confusion_matrix = cm
  )
}

# Evaluate RF M1 with CV-selected cutoff.
eval_m1_cv_thr <- evaluate_test_with_thr(
  final_fit = final_rf_m1,
  test_data = test_data,
  threshold = thr_m1_cv,
  model_name = "RF_M1 CV-threshold"
)
# Evaluate RF M1_down with CV-selected cutoff.
eval_m1_down_cv_thr <- evaluate_test_with_thr(
  final_fit = final_rf_m1_down,
  test_data = test_data,
  threshold = thr_m1_down_cv,
  model_name = "RF_M1_down CV-threshold"
)

# Combine CV-threshold test results.
cv_thr_test_tbl <- bind_rows(
  eval_m1_cv_thr$metrics,
  eval_m1_down_cv_thr$metrics
)

cat("\n===== Test-set results with CV-selected threshold =====\n")
print(cv_thr_test_tbl)

cat("\n===== Confusion matrix: RF_M1 CV-threshold =====\n")
eval_m1_cv_thr$confusion_matrix

cat("\n===== Confusion matrix: RF_M1_down CV-threshold =====\n")
eval_m1_down_cv_thr$confusion_matrix


# =========================================================
# 14. ROC curves on the 2019 test set
# =========================================================

# Compute ROC curves from test-set predicted probabilities.
roc_m1      <- roc_curve(eval_m1_cv_thr$results,      truth = truth, .pred_1, event_level = "second")
roc_m1_down <- roc_curve(eval_m1_down_cv_thr$results, truth = truth, .pred_1, event_level = "second")

# Combine ROC curve data for plotting.
plot_data <- bind_rows(
  roc_m1      %>% mutate(Model = "RF_M1"),
  roc_m1_down %>% mutate(Model = "RF_M1_down")
)


# Plot ROC curves
q2_roc_plot <-ggplot(plot_data, aes(x = 1 - specificity, y = sensitivity, colour = Model)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey70") +
  coord_equal() +
  labs(
    title = "ROC curves on the 2019 test set",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    colour = NULL
  ) +
  theme_minimal()
q2_roc_plot 


# Save ROC plot for the report.
ggsave(
  "q2_roc.png",
  q2_roc_plot,
  width = 6,
  height = 5,
  dpi = 300
)

