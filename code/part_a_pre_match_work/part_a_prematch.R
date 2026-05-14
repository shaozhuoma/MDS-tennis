# ---- load package ----
library(tidyverse)
library(lubridate)
library(tidymodels)    # rsample, recipes, parsnip, workflows, yardstick


# ---- load data ----
years <- 2000:2019
data_dir <- "D:/Projects/MDS-tennis/data/tennis_atp"
file_paths <- sprintf("%s/atp_matches_%d.csv", data_dir, years)
existing_files <- file_paths[file.exists(file_paths)]

df_raw <- map_dfr(existing_files, ~ readr::read_csv(.x, show_col_types = FALSE))
# df_raw
df<- df_raw %>%
  select(-w_ace, -w_df, -w_svpt, -w_1stIn, -w_1stWon, -w_2ndWon, -w_SvGms, -w_bpSaved, -w_bpFaced,
         -l_ace, -l_df, -l_svpt, -l_1stIn, -l_1stWon, -l_2ndWon, -l_SvGms, -l_bpSaved, -l_bpFaced)

# print(colnames(df_raw))
# print(colnames(df))
# 
skimr::skim(df)
summary(df)

##======tourney_level========== 
table(df$tourney_level)

# tourney level distribution
ggplot(df, aes(x = tourney_level)) +
  geom_bar() +
  labs(title = "Tourney Level Distribution",
       x = "Tourney Level",
       y = "Count") +
  theme_minimal()


##======best of========== 
# ggplot(df, aes(x = factor(best_of))) +  
#   geom_bar() +
#   labs(title = "Distribution of Best Of",
#        x = "Best of (Sets)",
#        y = "Count") +
#   theme_minimal()
# unique(df$best_of)
# class(df$best_of)

# change best_of to factor
# df$best_of <- factor(df$best_of) # TEMP
# class(df$best_of)

##====== score check ==========
#
# length(unique(df$score))
# 
# score_distribution <- table(df$score)
# 
# score_df <- as.data.frame(score_distribution)
# 
# score_df <- score_df[order(score_df$Freq, decreasing = TRUE), ]
# 
# head(score_df, 30)
# 
# top_scores <- names(sort(score_distribution, decreasing = TRUE))[1:30]
# 
# ggplot(df[df$score %in% top_scores, ], aes(x = score)) +
#   geom_bar() +
#   labs(title = "Top 20 Most Frequent Scores",
#        x = "Score",
#        y = "Count") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))  

##====== tourney_name ==========
# 
# tourney_name_freq <- table(df$tourney_name)
# length(tourney_name_freq)
# head(sort(tourney_name_freq, decreasing = TRUE),15)


df <- df %>%
  filter(
    tourney_level %in% c("G", "M", "A", "F", "D"),
    best_of %in% c(3, 5),
    !str_detect(score, regex("RET|W/O|W\\.O\\.|DEF", TRUE)),
    !str_detect(tourney_name, regex("Davis", TRUE))
    # tourney_level %in% c("G", "M", "A", "F"),    # 
    # best_of == 3,                                # 
    # !str_detect(score, regex("RET|W/O|W\\.O\\.|DEF", TRUE))   
  ) %>%
  arrange(tourney_date, match_num)



df
# 
df <- df %>%
  select(
    tourney_date, match_num,
    surface, tourney_level, tourney_name,
    winner_id, loser_id,
    winner_age, loser_age,
    winner_rank_points, loser_rank_points
  )
View(df)



# ---- 3) p1,p2 ----
df_pp <- df %>%
  mutate(
    # p1 < p2 ordering
    p1_id = if_else(winner_id < loser_id, winner_id, loser_id),
    p2_id = if_else(winner_id < loser_id, loser_id, winner_id),
    # p1 wins = 1，otherwise 0
    y     = as.integer(winner_id == p1_id)
  )
# set.seed(42) 


head(df_pp)
df_pp <- df_pp %>%
  mutate(
    surface = factor(surface, levels = c("Hard", "Clay", "Grass")),
    tourney_level = factor(tourney_level, levels = c("G", "M", "A", "F"))
  )%>%
  mutate(
    surface_Hard  = as.integer(surface == "Hard"),
    surface_Clay  = as.integer(surface == "Clay"),
    surface_Grass = as.integer(surface == "Grass"),
    level_G = as.integer(tourney_level == "G"),
    level_M = as.integer(tourney_level == "M"),
    level_A = as.integer(tourney_level == "A"),
    level_F = as.integer(tourney_level == "F"),
    level_D = as.integer(tourney_level == "D")
  )


# ---- 4) Elo  ----
K      <- 32
SCALE  <- 400
EINIT  <- 1500

# Elo fomula
prob_elo <- function(ra, rb) 1 / (1 + 10^(-(ra - rb) / SCALE))

# set up Elo environment
elo_env <- new.env(parent = emptyenv())

# get Elo points
getE <- function(id) {
  id <- as.character(id)
  if (is.null(elo_env[[id]])) EINIT else elo_env[[id]]
}

# set Elo 
setE <- function(id, v) { elo_env[[as.character(id)]] <- v }
 
df_pp <- df_pp %>% mutate(elo_p1 = NA_real_, elo_p2 = NA_real_)

for (i in seq_len(nrow(df_pp))) {
  a <- df_pp$p1_id[i]
  b <- df_pp$p2_id[i]
  
  ra <- getE(a)  # p1 Elo
  rb <- getE(b)  # p2 Elo
  

  df_pp$elo_p1[i] <- ra
  df_pp$elo_p2[i] <- rb
  
  # update Elo 
  Sa <- df_pp$y[i]  # p1 wins?
  pa <- prob_elo(ra, rb)  
  
  ra_new <- ra + K * (Sa - pa)  # update p1 Elo
  rb_new <- rb + K * ((1 - Sa) - (1 - pa))  # update p2 Elo
  
  setE(a, ra_new)  # save p1 Elo
  setE(b, rb_new)  # save p2 Elo
}


## rank_points
# check rank_points,elo,age statistics

colnames(df_pp)
summary(df_pp$winner_rank_points)
summary(df_pp$loser_rank_points)
summary(df_pp$winner_age)
summary(df_pp$loser_age)
summary(df_pp$elo_p1)
summary(df_pp$elo_p2)



# missing value
sum(is.na(df_pp$elo_p1))
sum(is.na(df_pp$elo_p2))
sum(is.na(df_pp$winner_rank_points))
sum(is.na(df_pp$loser_rank_points))
sum(is.na(df_pp$winner_age))
sum(is.na(df_pp$loser_age))

# elo distribution
ggplot(df_pp) +
  geom_histogram(aes(x = elo_p1, fill = "p1 Elo"), bins = 30, alpha = 0.5, color = "black") +
  geom_histogram(aes(x = elo_p2, fill = "p2 Elo"), bins = 30, alpha = 0.5, color = "black") +
  labs(title = "Elo Points Distribution for p1 and p2", x = "Elo Points", y = "Frequency") +
  scale_fill_manual(values = c("p1 Elo" = "blue", "p2 Elo" = "green")) +
  theme_minimal()

# winner and loser by age
ggplot(df_pp) +
  geom_histogram(aes(x = winner_age, fill = "Winner"), bins = 30, alpha = 0.5, color = "black") +
  geom_histogram(aes(x = loser_age, fill = "Loser"), bins = 30, alpha = 0.5, color = "black") +
  labs(title = "Winner vs Loser Age Distribution", x = "Age", y = "Frequency") +
  scale_fill_manual(values = c("Winner" = "blue", "Loser" = "green")) +
  theme_minimal()


# winner loser by Rank
ggplot(df_pp) +
  geom_histogram(aes(x = winner_rank_points, fill = "Winner"), bins = 30, alpha = 0.5, color = "black") +
  geom_histogram(aes(x = loser_rank_points, fill = "Loser"), bins = 30, alpha = 0.5, color = "black") +
  labs(title = "Winner vs Loser Rank Points Distribution", x = "Rank Points", y = "Frequency") +
  scale_fill_manual(values = c("Winner" = "blue", "Loser" = "green")) +
  theme_minimal()

library(tidyverse)

# ranking points distribution
df_points <- df_pp %>%
  select(winner_rank_points, loser_rank_points) %>%
  pivot_longer(cols = everything(),
               names_to = "role",
               values_to = "points")

p_rank_skew <- ggplot(df_points, aes(x = points)) +
  geom_histogram(bins = 60, color = "black", fill = "grey70") +
  labs(
    title = "Distribution of ATP ranking points (raw scale)",
    x = "Ranking points",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 10) +              
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 8)
  )
p_rank_skew

# 
# ggsave(
#   filename = "rank_skew.png",
#   plot     = p_rank_skew,
#   width    = 3.4,   
#   height   = 2.2,   
#   units    = "in",
#   dpi      = 400
# )


# ---- 5) margin variables ----
df_pp <- df_pp %>%
  mutate(
    # elo diff
    elo_diff = elo_p1 - elo_p2,

    # rank_diff
    p1_points = if_else(p1_id == winner_id, winner_rank_points, loser_rank_points),
    p2_points = if_else(p2_id == winner_id, winner_rank_points, loser_rank_points),
    rank_points_diff = log1p(p1_points) - log1p(p2_points),

    # age_diff
    p1_age = if_else(p1_id == winner_id, winner_age, loser_age),
    p2_age = if_else(p2_id == winner_id, winner_age, loser_age),
    age_diff = p1_age - p2_age
  )



# elo diff distribution
ggplot(df_pp, aes(x = elo_diff)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Elo Difference Distribution", x = "Elo Difference", y = "Frequency")


## missing value check
sum(is.na(df_pp$rank_points_diff))
sum(is.na(df_pp$elo_diff))
sum(is.na(df_pp$age_diff))

# # key variables
# key_vars <- c(
#   "winner_rank_points", "loser_rank_points",
#   "winner_age", "loser_age",
#   "elo_p1", "elo_p2",
#   "score"
# )
# #==================================
# # 1. missing number
# na_summary <- df_pp %>%
#   summarise(across(all_of(key_vars), ~ sum(is.na(.))))
# 
# na_summary
# print(na_summary)

# record
n_before <- nrow(df_pp)
n_before
#drop na
df_pp_clean <- df_pp %>%
  drop_na(rank_points_diff, age_diff)   

n_after <- nrow(df_pp_clean)
n_removed <- n_before - n_after

n_before; n_after; n_removed
cat("Rows before cleaning:", n_before, "\n")
cat("Rows after cleaning :", n_after, "\n")
cat("Rows removed due to missing values:", n_removed, "\n")

# 3. rate
prop_removed <- n_removed / n_before
cat("Proportion removed:", round(100 * prop_removed, 2), "%\n")



## feature distribution
# 
ggplot(df_pp_clean, aes(x = rank_points_diff)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Rank Points Difference Distribution", x = "Rank Points Difference", y = "Frequency")

ggplot(df_pp_clean, aes(x = elo_diff)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Elo Difference Distribution", x = "Elo Difference", y = "Frequency")
# cor
cor_matrix <- cor(df_pp_clean %>% select(elo_diff, rank_points_diff, age_diff), use = "complete.obs")
print(cor_matrix)


# y distribution
table(df_pp_clean$y)

df_pp_clean <- df_pp_clean %>%
  mutate(y = as.factor(y))

str(df_pp_clean$y)
levels(df_pp_clean$y)

# split data
df_pp_clean <- df_pp_clean %>%
  mutate(tourney_date = as.integer(tourney_date))

train_data <- df_pp_clean %>% filter(tourney_date < 20190101)
test_data  <- df_pp_clean %>% filter(tourney_date >= 20190101)
train_data$surface <- as.factor(train_data$surface)
test_data$surface <- as.factor(test_data$surface)

train_data$tourney_level <- as.factor(train_data$tourney_level)
test_data$tourney_level <- as.factor(test_data$tourney_level)

# change target y into factors
train_data <- train_data %>% mutate(y = as.factor(y))
test_data  <- test_data %>% mutate(y = as.factor(y))
class(train_data$surface)

# model specification
## 1. logistic：y ~ rank_points_diff
recipe_lr_rank <- recipe(y ~ rank_points_diff, data = train_data) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

model_lr_rank <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

workflow_lr_rank <- workflow() %>%
  add_recipe(recipe_lr_rank) %>%
  add_model(model_lr_rank)
# 
# 2. logistic：y ~ elo_diff
recipe_lr_elo <- recipe(y ~ elo_diff, data = train_data) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

model_lr_elo <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

workflow_lr_elo <- workflow() %>%
  add_recipe(recipe_lr_elo) %>%
  add_model(model_lr_elo)

# 3. logistic：y ~ elo_diff + context
recipe_lr_elo_context <- recipe(y ~ elo_diff + age_diff + surface + tourney_level, data = train_data) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

model_lr_elo_context <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

workflow_lr_elo_context <- workflow() %>%
  add_recipe(recipe_lr_elo_context) %>%
  add_model(model_lr_elo_context)

# 4. LR：y ~ rank_points_diff + surface + tournament_level + interactions
recipe_lr_rank_surface <- recipe(y ~ rank_points_diff + surface + tourney_level, data = train_data) %>%
  step_naomit(all_predictors()) %>%  
  step_dummy(surface, tourney_level, one_hot = TRUE) %>% 
  step_interact(~ rank_points_diff:starts_with("surface_") +
                  rank_points_diff:starts_with("tourney_level_")) %>%  
  step_zv(all_predictors()) %>%        
  step_center(all_numeric_predictors()) %>%  
  step_scale(all_numeric_predictors())  

model_lr_rank_surface <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

workflow_lr_rank_surface <- workflow() %>%
  add_recipe(recipe_lr_rank_surface) %>%
  add_model(model_lr_rank_surface)

# 5. Random Forest

recipe_rf <- recipe(y ~ elo_diff + rank_points_diff + age_diff + surface + tourney_level, data = train_data) %>%
  # step_naomit(all_predictors()) %>%  
  # step_dummy(surface, tourney_level, one_hot = TRUE) %>%  
  # step_center(all_numeric()) %>%  
  # step_scale(all_numeric())  
  step_naomit(all_predictors()) %>%
  step_dummy(surface, tourney_level, one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%  # 
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

# model specification
model_rf <- rand_forest(mtry = 2, trees = 500, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# workflow
workflow_rf_model <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(model_rf)


class(train_data$y)


# fit
fit_lr_rank <- fit(workflow_lr_rank, data = train_data)
fit_lr_elo <- fit(workflow_lr_elo, data = train_data)
fit_lr_elo_context <- fit(workflow_lr_elo_context, data = train_data)
fit_lr_rank_surface <- fit(workflow_lr_rank_surface, data = train_data)
fit_rf <- fit(workflow_rf_model, data = train_data)

# test
pred_lr_rank_prob <- predict(fit_lr_rank, new_data = test_data, type = "prob")
pred_lr_elo_prob <- predict(fit_lr_elo, new_data = test_data, type = "prob")
pred_lr_elo_context_prob <- predict(fit_lr_elo_context, new_data = test_data, type = "prob")
pred_lr_rank_surface_prob <- predict(fit_lr_rank_surface, new_data = test_data, type = "prob")
pred_rf_prob <- predict(fit_rf, new_data = test_data, type = "prob")


colnames(pred_lr_rank_prob)

#label
pred_lr_rank_tibble <- as_tibble(pred_lr_rank_prob)
pred_lr_elo_tibble <- as_tibble(pred_lr_elo_prob)
pred_lr_elo_context_tibble <- as_tibble(pred_lr_elo_context_prob)
pred_lr_rank_surface_tibble <- as_tibble(pred_lr_rank_surface_prob)
pred_rf_tibble <- as_tibble(pred_rf_prob)

#  .pred_1 
pred_lr_rank_tibble$.pred_class <- if_else(pred_lr_rank_tibble$.pred_1 > 0.5, "1", "0")
pred_lr_elo_tibble$.pred_class <- if_else(pred_lr_elo_tibble$.pred_1 > 0.5, "1", "0")
pred_lr_elo_context_tibble$.pred_class <- if_else(pred_lr_elo_context_tibble$.pred_1 > 0.5, "1", "0")
pred_lr_rank_surface_tibble$.pred_class <- if_else(pred_lr_rank_surface_tibble$.pred_1 > 0.5, "1", "0")
pred_rf_tibble$.pred_class <- if_else(pred_rf_tibble$.pred_1 > 0.5, "1", "0")


# get predictive class
pred_lr_rank_tibble$.pred_class <- factor(pred_lr_rank_tibble$.pred_class, levels = c("0", "1"))
pred_lr_elo_tibble$.pred_class <- factor(pred_lr_elo_tibble$.pred_class, levels = c("0", "1"))
pred_lr_elo_context_tibble$.pred_class <- factor(pred_lr_elo_context_tibble$.pred_class, levels = c("0", "1"))
pred_lr_rank_surface_tibble$.pred_class <- factor(pred_lr_rank_surface_tibble$.pred_class, levels = c("0", "1"))
pred_rf_tibble$.pred_class <- factor(pred_rf_tibble$.pred_class, levels = c("0", "1"))

# results 
results_lr_rank <- tibble(
  actual = test_data$y,
  predicted = pred_lr_rank_tibble$.pred_class,
  prob_1 = pred_lr_rank_prob$.pred_1
)

results_lr_elo <- tibble(
  actual = test_data$y,
  predicted = pred_lr_elo_tibble$.pred_class,
  prob_1 = pred_lr_elo_prob$.pred_1
)

results_lr_elo_context <- tibble(
  actual = test_data$y,
  predicted = pred_lr_elo_context_tibble$.pred_class,
  prob_1 = pred_lr_elo_context_prob$.pred_1
)

results_lr_rank_surface <- tibble(
  actual = test_data$y,
  predicted = pred_lr_rank_surface_tibble$.pred_class,
  prob_1 = pred_lr_rank_surface_prob$.pred_1
)

results_rf <- tibble(
  actual = test_data$y,
  predicted = pred_rf_tibble$.pred_class,
  prob_1 = pred_rf_prob$.pred_1
)


metrics_list <- list(
  lr_rank = list(
    accuracy = accuracy(results_lr_rank, truth = actual, estimate = predicted),
    roc_auc  = roc_auc(results_lr_rank, truth = actual, prob_1, event_level = "second"),
    f_meas   = f_meas(results_lr_rank, truth = actual, estimate = predicted, event_level = "second"),
    log_loss = mn_log_loss(
      results_lr_rank,
      truth = actual,
      prob_1,
      event_level = "second"
    )
  ),
  lr_elo = list(
    accuracy = accuracy(results_lr_elo, truth = actual, estimate = predicted),
    roc_auc  = roc_auc(results_lr_elo, truth = actual, prob_1, event_level = "second"),
    f_meas   = f_meas(results_lr_elo, truth = actual, estimate = predicted, event_level = "second"),
    log_loss = mn_log_loss(
      results_lr_elo,
      truth = actual,
      prob_1,
      event_level = "second"
    )
  ),
  lr_elo_context = list(
    accuracy = accuracy(results_lr_elo_context, truth = actual, estimate = predicted),
    roc_auc  = roc_auc(results_lr_elo_context, truth = actual, prob_1, event_level = "second"),
    f_meas   = f_meas(results_lr_elo_context, truth = actual, estimate = predicted, event_level = "second"),
    log_loss = mn_log_loss(
      results_lr_elo_context,
      truth = actual,
      prob_1,
      event_level = "second"
    )
  ),
  lr_rank_surface = list(
    accuracy = accuracy(results_lr_rank_surface, truth = actual, estimate = predicted),
    roc_auc  = roc_auc(results_lr_rank_surface, truth = actual, prob_1, event_level = "second"),
    f_meas   = f_meas(results_lr_rank_surface, truth = actual, estimate = predicted, event_level = "second"),
    log_loss = mn_log_loss(
      results_lr_rank_surface,
      truth = actual,
      prob_1,
      event_level = "second"
    )
  ),
  rf = list(
    accuracy = accuracy(results_rf, truth = actual, estimate = predicted),
    roc_auc  = roc_auc(results_rf, truth = actual, prob_1, event_level = "second"),
    f_meas   = f_meas(results_rf, truth = actual, estimate = predicted, event_level = "second"),
    log_loss = mn_log_loss(
      results_rf,
      truth = actual,
      prob_1,
      event_level = "second"
    )
  )
)



# overall table
metrics_summary <- tibble(
  Model = c("Logistic Regression - Rank",
            "Logistic Regression - Elo",
            "Logistic Regression - Elo Context",
            "Logistic Regression - Rank + Surface",
            "Random Forest"),
  Accuracy = sapply(metrics_list, function(x) x$accuracy$.estimate),
  ROC_AUC  = sapply(metrics_list, function(x) x$roc_auc$.estimate),
  F1_Score = sapply(metrics_list, function(x) x$f_meas$.estimate),
  LogLoss  = sapply(metrics_list, function(x) x$log_loss$.estimate)
)

print(metrics_summary)





# 
# head(unique(test_data$score))
# 
# 
# 
# metrics_summary2 <- metrics_summary %>%
#   mutate(
#     Model_short = c("Rank", "Elo", "Elo+Ctx", "Rank+Int", "RF")
#   )
# 
# # Rank baseline
# baseline <- metrics_summary2 %>%
#   filter(Model_short == "Rank")
# 
# 
# metrics_gain <- metrics_summary2 %>%
#   mutate(
#     Gain_Accuracy = Accuracy - baseline$Accuracy,
#     Gain_AUC = ROC_AUC - baseline$ROC_AUC,
#     Gain_F1 = F1_Score - baseline$F1_Score,
#     Gain_LogLoss = baseline$LogLoss - LogLoss   
#   ) %>%
#   select(Model_short, Gain_Accuracy, Gain_AUC, Gain_F1, Gain_LogLoss) %>%
#   pivot_longer(
#     cols = -Model_short,
#     names_to = "Metric",
#     values_to = "Gain"
#   )
# 
# 
# ggplot(metrics_gain, aes(x = Model_short, y = Gain)) +
#   geom_col(width = 0.6, fill="grey40") +
#   facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     x = "Model",
#     y = "Improvement over Rank-only",
#     title = "Performance improvement over Rank-only baseline"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 30, hjust = 1)
#   )
# 
# 
# 
# 
# 
# 
# # columns: Model, Accuracy, ROC_AUC, F1_Score, LogLoss
# 
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# 
# metrics_summary2 <- metrics_summary %>%
#   mutate(
#     ShortModel = c("Rank", "Elo", "Elo+Ctx", "Rank+Int", "RF")
#   )
# 
# # baseline
# baseline <- metrics_summary2 %>%
#   filter(ShortModel == "Rank")
# 
# # gain
# metrics_gain <- metrics_summary2 %>%
#   mutate(
#     Gain_Accuracy = Accuracy - baseline$Accuracy,
#     Gain_AUC      = ROC_AUC  - baseline$ROC_AUC,
#     Gain_F1       = F1_Score - baseline$F1_Score,
#     Gain_LogLoss  = baseline$LogLoss - LogLoss
#   ) %>%
#   filter(ShortModel != "Rank") %>%
#   select(ShortModel, starts_with("Gain_")) %>%
#   pivot_longer(
#     cols      = starts_with("Gain_"),
#     names_to  = "Metric",
#     values_to = "Gain"
#   ) %>%
#   mutate(
#     Metric = recode(
#       Metric,
#       Gain_Accuracy = "Accuracy",
#       Gain_AUC      = "ROC-AUC",
#       Gain_F1       = "F1-score",
#       Gain_LogLoss  = "Log-loss"
#     ),
#     
#     ShortModel = factor(ShortModel,
#                         levels = c("Elo", "Elo+Ctx", "Rank+Int", "RF"))
#   )
# 
# 
# p_gain <- ggplot(metrics_gain, aes(x = ShortModel, y = Gain)) +
#   geom_col(width = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
#   facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
#   labs(
#     title = "Performance improvement over Rank-only baseline",
#     x = "Model",
#     y = "Improvement over Rank-only"
#   ) +
#   theme_minimal(base_size = 10) +
#   theme(
#     plot.title  = element_text(hjust = 0.5, face = "bold"),
#     strip.text  = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 30, hjust = 1)
#   )
# 
# # plot
# ggsave(
#   filename = "metric_gains.png",
#   plot     = p_gain,
#   width    = 6,
#   height   = 4,
#   units    = "in",
#   dpi      = 400
# )
# 
# getwd()
# list.files()