# =========================================================
# 1. Package loading
# =========================================================

library(tidyverse)
library(skimr)

# =========================================================
# 2. Load raw Part B dataset
# =========================================================
# please change the file path
df_b <- read_csv("df_partB_raw.csv")

# Check basic structure and size.
glimpse(df_b)
dim(df_b)

# =========================================================
# 3. Basic data checks
# =========================================================

# Check variable types, missing values, and numeric summaries.
skim(df_b)

# Check missing values by column.
colSums(is.na(df_b)) |> sort(decreasing = TRUE)

# Sanity check: Set 1 score difference 
summary(df_b$p1_s1 - df_b$p2_s1)

# Check target and set-outcome distributions.
table(df_b$y, useNA = "ifany")
table(df_b$set1_win, useNA = "ifany")
table(df_b$set2_win, useNA = "ifany")
table(df_b$set3_win, useNA = "ifany")


# =========================================================
# 4. Construct Part B features and targets
# =========================================================

# need_s3 logic?
# need_s3 = 1 when the first two sets are split.
# set1_win!=set2_win means that a 3rd set is needed (p1's perspective)
df_b2 <- df_b %>% 
  mutate(
    s1_score_diff = p1_s1 - p2_s1,
    s1_score_sum  = p1_s1 + p2_s1,
    s2_score_diff = p1_s2 - p2_s2,
    elo_diff      = elo_p1 - elo_p2,        # 
    need_s3 = as.integer(set1_win != set2_win) #
  ) %>% 
  select(
    # Match information
    tourney_date, match_num, surface, tourney_level,
    p1_id, p2_id,
    # Targets and set outcomes
    y, set1_win, set2_win, set3_win,need_s3,
    # pre-match features
    elo_p1, elo_p2, elo_diff,             
    rank_points_diff, age_diff,
    # set-level score features
    s1_score_diff, s1_score_sum,s2_score_diff
  )
# Check the prepared feature dataset.
skim(df_b2)

# =========================================================
# complete-case modelling dataset check
# =========================================================
# This version removes rows with missing modelling variables.
# The Q1 and Q2 scripts also perform task-specific drop_na() steps.
df_b3 <- df_b2 %>%
  mutate(
    surface = factor(surface, levels = c("Hard", "Clay", "Grass")),
    tourney_level = factor(tourney_level, levels = c("G", "M", "A", "F"))
  ) %>%
  drop_na(
    y, need_s3,
    surface, tourney_level,
    elo_diff, rank_points_diff, age_diff,
    s1_score_diff, s1_score_sum
  )
skim(df_b3)


# =========================================================
# 6. Save prepared dataset
# =========================================================

# Save the prepared dataset used by the Q1 and Q2 modelling scripts.

out_path <- "df_partB_ready.csv"
write_csv(df_b2, out_path)
cat("Prepared dataset saved to:", out_path, "\n")




