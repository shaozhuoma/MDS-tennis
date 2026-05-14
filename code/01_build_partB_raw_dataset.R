# =========================================================
# 1. Package loading
# =========================================================

library(tidyverse)
library(lubridate)
library(tidymodels)

# =========================================================
# 2. Load and combine ATP match files
# =========================================================
# Years used in this project.
years <- 2000:2019
# Please change this path to the local folder containing Jeff Sackmann ATP files.
data_dir <- "D:/Projects/part_b/MDS-tennis/original_atp_data"
# Build file paths for yearly ATP match files.
file_paths <- sprintf("%s/atp_matches_%d.csv", data_dir, years)
# Keep only files that exist in the local folder.
existing_files <- file_paths[file.exists(file_paths)]
# Combine all available yearly files into one dataset.
df_raw <- map_dfr(existing_files, ~ readr::read_csv(.x, show_col_types = FALSE))

# =========================================================
# 3. Remove detailed match statistics not used in this project
# =========================================================

# Serve and return statistics are removed because this project focuses on
# pre-match features and set-level score information.
df<- df_raw %>%
  select(-w_ace, -w_df, -w_svpt, -w_1stIn, -w_1stWon, -w_2ndWon, -w_SvGms, -w_bpSaved, -w_bpFaced,
         -l_ace, -l_df, -l_svpt, -l_1stIn, -l_1stWon, -l_2ndWon, -l_SvGms, -l_bpSaved, -l_bpFaced)

# Basic data inspection.
skimr::skim(df)
summary(df)

# Check tournament level distribution.
table(df$tourney_level)

# tourney level distribution
ggplot(df, aes(x = tourney_level)) +
  geom_bar() +
  labs(title = "Tourney Level Distribution",
       x = "Tourney Level",
       y = "Count") +
  theme_minimal()



# =========================================================
# 4. Filter matches for Part B analysis
# =========================================================

# This project keeps completed best-of-three matches from main ATP events.
# Retirements, walkovers, and defaults are removed because they do not represent
# standard completed match outcomes.
df <- df %>%
  filter(
    tourney_level %in% c("G", "M", "A", "F"),
    best_of == 3,
    !str_detect(score, regex("RET|W/O|W\\.O\\.|DEF", TRUE))
  ) %>%
  arrange(tourney_date, match_num)


df
View(df)


# =========================================================
# 5. Random P1/P2 assignment
# =========================================================

set.seed(42) 
# The raw data are recorded as winner and loser.
# To avoid always assigning the winner to the same side, the winner and loser
# are randomly assigned to P1 and P2.
df_pp <- df %>%
  mutate(
    # flip = 1 means the winner is assigned to P1;
    # flip = 0 means the loser is assigned to P1.
    flip = rbinom(n(), 1, 0.5),
    
    
    p1_id = if_else(flip == 1, winner_id, loser_id),
    p2_id = if_else(flip == 1, loser_id, winner_id),
    
    # y = 1 means P1 wins the match.
    y = as.integer(flip == 1)
  ) %>%
  select(-flip)

head(df_pp)

# Convert match-context variables into factors.
df_pp <- df_pp %>%
  mutate(
    surface = factor(surface, levels = c("Hard", "Clay", "Grass")),
    tourney_level = factor(tourney_level, levels = c("G", "M", "A", "F"))
  )


# =========================================================
# 6. Elo rating calculation
# =========================================================
K      <- 32
SCALE  <- 400
EINIT  <- 1500

# Elo expected win probability for player A against player B.
prob_elo <- function(ra, rb) 1 / (1 + 10^(-(ra - rb) / SCALE))

# Store player Elo ratings in an environment.
elo_env <- new.env(parent = emptyenv())

# Get a player's current Elo rating.
# New players start with the initial Elo rating.
getE <- function(id) {
  id <- as.character(id)
  if (is.null(elo_env[[id]])) EINIT else elo_env[[id]]
}

# Update a player's Elo rating in the environment.
setE <- function(id, v) { elo_env[[as.character(id)]] <- v }

# Create columns for pre-match Elo ratings.
df_pp <- df_pp %>% mutate(elo_p1 = NA_real_, elo_p2 = NA_real_)

# Update Elo ratings match by match in chronological order.
for (i in seq_len(nrow(df_pp))) {
  a <- df_pp$p1_id[i]
  b <- df_pp$p2_id[i]
  
  ra <- getE(a)  # p1 Elo
  rb <- getE(b)  # p2 Elo
  
  # Store pre-match Elo ratings.
  df_pp$elo_p1[i] <- ra
  df_pp$elo_p2[i] <- rb
  
  # Match result from P1 perspective.
  Sa <- df_pp$y[i]  
  pa <- prob_elo(ra, rb)   # Expected win probability for P1.
  # Elo updates after the match result is known.
  ra_new <- ra + K * (Sa - pa)  # update p1 Elo
  rb_new <- rb + K * ((1 - Sa) - (1 - pa))  # update p2 Elo
  
  setE(a, ra_new)  
  setE(b, rb_new)  
}



df_pp <- df_pp %>%
  mutate(
    # Elo difference from P1 perspective.
    elo_diff = elo_p1 - elo_p2,
    
    # Ranking points aligned to the P1/P2 coding.
    p1_pts = if_else(y == 1, winner_rank_points, loser_rank_points),
    p2_pts = if_else(y == 1, loser_rank_points, winner_rank_points),
    rank_points_diff = log1p(p1_pts) - log1p(p2_pts),
    
    # Age difference aligned to the P1/P2 coding.
    p1_age = if_else(y == 1, winner_age, loser_age),
    p2_age = if_else(y == 1, loser_age, winner_age),
    age_diff = p1_age - p2_age
  )

# Check missing values in key pre-match features.
sum(is.na(df_pp$rank_points_diff))
sum(is.na(df_pp$elo_diff))
sum(is.na(df_pp$age_diff))




# Remove rows with missing ranking-point or age features.
n_before <- nrow(df_pp)

df_pp_clean <- df_pp %>%
  drop_na(rank_points_diff, age_diff)   

n_after <- nrow(df_pp_clean)
n_removed <- n_before - n_after

n_before; n_after; n_removed

cat("Rows before cleaning:", n_before, "\n")
cat("Rows after cleaning :", n_after, "\n")
cat("Rows removed due to missing values:", n_removed, "\n")

prop_removed <- n_removed / n_before
cat("Proportion removed:", round(100 * prop_removed, 2), "%\n")


# Convert tourney_date to integer for chronological splitting later.
df_pp_clean <- df_pp_clean %>%
  mutate(tourney_date = as.integer(tourney_date))



# =========================================================
# 8. Parse set scores and align them to P1/P2
# =========================================================
df_3sets <- df_pp_clean %>%                       
  filter(best_of == 3, !is.na(score)) %>%         
  mutate(score = str_remove_all(score, "\\(.*?\\)") %>% str_trim()) %>%
  # Split the match score into separate set scores.
  separate(score, into = c("s1","s2","s3"), sep = " ", fill = "right") %>% 
  # Extract winner and loser games in each set.
  mutate(
    w_s1 = as.numeric(str_extract(s1, "^[0-9]+")),
    l_s1 = as.numeric(str_extract(s1, "(?<=-)[0-9]+")),
    w_s2 = as.numeric(str_extract(s2, "^[0-9]+")),
    l_s2 = as.numeric(str_extract(s2, "(?<=-)[0-9]+")),
    w_s3 = as.numeric(str_extract(s3, "^[0-9]+")),
    l_s3 = as.numeric(str_extract(s3, "(?<=-)[0-9]+"))
  ) %>%
  # Convert winner/loser set scores to the random P1/P2 perspective.
  mutate(
    p1_s1 = if_else(y == 1, w_s1, l_s1),
    p2_s1 = if_else(y == 1, l_s1, w_s1),
    p1_s2 = if_else(y == 1, w_s2, l_s2),
    p2_s2 = if_else(y == 1, l_s2, w_s2),
    p1_s3 = if_else(y == 1, w_s3, l_s3),
    p2_s3 = if_else(y == 1, l_s3, w_s3),
    set1_win = if_else(p1_s1 > p2_s1, 1, 0, missing = NA),
    set2_win = if_else(p1_s2 > p2_s2, 1, 0, missing = NA),
    set3_win = if_else(p1_s3 > p2_s3, 1, 0, missing = NA)
  )
# =========================================================
# 9. Keep standard completed Set 1 scores
# =========================================================

# Valid Set 1 scores include 6-0, 6-1, 6-2, 6-3, 6-4, 7-5, and 7-6,
# including reversed versions from the P1/P2 perspective.
df_3sets <- df_3sets %>%
  filter(
    (abs(p1_s1 - p2_s1) == 6 & p1_s1 + p2_s1 == 6)  |
      (abs(p1_s1 - p2_s1) == 5 & p1_s1 + p2_s1 == 7)  |
      (abs(p1_s1 - p2_s1) == 4 & p1_s1 + p2_s1 == 8)  |
      (abs(p1_s1 - p2_s1) == 3 & p1_s1 + p2_s1 == 9)  |
      (abs(p1_s1 - p2_s1) == 2 & (p1_s1 + p2_s1) %in% c(10, 12)) |
      (abs(p1_s1 - p2_s1) == 1 & p1_s1 + p2_s1 == 13)
  )

# Sanity check: Set 1 score difference 
summary(df_3sets$p1_s1 - df_3sets$p2_s1)
df_3sets$set1_win


# =========================================================
# 10. Save first-stage Part B dataset
# =========================================================
df_partB <- df_3sets %>%
  select(
    tourney_date, match_num, surface, tourney_level,
    p1_id, p2_id, y,
    elo_p1, elo_p2, rank_points_diff, age_diff,
    p1_s1, p2_s1, p1_s2, p2_s2, p1_s3, p2_s3,
    set1_win, set2_win, set3_win
  )

write_csv(df_partB, "df_partB_raw.csv")
cat("df_partB_raw.csv saved. Rows:", nrow(df_partB), "\n")


