# =========================================================
# 1. Package loading
# =========================================================
library(tidyverse)

# =========================================================
# 2. Load prepared Part B dataset
# =========================================================

# please change to your path

df <- read_csv("df_partB_ready.csv", show_col_types = FALSE)
head(df)
# =========================================================
# 3. Basic checks for Set 2 score difference
# =========================================================

# Check whether s2_score_diff exists.
"s2_score_diff" %in% names(df)
  
# Check key Set 1 and Set 2 variables.
df %>%
  select(s1_score_diff, s2_score_diff, set2_win) %>%
  head(10)
  
# Set 2 score difference should contain both positive and negative values.
summary(df$s2_score_diff)

# Check whether set2_win is consistent with the direction of s2_score_diff.
with(df, table(set2_win, s2_score_diff > 0, useNA = "ifany"))
  
# =========================================================
# 4. Prepare labels for Set 2 outcome
# =========================================================

df_plot <- df %>%
  mutate(
    Outcome = if_else(set2_win == 1, "P1 Won Set 2", "P1 Lost Set 2")
  )

# =========================================================
# 5. Plot Set 1 game difference by Set 2 result
# =========================================================

s1_diff_plot <- ggplot(df_plot, aes(x = s1_score_diff, fill = Outcome)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_x_continuous(breaks = -6:6) +
  scale_fill_manual(values = c(
    "P1 Won Set 2"  = "grey35",
    "P1 Lost Set 2" = "grey75"
  )) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title    = "Set-1 Margin vs. Set-2 Result",
       subtitle = "Game difference from −6 to +6",
       x = "Set 1 Games Difference (P1 − P2)",
       y = "Match Count",
       fill = NULL) +
  theme_minimal()

s1_diff_plot

ggsave(
  "s1_margin_set2_result.png",
  s1_diff_plot,
  width = 7.5,
  height = 4.5,
  dpi = 400
)


# =========================================================
# 6. Plot Set 1 total games by Set 2 result
# =========================================================

s1_sum_plot <- ggplot(df_plot, aes(x = Outcome, y = s1_score_sum, fill = Outcome)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2) +
  scale_fill_manual(values = c("P1 Won Set 2"  = "#60A5FA",
                               "P1 Lost Set 2" = "#F87171")) +
  labs(title = "Set-1 Total Games vs. Set-2 Result",
       x = NULL,
       y = "Set 1 Total Games",
       fill = NULL) +
  theme_minimal()

ggsave("set1_sum_vs_set2_result.png", width = 6, height = 4.5, dpi = 400)

s1_sum_plot

ggsave(
  "s1_total_games_set2_result.png",
  s1_sum_plot,
  width = 6,
  height = 4.5,
  dpi = 400
)

# =========================================================
# 7. Frequency heatmap of Set 1 and Set 2 score differences
# =========================================================
s1_s2_heatmap <- ggplot(df, aes(s1_score_diff, s2_score_diff)) +
  stat_bin2d(bins = 13) +                               
  scale_fill_viridis_c(                                
    option = "C",
    begin   = .20, end = .90,
    name    = "Matches"                                 
  ) +
  coord_fixed() +                                       
  labs(
    title = "Frequency Heatmap",
    x = "Set-1 diff (P1 – P2)",
    y = "Set-2 diff (P1 – P2)"
  ) +
  theme_minimal()

s1_s2_heatmap

ggsave(
  "s1_s2_heatmap.png",
  s1_s2_heatmap,
  width = 6,
  height = 5,
  dpi = 400
)

# =========================================================
# 8. Optional exploratory check: reversal rate
# =========================================================

WW <- sum(df$s1_score_diff > 0 & df$s2_score_diff > 0)
LL <- sum(df$s1_score_diff < 0 & df$s2_score_diff < 0)
LW <- sum(df$s1_score_diff < 0 & df$s2_score_diff > 0)
WL <- sum(df$s1_score_diff > 0 & df$s2_score_diff < 0)

# Count same-direction and reversal patterns.
c(WW = WW, LL = LL, LW = LW, WL = WL)

# Reversal rate: proportion of matches where Set 2 reverses Set 1 direction.
reversal_rate <- (LW + WL) / (WW + LL + LW + WL)

reversal_rate