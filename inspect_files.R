# inspect_files.R
library(tidyverse)
library(skimr)

# 定义路径变量（改成你自己机器上的实际路径）
file_main     <- "data/atp_matches_2024.csv"            # 主赛
file_qual     <- "data/atp_matches_qual_chall_2024.csv" # 资格赛+挑战赛
file_futures  <- "data/atp_matches_futures_2024.csv"    # Futures
file_doubles  <- "data/atp_matches_doubles_2020.csv"    # 双打
file_players  <- "data/atp_players.csv"                 # 球员
file_rankings <- "data/atp_rankings_current.csv"        # 排名

# 检查函数
inspect <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  print(skim(df))
}

# 用法：一次只跑一个，避免乱
inspect(file_main)
# inspect(file_qual)
# inspect(file_futures)
# inspect(file_doubles)
# inspect(file_players)
# inspect(file_rankings)
