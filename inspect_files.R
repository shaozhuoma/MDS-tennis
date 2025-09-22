# inspect_files.R
library(tidyverse)
library(skimr)

# define file paths
file_main     <- "data/atp_matches_2024.csv"            # Main 
file_qual     <- "data/atp_matches_qual_chall_2024.csv" # Qual/Challenger
file_futures  <- "data/atp_matches_futures_2024.csv"    # Futures
file_doubles  <- "data/atp_matches_doubles_2020.csv"    # Doubles
file_players  <- "data/atp_players.csv"                 # players
file_rankings <- "data/atp_rankings_current.csv"        # rankings

# inspect function
inspect <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  print(skim(df))
}

# example usage
inspect(file_main)
# inspect(file_qual)
# inspect(file_futures)
# inspect(file_doubles)
# inspect(file_players)
# inspect(file_rankings)
