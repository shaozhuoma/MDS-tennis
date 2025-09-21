# Load required package
library(tidyverse)

# Read dataset (change the year if needed)
df <- read_csv("data/tennis_atp/atp_matches_2023.csv")

# Data dimensions (rows, columns)
print(dim(df))

# Column names and data types
glimpse(df)


# First few rows
short_data<- head(df, 5)
short_data

# Count of missing values per column
options(width = 200)
skimr::skim(df)
#missing_summary <- colSums(is.na(df))
#print(missing_summary)


# List all column names
print(names(df))


