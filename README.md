# MDS-tennis

MDS Project - Tennis
1. Project Setup

Created GitHub repository MDS-tennis

Added .gitignore to exclude:

data/ (raw dataset not uploaded, as per supervisor guidance)

virtual environments, cache files, system files

Cloned dataset from Jeff Sackmann’s ATP Tennis repository
 into local data/tennis_atp/

2. Data Exploration (Python)

Loaded sample dataset atp_matches_2024.csv using pandas

Examined:

Shape (rows × columns)

Column names and data types

Descriptive statistics (.describe())

Missing value counts

Confirmed dataset contains:

Tournament metadata

Player biographical information

Match statistics (aces, double faults, serve points, etc.)

Player rankings

3. Data Exploration (R)

Loaded atp_matches_2023.csv using tidyverse

Used glimpse() and skimr::skim() to explore structure

Summarized:

Data dimensions

Column types

Missing values

Unique counts for categorical variables

4. Progress Summary

Repository set up and connected to GitHub

Data successfully loaded and explored in both Python and R

Preliminary understanding of dataset structure and variable categories established
