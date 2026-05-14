# The Predictive Value of Set 1 Information in Tennis Match Forecasting

This repository contains the R code and processed modelling data used for the Data Science Research Project Part B report.

The project investigates whether Set 1 information adds predictive value beyond pre-match information in best-of-three ATP tennis matches.

## Project Overview

The analysis extends a pre-match tennis prediction baseline by adding Set 1 information as an early in-match signal.

Two prediction tasks are considered:

| Task | Target                        | Main question                                                |
| ---- | ----------------------------- | ------------------------------------------------------------ |
| Q1   | Final match winner            | Does Set 1 information improve match-winner prediction?      |
| Q2   | Whether a third set is needed | Can Set 1 information help predict whether the match reaches a deciding third set? |

## Repository Structure

```text
├── README.md
├── code/
│   ├── 01_build_partB_raw_dataset.R
│   ├── 02_build_partB_ready_dataset.R
│   ├── 03_part_b_eda.R
│   ├── 04_model_Q1.R
│   ├── 05_model_Q2.R
│   └── part_a_pre_match_work/
│       └── part_a_prematch.R
├── modeling_data/
│   ├── df_partB_raw.csv
│   └── df_partB_ready.csv
└── figure/
    ├── q1_delta.png
    ├── q2_roc.png
    ├── set1_diff_vs_set2_result.png
    ├── set1_set2_diff_heatmap.png
    ├── set1_sum_vs_set2_result.png
    └── part_a_figure/
        ├── metric_gains.png
        └── rank_skew.png
```

## Data Source

The raw ATP match data are from Jeff Sackmann's public tennis ATP dataset: [tennis_atp](https://github.com/jeffsackmann/tennis_atp).

The original yearly ATP files are not included in this repository. To rebuild the raw Part B dataset from the original files, please download the ATP match files locally and change the `data_dir` path in:

```
code/01_build_partB_raw_dataset.R
```

The prepared modelling datasets used by the report are included in:

```
modeling_data/df_partB_raw.csv
modeling_data/df_partB_ready.csv
```

## Running Order

Run the scripts in the following order:

```
1. code/01_build_partB_raw_dataset.R
2. code/02_build_partB_ready_dataset.R
3. code/03_part_b_eda.R
4. code/04_model_Q1.R
5. code/05_model_Q2.R
```

If the prepared datasets already exist in `modeling_data/`, the Q1 and Q2 scripts can be run directly after adjusting file paths if needed.

## Script Summary

### `01_build_partB_raw_dataset.R`

This is the first-stage data preparation script.

Main steps:

- load yearly ATP match files from 2000 to 2019;
- filter main ATP best-of-three completed matches;
- remove retirements, walkovers, and defaults;
- randomly assign winner and loser to P1 and P2;
- construct chronological Elo ratings;
- create pre-match difference features;
- split match scorelines into set-level scores;
- align set scores to the P1/P2 coding;
- save the output as `df_partB_raw.csv`.

### `02_build_partB_ready_dataset.R`

This script converts the raw Part B dataset into the final modelling dataset.

Main steps:

- load `df_partB_raw.csv`;
- construct `s1_score_diff`, `s1_score_sum`, and `s2_score_diff`;
- construct `elo_diff`;
- create the Q2 target variable `need_s3`;
- select variables used in the modelling scripts;
- save the output as `df_partB_ready.csv`.

### `03_part_b_eda.R`

This script performs exploratory feature checks for Set 1 information.

Main outputs:

- Set 1 game difference vs Set 2 result;
- Set 1 total games vs Set 2 result;
- Set 1 and Set 2 score-difference heatmap.

The figures are saved in the `figure/` folder.

### `04_model_Q1.R`

This script evaluates Q1: final match-winner prediction.

The target variable is `y`, where:

| Target  | Meaning           |
| ------- | ----------------- |
| `y = 1` | P1 wins the match |
| `y = 0` | P2 wins the match |

Model comparison:

| Model        | Feature set                                           |
| ------------ | ----------------------------------------------------- |
| Q1 Benchmark | Set 1 winner rule                                     |
| Q1 GLM M0    | Pre-match features only                               |
| Q1 GLM M1    | Pre-match + Set 1 game difference                     |
| Q1 GLM M2    | Pre-match + Set 1 game difference + Set 1 total games |
| Q1 RF M0     | Pre-match features only                               |
| Q1 RF M1     | Pre-match + Set 1 game difference                     |
| Q1 RF M2     | Pre-match + Set 1 game difference + Set 1 total games |

Main outputs:

- Q1 test-set model comparison table;
- Set 1 winner benchmark performance;
- performance change plot saved as `figure/q1_delta.png`.

### `05_model_Q2.R`

This script evaluates Q2: third-set prediction.

The target variable is `need_s3`, where:

| Target        | Meaning                                 |
| ------------- | --------------------------------------- |
| `need_s3 = 1` | The match requires a deciding third set |
| `need_s3 = 0` | The match ends in straight sets         |

Model comparison:

| Model      | Feature set                                | Purpose                   |
| ---------- | ------------------------------------------ | ------------------------- |
| GLM M0     | Pre-match features only                    | Baseline model            |
| GLM M1     | Pre-match + Set 1 features                 | Test added value of Set 1 |
| RF M0      | Pre-match features only                    | Flexible baseline         |
| RF M1      | Pre-match + Set 1 features                 | Flexible Set 1 model      |
| RF M1_down | Pre-match + Set 1 features + down-sampling | Handle class imbalance    |

Main steps:

- check the class distribution of `need_s3`;
- run stratified five-fold cross-validation;
- compare models using accuracy, balanced accuracy, and ROC-AUC;
- fit selected random forest models on the full training set;
- evaluate test-set performance using the default cutoff of 0.5;
- select probability cutoffs using Youden's index from cross-validation predictions;
- evaluate test-set performance using the selected cutoffs;
- save the ROC curve as `figure/q2_roc.png`.

## Main Features

Pre-match features:

```
elo_diff
rank_points_diff
age_diff
surface
tourney_level
```

Set 1 features:

```
set1_win
s1_score_diff
s1_score_sum
s1_margin_abs
```

## Train/Test Split

A chronological train/test split is used:

| Split        | Data                |
| ------------ | ------------------- |
| Training set | Matches before 2019 |
| Test set     | Matches from 2019   |

This follows the prediction setting where past matches are used to predict future matches.

## Required R Packages

```
library(tidyverse)
library(lubridate)
library(tidymodels)
library(themis)
library(ranger)
library(skimr)
```

## Reproducibility Notes

The reported Q1 and Q2 results are based on the fixed prepared dataset
`modeling_data/df_partB_ready.csv`.

The random P1/P2 assignment was created during data preparation and saved in this prepared CSV file. The Q1 and Q2 modelling scripts read `df_partB_ready.csv`directly, so the reported model results can be reproduced from this file.

A random seed was added to the first-stage data preparation script after the prepared dataset had already been created and used for the final Q1 and Q2 analysis. Therefore, re-running the full data preparation pipeline from the original ATP files may produce a different random P1/P2 assignment and slightly different results. For exact reproduction of the report results, start from the
provided `df_partB_ready.csv` and run `04_model_Q1.R` and `05_model_Q2.R`.

The folder `code/part_a_pre_match_work/` contains earlier Part A pre-match modelling code for reference only. It is not part of the final Part B modelling pipeline. Re-running it may produce slightly different metrics because the Part B data preparation workflow was later separated and revised.

## Notes

Some file paths may need to be adjusted depending on the local folder structure.

The original ATP yearly data files are not uploaded to this repository. The processed modelling data used for the report are provided in the `modeling_data/` folder.
