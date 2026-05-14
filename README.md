## `q1_model.R`: Match-winner Prediction

 Q1 evaluates whether Set 1 information improves final match-winner prediction.

The target variable is `y`, where:

| Target  | Meaning           |
| ------- | ----------------- |
| `y = 1` | P1 wins the match |
| `y = 0` | P2 wins the match |

### Main Purpose

Q1 tests whether adding Set 1 score information improves prediction beyond the pre-match baseline.

It also includes a simple Set 1 winner benchmark. This benchmark predicts the final match winner as the player who wins Set 1.

### Feature Sets

| Model        | Feature set                                           | Purpose                                         |
| ------------ | ----------------------------------------------------- | ----------------------------------------------- |
| Q1 Benchmark | Set 1 winner rule                                     | Simple benchmark                                |
| Q1 GLM M0    | Pre-match features only                               | Logistic regression baseline                    |
| Q1 GLM M1    | Pre-match + Set 1 game difference                     | Test added value of Set 1 margin direction      |
| Q1 GLM M2    | Pre-match + Set 1 game difference + Set 1 total games | Test whether Set 1 length adds more information |
| Q1 RF M0     | Pre-match features only                               | Random forest baseline                          |
| Q1 RF M1     | Pre-match + Set 1 game difference                     | Flexible Set 1 model                            |
| Q1 RF M2     | Pre-match + Set 1 game difference + Set 1 total games | Flexible extended Set 1 model                   |

### Main Steps

1. Load `df_partB_ready.csv`.
2. Prepare the Q1 modelling dataset.
3. Use a chronological train/test split:
   - training set: matches before 2019;
   - test set: matches from 2019.
4. Check the class distribution of `y`.
5. Fit logistic regression and random forest models.
6. Compare pre-match-only models with Set 1 extended models.
7. Evaluate the Set 1 winner benchmark.
8. Calculate test-set metrics.
9. Plot changes in model performance relative to the pre-match baseline.
10. Save the figure as `q1_delta.png`.

### Evaluation Metrics

The Q1 models are evaluated using:

```text
Accuracy
ROC-AUC
F1-score
Log loss
```

## `q2_model.R` — Third-set Prediction

### Main purpose

This script evaluates whether Set 1 information helps predict whether a best-of-three match reaches a deciding third set.

### **Target variable**

| Target        | Meaning                                 |
| ------------- | --------------------------------------- |
| `need_s3 = 1` | The match requires a deciding third set |
| `need_s3 = 0` | The match ends in straight sets         |

### **Model comparison**

| Model      | Feature set                                | Purpose                   |
| ---------- | ------------------------------------------ | ------------------------- |
| GLM M0     | Pre-match features only                    | Baseline                  |
| GLM M1     | Pre-match + Set 1 features                 | Test added value of Set 1 |
| RF M0      | Pre-match features only                    | Baseline                  |
| RF M1      | Pre-match + Set 1 features                 | Test added value of Set 1 |
| RF M1_down | Pre-match + Set 1 features + down-sampling | Handle class imbalance    |

### **Main steps**

1. Load `df_partB_ready.csv`.
2. Construct `need_s3` if it is not already included.
3. Check the class distribution of `need_s3`.
4. Split the data :
   - training set: matches before 2019;
   - test set: matches from 2019.
5. Run stratified five-fold cross-validation.
6. Compare GLM and random forest models using accuracy, balanced accuracy, and ROC-AUC.
7. Fit the selected random forest models on the full training set.
8. Evaluate test-set performance using:
   - default cutoff = 0.5;
   - CV-selected cutoff based on Youden's index.
9. Save the ROC curve as `q2_roc.png`.

### **Main outputs**

- Cross-validation summary table;
- test-set metric tables;
- confusion matrices;
- CV-selected thresholds;
- ROC curve for RF M1 and RF M1_down.
