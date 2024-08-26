# R Projects by Ieva

This repository contains several R scripts created as part of master's degree Statistics course assignments.

- [Postoperative Pain Prediction Analysis](#postoperative-pain-prediction-analysis)
- [Pain Prediction Analysis](#pain-prediction-analysis)

## Postoperative Pain Prediction Analysis

This analysis focuses on predicting postoperative pain experienced by patients undergoing surgical extraction of the third mandibular molar (wisdom tooth). The goal is to identify predictors of postoperative pain and compare the effectiveness of two linear regression models.

### Table of Contents

- [Overview](#overview)
- [Dataset](#dataset)
- [Analysis Workflow](#analysis-workflow)
  - [Data Cleaning and Preparation](#data-cleaning-and-preparation)
  - [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
  - [Linear Regression Models](#linear-regression-models)
  - [Model Diagnostics and Assumptions](#model-diagnostics-and-assumptions)
  - [Model Comparison](#model-comparison)
- [Results](#results)
- [Conclusion](#conclusion)
- [How to Use](#how-to-use)
- [Dependencies](#dependencies)

### Overview

The goal of this analysis is to predict postoperative pain based on various predictors such as age, sex, psychological traits, and cortisol levels. Two linear regression models are built and compared to determine the best predictors of postoperative pain.

### Dataset

The dataset used in this analysis is accessed from [this CSV file](https://tinyurl.com/ha-dataset1). It includes variables such as age, sex, STAI trait, pain catastrophizing, mindfulness, cortisol levels, and pain scores.

### Analysis Workflow

#### Data Cleaning and Preparation

- **Coding Errors:** The dataset is checked for coding errors, such as impossible values in the `pain` and `STAI_trait` columns. Errors are corrected by replacing incorrect values with appropriate ones.
- **Data Type Conversion:** The `pain` and `STAI_trait` columns are converted to integers for proper analysis.

#### Exploratory Data Analysis (EDA)

- **Visualization:** Histograms and scatter plots are used to visualize the distribution and relationships between variables.
- **Correlation Analysis:** A correlation matrix is plotted to explore the relationships between potential predictors.

#### Linear Regression Models

Two linear regression models are developed:

1. **Model 1:** Predicts postoperative pain based on age and sex.
2. **Model 2:** Extends Model 1 by adding STAI trait, pain catastrophizing, mindfulness, and cortisol levels as predictors.

#### Model Diagnostics and Assumptions

For both models, the following diagnostic checks are performed:

- **Outliers:** Influential outliers are identified using leverage plots and Cook's distance.
- **Normality:** Residuals are checked for normality using QQ plots, histograms, and skewness/kurtosis measures.
- **Linearity:** Residual plots are used to check the linearity assumption.
- **Homoscedasticity:** Plots and statistical tests (e.g., Breusch-Pagan test) are used to check for homoscedasticity.
- **Multicollinearity:** Variance Inflation Factor (VIF) is used to assess multicollinearity among predictors.

#### Model Comparison

- **Adjusted R-squared:** Used to compare the variance explained by each model.
- **AIC:** Akaike Information Criterion is used to compare model fit.
- **ANOVA:** Analysis of Variance is used to test the significance of the difference between the models.

### Results

- **Model 1:** Explained a portion of the variance in postoperative pain but did not include important psychological and physiological predictors.
- **Model 2:** Showed a significant improvement in the variance explained by including STAI trait, pain catastrophizing, mindfulness, and cortisol levels.
- **Model 2.1:** Addressed multicollinearity between cortisol serum and saliva by combining them into a single predictor, further improving model fit.

### Conclusion

Adding psychological traits (STAI, pain catastrophizing, mindfulness) and cortisol levels significantly improved the prediction of postoperative pain. The final model (Model 2.1) explained 51.88% of the variance in postoperative pain, providing a better understanding of the factors influencing pain outcomes after surgery.

### How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/repositoryname.git
