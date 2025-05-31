# Collection of Methods for Exploratory Analysis

Comprehensive collection of R scripts for performing Exploratory Data Analysis (EDA). 
It follows a structured approach to data understanding and preparation.
Work in progress to unify and streamline the EDA workflow.

## Key Topics Covered

- **Descriptive Summaries**: 
Generating comprehensive statistics and visualizations for continuous and categorical variables.
- **Outlier Detection**: 
Visualization of outliers using various statistical and robust methods.
- **Missing Data Handling**: 
Analysis of missingness patterns, performing intelligent NA removal, and applying diverse imputation techniques.
- **Feature Engineering**: 
Transformation of continuous features (log, z-score, min-max, softmax) and discretization using various binning strategies.
- **Correlation Analysis**: 
Exploring relationships between variables through heatmaps and scatterplots.
- **Feature Selection**: 
Utilizing filter methods based on variance, MAD, entropy, mutual information, correlation, and Laplacian scores to identify relevant features.
- **Influence Analysis and Dimensionality Reduction**: 
Application of Principal Component Analysis (PCA), Independent Component Analysis (ICA), and Factor Analysis for deeper insights and dimensionality reduction, including specialized methods for mixed data types.

## Utilized R Packages

# R Packages for Data Analysis

Incomplete list of R packages useful for various stages of the exploratory data analysis

## R Packages for Automatic EDA

* [`DataExplorer`](https://cran.r-project.org/web/packages/DataExplorer/index.html) - Automates the most common EDA tasks, generating interactive reports.
* [`SmartEDA`](https://cran.r-project.org/web/packages/SmartEDA/index.html) - Provides a one-liner solution to generate a complete EDA report.
* [`AEDA`](https://cran.r-project.org/web/packages/AEDA/index.html) - Functions for Automatic Exploratory Data Analysis.
* [`dataMaid`](https://cran.r-project.org/web/packages/dataMaid/index.html) - Creates a customizable data report for various data types.
* [`dlookr`](https://cran.r-project.org/web/packages/dlookr/index.html) - Tools for EDA, data wrangling, and data quality.
* [`ExPanDaR`](https://cran.r-project.org/web/packages/ExPanDaR/index.html) - Interactive panel for exploratory data analysis.

## R Packages for Summary Statistics

* [`inspectdf`](https://cran.r-project.org/web/packages/inspectdf/index.html) - Functions to make it easy to inspect a data frame.
* [`summarytools`](https://cran.r-project.org/web/packages/summarytools/index.html) - Tools to quickly and neatly summarize data.
* [`dlookr`](https://cran.r-project.org/web/packages/dlookr/index.html) - (Also listed under EDA) Includes functions for generating summary statistics.
* [`skimr`](https://cran.r-project.org/web/packages/skimr/index.html) - A quick and easy way to get a summary of your data.
* [`psych`](https://cran.r-project.org/web/packages/psych/index.html) - Procedures for psychological, psychometric, and personality research.
* [`DataExplorer`](https://cran.r-project.org/web/packages/DataExplorer/index.html) - (Also listed under EDA) Offers summary statistics as part of its EDA capabilities.

## R Packages for Analytical/Visual Outlier Detection

* [`outliers`](https://cran.r-project.org/web/packages/outliers/index.html) - A collection of tests for outliers.
* [`car`](https://cran.r-project.org/web/packages/car/index.html) - Companion to Applied Regression (includes outlier detection functions).
* [`EnvStats`](https://cran.r-project.org/web/packages/EnvStats/index.html) - Functions for environmental statistics (includes outlier tests).
* [`DescTools`](https://cran.r-project.org/web/packages/DescTools/index.html) - Tools for descriptive statistics.
* [`performance`](https://cran.r-project.org/web/packages/performance/index.html) - Utilities for computing, comparing and evaluating model performance.
* [`dlookr`](https://cran.r-project.org/web/packages/dlookr/index.html) - (Also listed under EDA) Provides functions for outlier detection.
* [`robustbase`](https://cran.r-project.org/web/packages/robustbase/index.html) - Basic Robust Statistics.
* [`mvoutlier`](https://cran.r-project.org/web/packages/mvoutlier/index.html) - Robust methods for outlier detection in multivariate data.
* [`MVN`](https://cran.r-project.org/web/packages/MVN/index.html) - Provides functions and plots for assessing multivariate normality, including outlier detection.

## R Packages for Visualizing Missingness

* [`naniar`](https://cran.r-project.org/web/packages/naniar/index.html) - Provides tidy data principles for missing data.
* [`visdat`](https://cran.r-project.org/web/packages/visdat/index.html) - Quickly visualize your entire data frame.
* [`VIM`](https://cran.r-project.org/web/packages/VIM/index.html) - Visualization and Imputation of Missing Values.
* [`dlookr`](https://cran.r-project.org/web/packages/dlookr/index.html) - (Also listed under EDA) Offers tools for visualizing missing data.
* [`DataExplorer`](https://cran.r-project.org/web/packages/DataExplorer/index.html) - (Also listed under EDA) Includes visualizations for missing data.

## R Packages for Data Imputation

* [`mice`](https://cran.r-project.org/web/packages/mice/index.html) - Multivariate Imputation by Chained Equations.
* [`VIM`](https://cran.r-project.org/web/packages/VIM/index.html) - (Also listed under Visualizing Missingness) Includes functions for imputation.
* [`Amelia`](https://cran.r-project.org/web/packages/Amelia/index.html) - A program for multiple imputation of incomplete multivariate data.
* [`missForest`](https://cran.r-project.org/web/packages/missForest/index.html) - Nonparametric Missing Value Imputation using Random Forest.
* [`mi`](https://cran.r-project.org/web/packages/mi/index.html) - Missing Data Imputation and Model Checking.
* [`Hmisc`](https://cran.r-project.org/web/packages/Hmisc/index.html) - Harrell Miscellaneous (contains `impute` function).
* [`miceRanger`](https://cran.r-project.org/web/packages/miceRanger/index.html) - Fast Imputation Using Random Forests.
* [`simputation`](https://cran.r-project.org/web/packages/simputation/index.html) - Simple Imputation.

## R Packages for Correlation Analysis

* [`DataExplorer`](https://cran.r-project.org/web/packages/DataExplorer/index.html) - (Also listed under EDA) Provides correlation plots.
* [`ExPanDaR`](https://cran.r-project.org/web/packages/ExPanDaR/index.html) - (Also listed under EDA) Can visualize correlations.
* [`inspectdf`](https://cran.r-project.org/web/packages/inspectdf/index.html) - (Also listed under Summary Statistics) Offers correlation visualizations.
* [`PerformanceAnalytics`](https://cran.r-project.org/web/packages/PerformanceAnalytics/index.html) - Econometric tools for performance and risk analysis (includes correlation matrix visualization).
* [`GGally`](https://cran.r-project.org/web/packages/GGally/index.html) - Extension to `ggplot2` for creating matrix plots, including correlation plots.

## R Packages for PCA Analysis

* [`DataExplorer`](https://cran.r-project.org/web/packages/DataExplorer/index.html) - (Also listed under EDA) Can perform PCA.
* [`AEDA`](https://cran.r-project.org/web/packages/AEDA/index.html) - (Also listed under EDA) May offer PCA-related functions.
* [`FactoMineR`](https://cran.r-project.org/web/packages/FactoMineR/index.html) - Multivariate Exploratory Data Analysis and Data Mining.
* [`explor`](https://cran.r-project.org/web/packages/explor/index.html) - Interactive plots for multivariate analysis.
* [`pcaMethods`](https://cran.r-project.org/web/packages/pcaMethods/index.html) - A collection of PCA methods.
* [`PCAmixdata`](https://cran.r-project.org/web/packages/PCAmixdata/index.html) - Multivariate analysis of mixed data.
* [`fastICA`](https://cran.r-project.org/web/packages/fastICA/index.html) - FastICA Algorithm for Independent Component Analysis.
* [`mixOmics`](https://cran.r-project.org/web/packages/mixOmics/index.html) - Omics data integration and analysis.
* [`psych`](https://cran.r-project.org/web/packages/psych/index.html) - (Also listed under Summary Statistics) Includes functions for PCA and factor analysis.