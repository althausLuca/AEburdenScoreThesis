# Project Title

This repository contains code for the simulation of adverse event burden scores and the evaluation of statistical models for analyzing such data.
The project is part of a master thesis at the University of Bern, Switzerland.

All the scripts are located in the `R` folder, with subdirectories for:
- [data generation](R/data_generation/README.md)
- model implementationrelative
- evaluation techniques

### data generation
- AE_simulation


### **R Folder**
The `R` folder is central to the project and contains the following subdirectories:

- **`data_generation`**: Contains scripts for generating and simulating data, including:
    - `AE_simulation`
    - `analysis`
    - `trial_size_variation_`

- **`models_and_tests`**: Includes the implementation of statistical models and associated tests:
    - **Models**:
        - ANOVA, Log ANOVA, Quantile Regression
        - Tweedie models
        - Zero-Inflated Models (Gamma, Lognormal, and Normal)
    - **Tests**:
        - Permutation tests
        - Two-part tests
        - Wilcoxon tests

- **`evaluation`**: Scripts for evaluating model performance and fit, such as:
    - AIC computation
    - Distribution fitting
    - Proportion of p-values analysis

- **`experiments`**: Contains results and scripts for various experimental setups, e.g., quantile p-value analysis and Tweedie experiments.

- **`ThesisFiguresCode`**: Contains utilities for plotting figures used in publications or reports.

- **`run_models`**: Scripts to execute models with logs for debugging.

### Other Key Directories

- **`data`**: Contains raw and processed datasets for different experimental conditions, such as longer event durations and sample size variations.
- **`plots` and `plots_`**: Directories for storing figures and visualizations from the analysis.
- **`results` and `results_`**: Outputs from model runs and evaluations.
- **`tables`**: Tabulated results, particularly for sample size variation studies.

## Models

The repository features various statistical and machine learning models to analyze the generated data. The models are categorized as:

1. **Traditional Models**:
    - ANOVA and its variants
    - Tweedie models

2. **Advanced Regression**:
    - Quantile Regression
    - Zero-Inflated Models for Gamma, Lognormal, and Normal distributions

3. **Evaluation Techniques**:
    - Model evaluation with metrics like AIC
    - Distribution fitting and hypothesis testing

## Data Generation

Data generation is handled in the `data_generation` subfolder, using simulations tailored for:
- AE (Adverse Event) modeling
- Sample size and trial size variation
- Scenario-based factor analysis

These simulations ensure diverse datasets that challenge model robustness under varying experimental conditions.

## Setup

### Prerequisites
- **R version**: Ensure you have R installed, along with the necessary packages. Use the `renv` folder to replicate the environment for consistent results.
- **System Requirements**: Scripts are optimized for execution on high-performance clusters (e.g., `ubelix`), but can be adapted for local runs.

### Steps to Run
1. Clone the repository:
   ```bash
   git clone <repository-url>



