# Project Title

This repository contains code for the simulation of adverse event burden scores and the evaluation of statistical models for analyzing such data.
The project is part of a master thesis at the University of Bern, Switzerland.

All the scripts are located in the `R` folder, with subdirectories for:
- [data generation](R/data_generation/)
- [model/test implementation](R/models_and_tests/)
- [model execution](R/run_models/)
- [evaluation](R/evaluation/)

### Other Directories
- **`data`**: Contains simulated AE burden score data.
- **`results`** **: Storing the output from model/test runs.
- **`plots` **: Directory for storing figures and visualizations from the evluation.

### Setup
The file 'setup.R' loads the necessary packages to run the scripts. 


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




