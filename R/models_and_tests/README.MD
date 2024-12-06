# Implementation of Models and Tests

This directory contains the implementation of  models and tests used in the analysis. 

The `file models_and_tests.R` contains the definition of the different models and tests.

### Models
Models are located in the `model` folder and have  files:
- **`fit_model.R`**: This file fits the model to a dataframe with columns Score and Group and returns:
    - The model definition
    - Fitted parameters
    - Treatment effect p-value
    - AIC value
- **`get_cdf.R`**: This file contains the cumulative distribution function (CDF) of the model using the fitted parameters.

### Tests
Tests are located in the `test` folder and have the file:
- **`run_test.R`**: Runs the specified test on the data and returns the p-value of the test.
