
The file models_and_tests.R contains the functions that define a model or test.
Each model and test has an own folder in models/ or tests/ respectively with files:
fit_model.R and get_CDFs.R for the models and run_test.R for the tests, defining the model and test functions using its class.
When sourcing the file models_and_tests.R all fit_model, get_CDFs and run_test functions are loaded in the environment as well.

