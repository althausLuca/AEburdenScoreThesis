# AE burden score simulation and evaluation

This repository contains code for the simulation of adverse event burden scores and the evaluation of statistical models.
The project is part of a master thesis at the University of Bern, Switzerland.


## Structure
All the scripts are located in the `R` folder, with subdirectories for:
- [data generation](R/data_generation/)
- [model/test implementation](R/models_and_tests/)
- [model execution](R/run_models/)
- [evaluation](R/evaluation/)

### Other Directories
- **`data`**: Contains simulated AE burden score data.
- **`results`**: Storing the output from model/test runs.
- **`plots`**: Directory for storing figures and visualizations from the evluation.


## Used Methods: 
### Models
- ANOVA
- Log-ANOVA (with shift constant)
- Tweedie Regression
- Zero-Inflated Gamma 
- Zero-Inflated Lognormal
- Quantile Regression

### Tests
- Wilcoxon Rank-Sum Test
- Permutation Test on between-group mean differences
- Two-part T-Test
- Two-part Wilcoxon Rank-Sum Test









