## Data Generation

The code for the simulation of AE burden scores is located in the `R/data_generation/AE_simulation` directory. 
Below is a description of the main files and their purposes:

- **`AE_types.R`**: Defines the default base-case AE types used in the simulations.
- **`config_and_init.R`**: Contains the configuration and initialization settings for simulations, including parameter variations for different scenarios.
- **`trial_data.R`**: Defines the `trial_data` object, which specifies the instructions for a single data-generating mechanism and stores the generated data.
- **`initialize_files.R`**: Creates a file with `trial_data` objects for each data-generating mechanism.
- **`summary.R`**: Produces a summary of the generated data, including key statistics and metrics.

