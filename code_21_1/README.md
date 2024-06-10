If you want to run the code
Run the setup.R file to install the required packages
The working directory should be set to the folder containing this file
(in Rstudio open project and select StatsMA.Rproj).
The data folder is too large for overleaf so it is not included in the zip file.


Changes to the code -19.11
I stored the data as json before which is nice for visaualization but the loaded data
was quite different from the stored data. 
The data is now stored as .rds and inside a tibble (I had some problems with default dfs) dataframe using a bit a different iteration approach. 
My aim was to load the data and be able to use data$lm$pvalue for example to get all p values for lm.
Small markdown for anova just to check variance and if we mostly would reject when scenarios are equal.
A small markdown for tweedie regression to test the tweedie.profile function. Mostly values close to 1 are preferred.


