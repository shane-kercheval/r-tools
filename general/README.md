# General Helper Functions

## basic_stats.R
- general methods

## correlations.R
- methods to help with correlations

## dates.R
- date helper methods

## model_measurements.R`
- provides a list of functions that help assess the quality of various models
	- e.g. https://en.wikipedia.org/wiki/Sensitivity_and_specificity

## outliers.R
- outlier helper methods

# Clustering

## clustering_methods.R


# logger.R

- setting `logger.output_file` to NULL allows logging to be printed to console. Setting it to file path allows outputing to file.
	- e.g. you can do `logger.set_output(NULL)` and then manually use sink e.g. `sink(file='./results/non_try_analysis_results.txt', append=FALSE)`
