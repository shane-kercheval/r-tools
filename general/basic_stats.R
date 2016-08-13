source('outliers.R', chdir=TRUE)

# note that if some data has subsets (e.g. comparing all users to a subset of users) for the subset data, you only want to pass in the 
# subset (as opposed to relying on NA data). The reason is that, for example, the '% data above/below outlier threshold' needs to know 
# the total lenght of only the subset, in order to accurately calculate
create_percentile_matrix <- function(list_of_datasets, row_names, percentiles=c(0, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975, 1), round_by=1)	
{
	percentile_list = lapply(list_of_datasets, function(x){c(quantile(x, probs=percentiles, na.rm=TRUE), 
																	calculate_outlier_thresholds(x), 
																	sum(x<calculate_outlier_thresholds(x)[[1]], na.rm=TRUE)/length(x),
																	sum(x>calculate_outlier_thresholds(x)[[2]], na.rm=TRUE)/length(x))})
	percentile_matrix = do.call(rbind, percentile_list)
	num_columns = ncol(percentile_matrix)
	colnames(percentile_matrix) = c(colnames(percentile_matrix)[-seq(from=num_columns, to=num_columns-3, by=-1)], 
									'outlier treshold lower', 'outlier threshold upper', '% data below threshold', '% data above threshold')
	rownames(percentile_matrix) = row_names

	percentile_matrix[,1:(num_columns-2)] = round(percentile_matrix[,1:(num_columns-2)], round_by)
	percentile_matrix[,(num_columns-1):num_columns] = round(percentile_matrix[,(num_columns-1):num_columns], 3)

	return (percentile_matrix)
}
