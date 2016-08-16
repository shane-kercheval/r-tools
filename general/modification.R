#######################################################################################################################################
# scales data
#######################################################################################################################################
get_scaled_dataset <- function(data_frame, named_column)
{
	return (as.data.frame(lapply(data_frame[, -grep(named_column, colnames(data_frame))], scale)))
}

#######################################################################################################################################
# returns the numeric and logical columns, as well as the column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_numeric_logical_data <- function(data_frame, named_column)
{
	numeric_columns = sapply(data_frame, is.numeric)
	logical_columns = sapply(data_frame, is.logical)
	cluster_columns = numeric_columns | logical_columns
	cluster_data = data_frame[cluster_columns]
	cluster_data[named_column] = data_frame[named_column]

	return (cluster_data)
}
