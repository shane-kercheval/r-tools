#######################################################################################################################################
# scales data
#######################################################################################################################################
get_scaled_dataset <- function(data_frame, named_column)
{
	return (as.data.frame(lapply(data_frame[, -grep(named_column, colnames(data_frame))], scale)))
}

#######################################################################################################################################
# returns the numeric columns, as well as the (optional) column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_numeric_dataset <- function(data_frame, named_column=NULL)
{
	numeric_columns = sapply(data_frame, is.numeric)
	new_dataset = data_frame[numeric_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

#######################################################################################################################################
# returns the logical columns, as well as the (optional) column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = sapply(data_frame, is.logical)
	new_dataset = data_frame[logical_columns]
	if(!is.null(named_column))
	{
		new_dataset = cbind(data_frame[named_column], new_dataset)
	}

	return (new_dataset)
}

#######################################################################################################################################
# returns the numeric and logical columns, as well as the column `named_column` (most likely a row unique identifier)
#######################################################################################################################################
get_numeric_logical_dataset <- function(data_frame, named_column=NULL)
{
	logical_columns = sapply(data_frame, is.logical)
	new_dataset = get_numeric_dataset(data_frame=data_frame, named_column=named_column)
	new_dataset = cbind(new_dataset, data_frame[logical_columns])
	return (new_dataset)
}
